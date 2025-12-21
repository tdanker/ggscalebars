library(dplyr)
library(stringr)
library(ggplot2)
library(knitr)
library(cowplot)
theme_set(theme_cowplot())
library(purrr)
library(readr)

# cached version to get traces
get_oo_traces_cached<-function(plate,well, verbose=F,...){
  filename=paste(plate,well, sep="_")
  if(file.exists(filename)){
    if(verbose) print("reading from cached file")
    result=readRDS(filename)
  }else{
    if(verbose) print("reading from roboocyte data")
    result= read_traces_(plate, well) 
    if(verbose) print("writing  cache file")
    saveRDS(result, file=filename)
  }
  result
}  

#injection
get_injection_info <-function(plate, well=NULL, ...){
  require("XML")
  require("methods")
  require(dplyr)
  xmlParse(file = paste0(plate,".rpf")) ->x
  prop       <- getNodeSet(x, "//SampleInfo/PropInject")      %>% xmlToDataFrame()
  Injections <- getNodeSet(x, "//SampleInfo")      %>% xmlToDataFrame()
  Samples    <- getNodeSet(x, "//Sample")          %>% xmlToDataFrame()
  wells      <- getNodeSet(x, "//WellList/Well")   %>% xmlToDataFrame()
  stopifnot(nrow(prop)==nrow(Injections))
  Injections$InjectionVolume = prop$InjectVolume
  
  left_join(Injections, wells, by=c("WellIndex"= "Index")) %>% left_join(Samples, by=c("SampleID"="ID")) -> WELLINFO

  WELLINFO <-  WELLINFO %>% mutate(Plate=plate) %>%
    select( Plate,  Well=Name.x, InjectionDate, RNA=Name.y, Concentration, InjectionVolume, Comment) #%>% 
  if(!is.null(well)){
    WELLINFO <- WELLINFO %>% filter(Well==well) 
  }
  WELLINFO
}


get_oo_log<- function(Plate, Well, filter=".*", end="Current clamp at 0 nA|Recording protocol completed|==> Date",...){
  plate_log<-read_lines(paste0(Plate, ".log"))
  
  script_starts <- plate_log %>% str_which(">----- MessageLog start -----<")
  script_ends   <- plate_log %>% str_which(">----- MessageLog end -----<")
  stopifnot(length(script_ends)== length(script_starts))
  
  data.frame(start=script_starts, end_=script_ends) %>% pmap_dfr(function(start, end_){
    plate_log<- plate_log[start:end_]
    Dates <- plate_log %>% str_subset("==> Date") %>% lubridate::ymd_hms()
    stopifnot(length(Dates)==2)
    Dates
    oo_start <- plate_log %>% str_which(paste0("Recording oocyte in well: ", Well))
    
    oo_start %>% map_dfr(function(oo_start){
      oo_log <- plate_log[c(oo_start:length(plate_log))]
      oo_end<-  oo_log %>% str_which(end)
      oo_log <- oo_log[1:oo_end[1]]
      oo_log <- oo_log [str_which(oo_log, pattern=filter)]
      Well <- oo_log[1] %>% str_split( pattern=":") %>% chuck(1,2) %>% str_trim()
      as_tibble( oo_log) %>% transmute(Plate=Plate, Well=Well, run=start, run_start=Dates[1], run_end=Dates[2], logstart=oo_start, logmessages=value)
    })
  }) %>% group_by(Plate, Well, run) %>%
    mutate(test=logstart %>% as.factor %>% as.numeric, 
           run_time=list(c(start=run_start[1], end=run_end[1]))) %>% 
    ungroup %>% 
    mutate(run=run %>% as.factor %>% as.numeric) %>%
    select(-run_start, -run_end, -logstart) %>%
    nest_by(Plate, run_time, run  , Well,   test,  .key="log")
  
  
  
}

xtract_log <- function(logmessages, pattern, n=1) {
  # function that returns NA for an empty result
  fill_NA <- function(x) {
    if (length(x) == 0) {
      NA
    } else{
      x
    }
  }
  
  logmessages[[1]] %>% str_subset(pattern) %>% pluck(n) %>% fill_NA() 
}

get_plate_data <- function(plate, well=NULL){
  require(readr)
  require(ggplot2)
  require(tidyr)
  require(dplyr)
  require(stringr)
  
  
  plate_data<-read_delim(paste0(plate, "_Export_Datatable.dat"), 
                          "\t", 
                          col_types = cols(),
                          escape_double = FALSE, 
                          trim_ws = TRUE ,
                          locale = locale(decimal_mark = ",", grouping_mark = ".")) %>% mutate(Plate=plate) %>% relocate(Plate, Well) 
  
  injection_info <- get_injection_info(plate, well)
 
  plate_data<- plate_data %>% left_join(injection_info, by = c("Plate", "Well"))
  if(!is.null(well)){
    plate_data<- plate_data %>% filter(Well==well) 
  }
  
  
  plate_data %>% as_tibble(.name_repair = "universal") %>% mutate(
    conc_1 = as.numeric(conc..1) * case_when (unit.1 == "nM" ~ .001,
                                              unit.1 == "µM" ~ 1,
                                              unit.1 == "mM" ~ 1000, TRUE~1),
    
    conc_2 = as.numeric(conc..2) * case_when (unit.2 == "nM" ~ .001,
                                              unit.2 == "µM" ~ 1,
                                              unit.2 == "mM" ~ 1000, TRUE~1),
    
    conc_3 = as.numeric(conc..3) * case_when (unit.3 == "nM" ~ .001,
                                              unit.3 == "µM" ~ 1,
                                              unit.3 == "mM" ~ 1000, TRUE~1)
    
  ) %>% rename(recording=ID)  %>% mutate(sweep=1+as.numeric(recording)-as.numeric(recording)[1]) %>% group_by(Plate, Well) 
}
 
 
plate_summary <- function(plate){
  get_plate_data(plate) %>% mutate(Plate=plate) %>% group_by(Plate, Well) %>%  summarise(n=n()) 
}

get_plate_locale <- function(plate){
  if (read_lines(paste0(plate, "_Export_Traces.dat"), n_max = 1, skip = 1) %>% str_detect("\\.")) {
    
    locale(decimal_mark = ".", grouping_mark = ",")
  }else {
    
    locale(decimal_mark = ",", grouping_mark = ".")
  }
}

# Version that takes platename and well name
read_traces_ <- function(plate, well){
  require(readr)
  require(ggplot2)
  require(tidyr)
  require(dplyr)
  require(stringr)
  
 
  datatable <- read_delim(paste0(plate, "_Export_Datatable.dat"),
             "\t",
             col_types = cols(),
             escape_double = FALSE,
             trim_ws = TRUE ,
             locale = get_plate_locale(plate))
  names(datatable) <- vctrs::vec_as_names(names(datatable), repair="universal")
 
  
  if(is.character(well)){
    recordings <- datatable %>% filter(Well==well) %>% .$ID
    datatable %>% filter(Well==well) 
    WELL<<-well
    WELLInfo<<-get_injection_info(plate,well)
  }else{
    stopifnot(is.numeric(well))
    wells<-datatable$Well %>% unique 
    WELL<<-wells[well]
    recordings <- datatable %>% filter(Well==wells[well]) %>% .$ID
  }

  
  traces <- read_delim(paste0(plate, "_Export_Traces.dat"), 
                       "\t", 
                       col_types = cols(),
                       escape_double = FALSE, 
                       trim_ws = TRUE ,
                       locale = get_plate_locale(plate)) 
  
  
  
  cbind(
    
    traces %>% select(starts_with("Time")) %>% 
      pivot_longer(everything()) %>% 
      transmute(
        recording= name %>% str_remove(pattern = "Time\\(ms\\)_RecordingID-") , 
        seconds=value/1000
      ),
    
    traces %>% select(starts_with("J"))%>% pivot_longer(everything()) %>% 
      transmute(
        current=value/1000
      ),
    datatable 
    
  ) %>% 
    filter(recording %in% recordings)
}

show_oocyte<-function(Plate, Well, ...){
  if(Well=="*"){
    plate_summary(Plate) %>% purrr::pmap( show_oocyte ) %>% walk(print)
    
  }else{
    pd<-get_plate_data(Plate,Well)
    CURSORS <- pd %>%
      select(Plate, Well, sweep, Left, Right, BaselineLeft, BaselineRight)  %>% 
      pivot_longer(cols=(-(1:3))) 
    
    read_traces_(Plate, Well) %>%
      mutate(compound=paste(Comp..2, conc..2, unit.2) %>% str_replace("empty 0 mM", "control"), sweep=1+as.numeric(recording)-as.numeric(recording)[1]) %>% 
      
      ggplot(aes(x=seconds, y=current, color=compound)) +
      facet_grid(cols = vars(sweep)) + 
      geom_line() + ylab("current [µA]") +
      theme(
        panel.spacing.x = unit(0.05, "cm"), 
        axis.text.x.bottom = element_text(size = 7)) +
      labs(caption = paste(Plate, WELL, "| RNA:", WELLInfo$RNA)) + 
      geom_vline(aes(xintercept=value/1000), data=CURSORS, col="grey", lty=3)+
      theme(strip.background=element_blank(), strip.text = element_blank(), panel.spacing.x = unit(-.3, "cm")) + 
      scale_color_manual(values=c("skyblue", "darkblue"))
  }
  
}

#takes a tibble with columns "Plate" and "Well":
show_oocytes <- function(x)x%>% pmap(show_oocyte) %>% walk(print)
