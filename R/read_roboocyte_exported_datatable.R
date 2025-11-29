read_ROBOO_exported_datatable <- function(plate_exportfile){  
  
  plate_data<-readr::read_delim(plate_exportfile, 
                                "\t", 
                                col_types = readr::cols(),
                                escape_double = FALSE, 
                                trim_ws = TRUE ,
                                lazy = FALSE,
                                locale = readr::locale(decimal_mark = ",", grouping_mark = ".")) %>% 
    mutate(plate=basename(plate_exportfile) %>% stringr::str_remove("_Export_Datatable.dat")) %>% 
    rename(well=Well) %>%
    relocate(plate, well) %>%  rowwise %>% 
    mutate(ptrs=list(tibble( file          =plate_exportfile  ,  
                             RecordingID   = ID)), 
            
           xoffset=0, yoffset=0) %>% 
    
     
    
    group_by(well) %>%
    mutate(id=paste(basename( plate) , well, (ID-ID[1])+1, sep="-")) %>% 
    relocate(id=id)  %>%
    mutate(file=basename(plate), .keep="unused", .after=id) %>% 
    mutate(swp=factor(ID-ID[1]+1), .after=well) %>%
    relocate(swp.start=`Start Date`, .after=swp) %>%
    mutate(swp.start= lubridate::dmy_hms(swp.start) %>% as.double() , .after=swp.start)
  
  
  
  
  
  
  plate_data %>% ungroup
  
  
}

 
#' Reformat output of read_Roboo
#'
#' @param ephysdata 
#'

#' @export
format_roboExport <- function(ephysdata){
  
  units::install_unit("M", "1 mol/l")
  
  deparse_unit_allow_na<-function(x) if(is.na(x) | !inherits(x, "units")){"~"}else{units::deparse_unit(x)}
  # 
  #ephysdata %>% 
    suppressMessages(ephysdata %>% as_tibble(.name_repair = "universal")) %>%
    #as_tibble(.name_repair = "universal") %>%
    rowwise %>%
    rename_with(~ paste0("CSR.",.), c(
      "ROI" ,
      "Minimum",
      "Pos.Min",
      "Maximum",
      "Pos.Max",
      "Average",
      "Area",
      "Left",
      "Right",
      "BaselineLeft",
      "BaselineRight",
      "BaselineCorrection",
      "Drift.1",
      "Drift.2",
      "DriftCorrection",
      "Baseline.Average",
      "Extremum"
    )) %>%
    rename_with(~ paste0("GILSON.",.), c(
      "Rack",
      "Slot",
      "Tube"
    )) %>%
    #mutate(across(starts_with("conc.."), function(x) if(x=="N/A"){NA}else{x})) %>%
    mutate(across(starts_with("unit."),  function(x) if(is.na(x)){"\u00b5M"}else{x})) %>%
    mutate(CPD.name1=Comp..1)%>%
    mutate(CPD.name2=Comp..2)%>%
    mutate(CPD.name3=Comp..3)%>%
    mutate(CPD.conc1=if_else(is.na(conc..1), units::set_units(NA_real_, "\u00b5M"), units::set_units(conc..1, as.character(unit.1), mode="standard")%>% units::set_units("\u00b5M"))) %>%
    mutate(CPD.conc2=if_else(is.na(conc..2), units::set_units(NA_real_, "\u00b5M"), units::set_units(conc..2, as.character(unit.2), mode="standard")%>% units::set_units("\u00b5M"))) %>%
    mutate(CPD.conc3=if_else(is.na(conc..3), units::set_units(NA_real_, "\u00b5M"), units::set_units(conc..3, as.character(unit.3), mode="standard")%>% units::set_units("\u00b5M"))) %>%
    
    mutate(CPD.label1 =ifelse(CPD.name1=="N/A", "", paste(CPD.name1, CPD.conc1, deparse_unit_allow_na(CPD.conc1)))) %>%
    mutate(CPD.label2 =ifelse(CPD.name1=="N/A", "", paste(CPD.name2, CPD.conc2, deparse_unit_allow_na(CPD.conc2)))) %>%
    mutate(CPD.label3 =ifelse(CPD.name1=="N/A", "", paste(CPD.name3, CPD.conc3, deparse_unit_allow_na(CPD.conc3)))) %>%
    #
    
    rename(CPD.buffer=Buffer) %>%
    rename(CPD.Valve=Valve) %>%  
    select(-starts_with(c("conc.","unit.","Comp.", "conc_"))) %>%
    tidyr::nest(CSR=starts_with("CSR"))%>%
    tidyr::nest(CPD=starts_with("CPD"))%>%
    tidyr::nest(GILSON=starts_with("GILSON")) %>%
      tidyr::nest(IV_info=c(  IV.Prot., IVCurveID, Voltage))%>%
    tidyr::hoist(CPD, "CPD.label1", "CPD.label2") %>% 
    relocate(id, file, well, swp, swp.start, CPD.label1, CPD.label2  ) %>%
    select(-Type, -Tag, -Mode,  -L.On, -Ls,   -Series, -Sample.Rate, -ID) 
  
  
}


#injection
#' get injection info of a Roboocyte plate
#'
#' @param plate plate name, without file ending ".rpf" or ".dat"
#' @param well optional: well to get info for (all wells per default)
#' @param nest optional: swith on/off nesting of the  results into a column named "RNA"
#' @param ... not used
#'
#' @return data frame with injection info
#' @export
get_injection_info <-function(plate, well=NULL, nest=TRUE,  ...){
  rpf_file <- get_file(paste0(plate,".rpf"))
  assertthat::assert_that(file.exists(rpf_file),
                            msg = glue::glue("cannot read RNA-INfo: rpf-file for plate {plate} does not exist"))
  XML::xmlParse(file = rpf_file) ->x
  prop       <- XML::getNodeSet(x, "//SampleInfo/PropInject")      %>% XML::xmlToDataFrame()
  Injections <- XML::getNodeSet(x, "//SampleInfo")      %>% XML::xmlToDataFrame()
  Samples    <- XML::getNodeSet(x, "//Sample")          %>% XML::xmlToDataFrame()
  wells      <- XML::getNodeSet(x, "//WellList/Well")   %>% XML::xmlToDataFrame()
  stopifnot(nrow(prop)==nrow(Injections))
  Injections$InjectionVolume = prop$InjectVolume
  
  dplyr::left_join(Injections, wells, by=c("WellIndex"= "Index")) %>% dplyr::left_join(Samples, by=c("SampleID"="ID")) -> WELLINFO
  
  WELLINFO <-  WELLINFO %>% dplyr::mutate(Plate=plate) %>%
    select( Plate,  Well=Name.x, 
            RNA.InjectionDate    = InjectionDate, 
            RNA.Name             = Name.y, 
            RNA.UserID           = UserID, 
            RNA.Concentration    = Concentration, 
            RNA.InjectionVolume  = InjectionVolume, 
            RNA.Comment          = Comment
            ) %>%
    mutate(RNA.InjectionVolume = RNA.InjectionVolume %>% as.numeric %>% units::set_units("pl") %>% units::set_units("nl"))
  if(!is.null(well)){
    WELLINFO <- WELLINFO %>% filter(Well==well) 
  }
  if(nest) WELLINFO <-  WELLINFO %>% tidyr::nest(RNA=starts_with("RNA")) 
  WELLINFO
}