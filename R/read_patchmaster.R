#' read HEKA patchmaster files
#'
#' @param file path to the file
#' @param exp optional, numeric or vector: select which experiments to read
#' @param ser optional, numeric or vector: select which series to read
#' @param swp optional, numeric or vector: select which sweeps to read
#' @param trc a string to be detected in the selected tracename, defaults to "Imon-1". If set to NA, all traces are returned
#'
#' @return a tibble with information about the file content, which can be further processed with e.g. ggsweeps
#' @export
#' @family ephys-data-readers 
#' @examples
#'  library("ephysdata")
#'  read_PATCHMASTER(ephysdata::examplefile("herg")) %>% filter(exp==1, ser==1, swp==1, trc=="Imon-1") %>% ggsweeps()
read_PATCHMASTER<-function(file, exp=NA, ser=NA, swp=NA, trc="Imon-1|Imon1|I-mon", cache_rerun=F, 
                           # option to return early to see how much performance the first step conumes (answer: ~50%) 
                           step_out=99, 
                           # we in-effectively filter only in the end. Thus, lets try an 
                           # option to actually read only the specified experiment, to see the effect:  (works as expected)
                           prefilter_exp_ser=F){
  xfun::cache_rds(  # this caching seems to make sense (confirmed by benchmarking)
    dir = "cache_patchmaster/", rerun = cache_rerun, 
    hash = list(file, file.size(file), exp, ser, swp, trc), 
    clean = F,
    file="readPM_",
    
    {
  
  file<-get_file(file)
  ti<-get_treeinfo(file)
  class(ti)<-"list"
  con=file(file, "rb")
  
  if(prefilter_exp_ser && !is.na(exp)) ti=prune_list_deep(ti,target_level=2, indices=exp)
  if(prefilter_exp_ser && !is.na(ser)) ti=prune_list_deep(ti,target_level=3, indices=ser)
  
  TABLE<-
    tibble(file=names(ti), exp=names(ti[[file]]))  %>% 
    rowwise %>% mutate(ser= list( tmp= names(purrr::pluck(ti, file, exp))) )                %>% tidyr::unnest(ser)  %>% 
    rowwise %>% mutate(swp= list( tmp= names(purrr::pluck(ti, file, exp, ser))) )           %>% tidyr::unnest(swp)  %>% 
    rowwise %>% mutate(trc= list( tmp= names(purrr::pluck(ti, file, exp, ser, swp))) )      %>% tidyr::unnest(trc) %>%
    rowwise %>% mutate(
      exp.=(purrr::pluck(ti, file, exp, purrr::attr_getter("dataptr")))  , 
      ser.=(purrr::pluck(ti, file, exp, ser, purrr::attr_getter("dataptr"))),
      swp.=(purrr::pluck(ti, file, exp, ser, swp, purrr::attr_getter("dataptr"))), 
      trc.=(purrr::pluck(ti, file, exp, ser, swp, trc, purrr::attr_getter("dataptr"))), 
      seconds=get_TraceTime(con, swp.)
    )
  
  if(step_out==1) return(TABLE)
  TABLE <- TABLE %>%
    rowwise %>% mutate(
      traceparams=list(get_trcparams(con, trc.)),
      exp_text=get_text(con, exp.),
      ser_text=get_text(con, ser.)
    ) %>% tidyr::unnest_wider(traceparams)
  if(step_out==2) return(TABLE)
  
  TABLE <- TABLE %>% 
  
   
    mutate(exp=iconv(exp,  "latin1", "utf-8")) %>%
    mutate(ser=iconv(ser,  "latin1", "utf-8")) %>%
    mutate(swp=iconv(swp,  "latin1", "utf-8")) %>%
    
      tidyr::separate(exp, into=c("exp", "explabel"), extra="merge") %>%
      tidyr::separate(ser, into=c("ser", "serlabel"), extra="merge") %>%
      tidyr::separate(swp, into=c("swp", "swplabel"), extra="merge") %>% 
    mutate(
      swp=swp %>% stringr::str_remove("s"),
      file_ = basename(file),
      id=paste(file_, exp,ser,swp,trc, sep="-"), 
      exp=factor(as.numeric(exp)), 
      ser=factor(as.numeric(ser)), 
      swp=factor(as.numeric(swp)), 
      ) %>% 
    
    
      # this line generates ptrs as a list of tibbles, which causes Problems 
      tidyr::nest(ptrs=c(file, contains("."))) %>% 
      
            # trying to dix this, but: this line does not do the same as abobe, unsing a list of lists, so this is not working:
            # mutate(ptrs= list(list(file, exp., ser., swp., trc.)))%>% select(-c(file, exp., ser., swp., trc.)) %>% 
            # as a workaround for now, we concert to a list of lists at the and of the function
    
    
      #tidyr::unnest_wider(traceparams) %>%
    rename(file=file_)
  close(con)
  
  if(!identical(NA, exp)) TABLE <- TABLE %>% filter(exp %in% .env$exp)
  if(!identical(NA, ser)) TABLE <- TABLE %>% filter(ser %in% .env$ser)
  if(!identical(NA, swp)) TABLE <- TABLE %>% filter(swp %in% .env$swp)
  if(!identical(NA, trc)) TABLE <- TABLE %>% filter(trc %>% stringr::str_detect(.env$trc))
  TABLE %>% relocate(id, file, exp, ser, swp, trc, contains("label"), swp.start=seconds) %>% 
    mutate(xoffset=0, yoffset=0)-> TABLE
  
  # we do not want to store $ptrs as tibbles, because this causes problems.
  # fix this, you can now do options(ephys4.HEKA_ptrs_as_lists=TRUE) to change to using lists
  
  if(isTRUE(getOption("ephys4.HEKA_ptrs_as_lists"))){
    TABLE$ptrs <- TABLE$ptrs %>% purrr::map(as.list)
  }
  
  
  TABLE %>% add_ptrs_class("ptrs_heka")
  
})}

get_TraceTime<-function(con, swp.){
  readAny(swp., con, 56,"double",8)  
}


get_trcparams<-function(con, trc.){
  # from pulsedfile.txt:
  #    TrSealResistance     = 168; (* LONGREAL *)
  #    TrCSlow              = 176; (* LONGREAL *)
  #    TrGSeries            = 184; (* LONGREAL *)
  #    TrRsValue            = 192; (* LONGREAL *)
  #    TrGLeak              = 200; (* LONGREAL *)
  #    TrMConductance       = 208; (* LONGREAL *)
  #    TrSealResistance     = 168; (* LONGREAL *)
  TrCSlow           <-readAny(trc., con, offset = 176, "double", 8) *1e12 
  TrGSeries         <-readAny(trc., con, offset = 184, "double", 8) *1e9
  TrRsValue         <-readAny(trc., con, offset = 192, "double", 8) *1e-6
  TrSealResistance  <-readAny(trc., con, offset = 168, "double", 8) *1e-6
  
  
  Rseries = 1e3*1/TrGSeries
  RsComp =  TrRsValue/Rseries
  
  list(
    Rseal  = TrSealResistance, 
    Cslow  = TrCSlow,
    Rseries= Rseries,
    RsComp = RsComp 
   )
}

get_text<-function(con, ptr){
  readlabel(ptr , con, offset = 36) #36 ist immer der text bzw comment, sowohl bei exp als auch bei ser
}

# select, at a given level, elements within a nested list, and keep all attributes 
prune_list_deep <- function(x, indices, current_level = 1, target_level = 2) {
  if (is.list(x)) {
    if (current_level == target_level) {
      ATTRS<-attributes(x)
      ATTRS$names <- ATTRS$names[indices]
      x = x[indices]
      attributes(x)<-ATTRS
      x
      
    }
    ATTRS<-attributes(x)
    x=lapply(x, function(list_element) {
      
      if (is.list(list_element)) {
        
        
        pruned=prune_list_deep(list_element, indices, current_level + 1, target_level)
        #attributes(pruned)<-attributes(list_element)
        pruned
      }  else {
        list_element
      }
    })
    attributes(x)<-ATTRS
    x
    
  } else {
    x
  }
}

