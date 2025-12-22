#' @title setup a drc plan 
#'  
#' @description 
#' Drc plans are a unified way of describing how a drc analysis can be performed on a given data set. 
#' 
#' @details 
#' If we want to perform a drc analysis, we need  to specify certain things: 
#'  a) how is compound name and concentration encoded in the data
#'  b) which sweeps form a group because they belong to the same cell
#'  c) which sweeps of a given cell should be used for the drc analysis
#'  
#' This specification largely depends on the given assay and platform     
#'
#' this one assumes that we have the compound as a serlabel and the concentrations in the swplabels
#' just as we normally do it in projects...
#' setup data for It plots and drc fitting
#'
#' @param ephysdata a tibble generated with one of the read_... functions
#' @param n number of sweeps at end of each liquid period to be used for dose response curve
#'
#' @family drc methods
#' @name drc_plan
#' @examples
#' read_PATCHMASTER(ephysdata::examplefile("hergDRC"),2) %>% 
#' add_cursor_point("peak", 2.28,2.3,max) %>% 
#'   drc_plan_HEKA(3)  %>%
#'   drc_get_lpresults_(peak,  normalize = TRUE)
NULL

#' @export
#' @describeIn drc_plan typical drc plan for HEKA patchmaster data
#' 
#' the liquid periods are defined by the sweep labels that 
#' have been set by the user. 
drc_plan_HEKA<-function(ephysdata, n=3, conc=NA, cpd=NA){
  ephysdata %>%  
    
    group_by(exp, ser) %>% mutate(conc=lp_from_labels(swplabel)) %>% 
    group_by(file, exp, ser, conc) %>%
    
    mutate(drc_sweep=row_number() %in% getlast(row_number(),n)) %>% 
    mutate(cpd=serlabel) %>%
    mutate(drc_id = paste(file, exp, ser)) -> result
  
  if(!is.na(conc[1])){
    result %>% mutate(conc=.env$conc) -> result
  }
  
  if(!is.na(cpd[1])){
    result %>% mutate(cpd=.env$cpd) -> result
  }
  
  result
}

#' @param ephysdata a tibble with ephys data
#'
#' @param cpd a vector if the applied compounds
#' @param conc a vector of same length of cpd, giving the concentrations
#' @param application_starts a vector of sweep nrs where each application starts
#' @param n how many sweeps at the end of each lp are to be included in drc analysis?
#' @param initial_cpd name of buffer before first application, defaults to "EC"
#' @param initial_conc concentration before first application, (usually 0)
#'
#' @export
#' @describeIn drc_plan manual drc plan for any data
#' 
drc_plan_manual <-function(ephysdata,  cpd, conc, application_starts, n=3, initial_cpd="EC", initial_conc=0){
  # drc_id has to be length 1 or length of NROW(ephysdata)
  # cpd has to be length 1, length of unique(drc_id) or NROW(ephysdata)
  # conc has to be of length NROW(ephysdata)
  # n is the number of last traces taken. 
  
  cpd_  <-cut(1:NROW(ephysdata), breaks = c(0,application_starts, Inf), labels = c(initial_cpd,cpd))
  conc_ <-cut(1:NROW(ephysdata), breaks = c(0,application_starts, Inf), labels = c(initial_conc,conc))
  
  ephysdata %>% ungroup %>%
    mutate(cpd=cpd_, conc=conc_)%>%
    group_by(exp,ser,cpd,conc) %>%
    mutate(drc_sweep=row_number() %in% getlast(row_number(),n)) %>%
    mutate(drc_id = paste(file, exp, ser))
}

# drc_plan_manual(cpd=c("Deltamitrin", "Lidocain"), conc=c(1,10),application_starts = c(20,73), n=1) 
