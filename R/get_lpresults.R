# relies on setup_drc_~. 
# so it is always the same. 
#' get liquid period results for It plots and drc fitting
#'
#' @param ephysdata a tibble with ephysdata as read with read_xxx 
#' @param col colum name - usually a cursor name -  to be used to calulate the results
#' @param normalize should the results be normalized ?
#' @param no_zero_conc should the concentration value zero be omitted? 
#'
#' @return ephys-data prepared for beeing used by \link{drc_fit}
#' @export
#'
#' @examples
#' read_PATCHMASTER(ephysdata::examplefile("hergDRC"),2) %>% 
#' add_cursor_point("peak", 2.28,2.3,max) %>% 
#'   drc_plan_HEKA(3)  %>%
#'   drc_get_lpresults_(peak,  normalize = TRUE)
drc_get_lpresults_<-function(ephysdata, col,  normalize=F, no_zero_conc=normalize){
  msg="get_lpresults needs the columns cpd, conc, drc_id, and drc_sweep in its input data. Please use a proper setup_drc method to prepare your data !"
  assertthat::assert_that(ephysdata %>% tibble::has_name("conc"),   
                          ephysdata %>% tibble::has_name("cpd"),  
                          ephysdata %>% tibble::has_name("drc_id"),  
                          ephysdata %>% tibble::has_name("drc_sweep"),
                          msg=msg)  
  result <-  
    ephysdata %>% 
    filter(drc_sweep) %>%
    group_by(drc_id, cpd, conc) %>%
    summarise(res=mean( {{ col }} )) %>% 
    relocate(res, conc, cpd) # we relocate just because drc_fit goes by order - at least for now
  
  if(normalize){
    result %>% mutate(res=res/res[1])  -> result
  }
  if(no_zero_conc){
    result %>% filter(!conc==0)-> result
  }
  return(result  )
}
