


#' cached analysis of ephys data
#' 
#' When reading and analysing ephys data, we always deal with similar situations:
#' the data in the ephysfile will not change after beeing recorded. 
#' Also, in the typical case, our analysis pipeline ("cursors" etc) will not change very often. 
#' So, in most cases, we want to cache the the results of file-reading and analysis. 
#' 
#' This following function caches the results, but still takes care about the possibility that
#'  
#' a) new data may be added to the file
#' 
#' b) we might change the analysis pipeline.  
#' 
#' We cache in 2 stages, so that data is never read twice unless the datafile changes.
#'
#' @param platefile the plate file to be read
#' @param analysis_function analysis pipeline, e.g. cursors
#' @param clean_ana if FALSE (the default), keep cache if analysis changes
#' @param rerun set this to TRUE if you want to force reevaluation of everything. 
#'
#' @export
#'
#' @examples
#' analyse_ROBOO_cached(
#'  ephysdata::examplefile("OO_GABA"), 
#'    . %>%  head(1) %>%  
#'    add_cursor_point(name = "peak",  39, 119, fun = min) %>%
#'    add_cursor_model(name = "exp",  110, 149, model_fun_exp)
#'  )
analyse_ROBOO_cached<-function(platefile, analysis_function = \(x)x, clean_ana=FALSE, rerun=FALSE ){
  file_read = xfun::cache_rds(
    file = basename(platefile), 
    clean=TRUE,  
    rerun = rerun, 
    dir="cache_datafiles/", 
    hash= list(file.info(platefile)$mtime), 
    read_ROBOO(platefile) 
  )
  
  file_analysed = xfun::cache_rds(
    file = basename(platefile), 
    clean=clean_ana, 
    rerun = rerun,
    dir="cache_analysis/", 
    hash= list(file.info(platefile)$mtime, deparse(body(analysis_function))), 
    file_read %>% analysis_function
  )
  file_analysed
}