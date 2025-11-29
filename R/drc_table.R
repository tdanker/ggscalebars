#' Print results as a table
#'
#' For more info, see \href{../doc/drc.html}{\code{vignette("Dose resoponse curves", package = "ephys4")}}
#'
#' @param df2 results from drc_fit
#'
#'
#' @family drc methods
#' @return a printable table of the fit results
#' @export
#'
#' @examples
#' read_PATCHMASTER(ephysdata::examplefile("hergDRC"),2) %>% 
#' add_cursor_point("peak", 2.28,2.3,max) %>% 
#'   drc_plan_HEKA(3)  %>%
#'   drc_get_lpresults_(peak,  normalize = TRUE) %>% drc_fit %>% drc_table
drc_table<-function(df2){

  df2 %>% tidyr::unnest_legacy(table, .drop = T) %>%
    mutate(across(where(is.numeric), ~as.character(signif(., 3)))) 
}


#' Print results as a table
#'
#' @param df2 results from drc_fit
#'
#' @return a printable table of the fit results
#' @export
drc_kable <- function(df2){
  drc_table(df2) %>% knitr::kable(.)
}
