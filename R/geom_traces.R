#' plot traces
#'
#' @param start start region 
#' @param end end region
#' @param maxpoints reduce to this number of samples
#' @param ... arguments to be passed to geom_line
#' @param filter_fun,filter_fun2 functions to modify the data
#'
#' @export
geom_trace <- function(start=0, end=NA, maxpoints=1000, filter_fun=unfiltered, filter_fun2=unfiltered, ...){
  geom_line( data= .%>% get_trace(start, end,maxpoints, filter_fun={{filter_fun}}, filter_fun2={{filter_fun2}}) %>% mutate(x=x+xoffset), ... )
} 
