#' a filter function that does nothing
#'
#' @param x input and output of the function.  
#'
#' @export
unfiltered<-function(x){x}



# y_corrected = x-b - x *  (d-b)/  (cursor_d - cursor_b)



#' create drift filter
#'
#' @param p1 first anchor, also called "baseline", 
#' @param p2 second anchor, also called "drift"
#' 
#' @return a function that can be used as a filter in \link{add_stream}
#' @export
#'
createFilter_driftcorrection<-function(p1, p2){
  function(x){

  drift=(x[p2]-x[p1])/(p2-p1)   # drift:   (d-b) / (cursor_d - cursor_b)
  
  x=x-x[p1] + (1:length(x))* -drift 
  
  
  
  
  x
}}