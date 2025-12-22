#' function to check if all criteria of ephysdata- objects are fulfilled
#'
#' @noRd
check_ephysdata<-function(ephysdata){
  stopifnot(is.factor(ephysdata$swp))
}
