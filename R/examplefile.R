#' provide example files for testing
#'
#' @param path 
#' @export
examplefile <- function(path){
  file.path(ephysdata::get_examples_path(), path)
}