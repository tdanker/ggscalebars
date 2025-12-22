#' provide example files for testing
#'
#' @param path path to a file in the ephysdata library
#' @export
ephysdata_examplefile <- function(path){
  file.path(ephysdata::get_examples_path(), path)
}