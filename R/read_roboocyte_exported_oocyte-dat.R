
#' read Roboocyte exported traces (.dat files)
#'
#' with the old Roboocyte, we used to export single oocytes into .dat files. 
#' This function is able to import these files. 
#' 
#' @param file the exported .dat file 
#' @family ephys-data-readers
#' @return ephys-data
#' @export
read_Roboocyte_exported_oocyte_dat<-function(file){
  mydata <- read.csv(file = file, head = T, sep = "\t")
  X<-select(mydata, ends_with("X.")) %>% 
    tidyr::pivot_longer(cols=everything(), names_pattern = "(.*)\\.X\\.", values_to = "x", names_to="id") 
  Y<-select(mydata, ends_with("Y.")) %>% 
    tidyr::pivot_longer(cols=everything(), names_pattern = "(.*)\\.Y\\.", values_to = "y", names_to="id") 
  X$y<-Y$y
  
  X %>% tidyr::separate(id, into=c("Well", "swp"), sep = ".Rec", remove = F) %>%  
    make_ephysdata
}