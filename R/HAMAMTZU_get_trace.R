# function that reads traces (only selected wells) from file
read_HAMAMATSU_traces <- function(filename, wells, tracedata_to=data){
  filename<-get_file(filename)
  l<-readLines(filename, n = 30)
  namesLine<-grep("No.*A1.*A2.*A3",l)
  
  names_<-suppressMessages( readr::read_delim(filename,
                                              "\t", escape_double = FALSE, trim_ws = TRUE,
                                              skip = namesLine-1, n_max = 1, col_names = F ))
  
  
  names_<-as.character(names_)
  if(!names_[3]==c("A1")){
    stop("unrecognised file format")
  }
  names_[1]<-"X"
  readr::read_delim(filename,show_col_types = FALSE,
                    "\t",  trim_ws = TRUE,
                    skip = namesLine+2, col_names = names_,col_select = all_of(c(x="No.", wells  ))) %>% 
    tidyr::pivot_longer(-x,names_to = "well", values_to = "y") %>% group_by(well) %>% mutate(x=x/1000) %>% tidyr::nest({{tracedata_to}} := c(x,y))
  
}