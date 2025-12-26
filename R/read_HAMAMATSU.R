
# read Hamamatzu export files in table fromat with well names in columns and timepoints in rows


#' Read  data exported from HAMAMATSU
#'
#' @param filename filename
#'
#' @return a tibble containing the data
#' @export
#' @family ephys-data-readers
#' @examples
#'

#'library(ephysdata)
#'read_HAMAMATSU(ephysdata::examplefile("HT_cm")) %>%  head(4) %>%
#' add_cursor_point("peak", 0, 2.5, max) %>%
#' ggsweeps() + xlim(0,11) +facet_wrap(~well)
#'
read_HAMAMATSU <- function(filename){
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
  # data_ <<- suppressMessages( readr::read_delim(filename,
  #                     "\t",  trim_ws = TRUE,
  #                     skip = namesLine+2, col_names = names_,n_max = 1))
  # 
  # 
  # data_<-data_[,-1]
  # data_[,1]<-data_[,1]/1000
  # names(data_)[1]<-"x"
  # data_$file=filename
  # data_$swp = factor(1)
  # data_$swp.start=as.double(0)
  # 
  # data_ %>% tidyr::pivot_longer(c(-x,-file, -swp, -swp.start),  names_to="well", values_to="y") %>%
  # 
  #   tidyr::nest(data= c(x,y)) %>%
  #   mutate(file_=basename(file), id=paste(file_, well, sep="-")) %>% tidyr::nest(ptrs=file) %>%
  #   rename(file=file_) %>%
  #   relocate( id, file, well) %>% mutate(xoffset=0, yoffset=0) ->data_
  # 
  # if(isTRUE(getOption("ephys4.HAMA_ptrs_as_lists"))){
  #   data_$ptrs <- data_$ptrs %>% purrr::map(as.list)
  # }
  WELLS<-names_[-(1:2)]
  data_ = tibble(id=paste0(basename(filename),"-", WELLS), file=basename(filename), well=WELLS, swp = factor(1), swp.start=as.double(0), ptrs=list(list(file=filename)), xoffset=as.double(0), yoffset=as.double(0))
  
  data_ %>% add_ptrs_class("ptrs_HAMA") 

}


#' @exportS3Method get_traces_of_file ptrs_HAMA
#' @keywords internal
get_traces_of_file.ptrs_HAMA <- function(df, name, rerun=TRUE){
  
  filename <- df$ptrs %>% purrr::map_chr("file") %>% unique
  wells=      df$well%>% unique
  
  df <- full_join(df,  read_HAMAMATSU_traces(filename, wells, tracedata_to = {{name}}) , by="well") 
}


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




