
#' make "ephysdata" object from any tidy data
#' 
#' this function takes a data.frame that should at least contain the columns id, swp, x,y, 
#' where id represents an "experiment", swp represents a "sweep", and x,y the trace data. 
#' From this, an ephysdata object is build which can be fed into the ephys4 toolchain.
#' If the data.frame is large and comes originally from a file, please consider using \link{make_ephysdata2}
#' @param tidy_ephysdata 
#'
#' @return ephys-data
#' @export
#' @family ephys-data-readers
#' @examples
#' x=1:100
#' y=sin(x*.1)
#' 
#' data.frame(id="example_experiment", swp=1, x=x, y=y ) %>%
#' make_ephysdata() %>% 
#' ggsweeps()
make_ephysdata<-function(tidy_ephysdata){
  stopifnot("id" %in% names(tidy_ephysdata))
  stopifnot("swp" %in% names(tidy_ephysdata))
  stopifnot("x" %in% names(tidy_ephysdata))
  stopifnot("y" %in% names(tidy_ephysdata))
  
  
  tidy_ephysdata <- tidy_ephysdata %>%  group_by(across(c(-x, -y))) 
  
  stopifnot( length(unique(tidy_ephysdata$id)) == n_groups(tidy_ephysdata) ) 
  
  tidy_ephysdata %>% 
    summarise(data=list(data.frame(x=as.double(x[!is.na(x)]),y=as.double(y[!is.na(y)]))), xoffset=0, yoffset=0, ptrs=list(list(file="x.dat"))) %>% 
    arrange(as.numeric(swp)) %>% 
    mutate(swp=factor(swp, levels=1:max(swp))) %>%
    # construct swp.start
    group_by(id) %>%
    mutate(l=max(data[[1]]$x, na.rm=T)) %>% ungroup %>%
    mutate(swp.start=lag((cumsum(l)))) %>%
    mutate(swp.start=if_else(is.na(swp.start), 0, swp.start)) %>%
  
    ungroup
}



#' make "ephysdata" object from filename and a file_reader function.
#' 
#' this function just takes the file name and the reader function. 
#' From this, an ephysdata object is build which can be fed into the ephys4 toolchain.
#' It will not contain the raw data in it, but the file pointer and the function to read it. 
#'
#' @param filepath 
#' @param file_reader a function that is able to read the file and returns a data frame with 2 columns named x and y.
#' @param unit optional unit for the y values (for ggsweeps)
#' @param id 
#' @param swp 
#' @param xoffset 
#' @param yoffset 
#' @param swp.start 
#'
#' @return ephys-data
#' @export
#' @family ephys-data-readers
make_ephysdata2<-function(filepath, file_reader, unit="", id=basename(filepath), swp=1, xoffset=0, yoffset=0, swp.start=0){
  
    tibble(id=id, 
           swp=as.factor(swp), 
           xoffset=xoffset, 
           yoffset=yoffset, 
           swp.start=swp.start, 
           ptrs=list(list(
              file       = filepath, 
              file_reader= file_reader,
              unit=unit
             ))
           ) 
}

