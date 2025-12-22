
#' make "ephysdata" object from any tidy data
#' 
#' this function takes a data.frame that should at least contain the columns id, swp, x,y, 
#' where id represents an "experiment", swp represents a "sweep", and x,y the trace data. 
#' From this, an ephysdata object is build which can be fed into the ephys4 toolchain.
#' 
#' 
#' @param tidy_ephysdata input data.frame to be converted into an "ephysdata" object. 
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





