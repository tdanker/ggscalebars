#' get raw data traces
#'
#' @param df tibble generated with read_PATCHMASTER
#' @param start optional start point
#' @param n  optional read n data points
#' @param maxpoints optional reduce to this number of points
#' @param filter_fun function to modify the y component of the trace data. Executed before cutting (except patchmaster, which reads partial), filter_fun2, and downsampling 
#' @param filter_fun2 function to modify the complete trace data frame. Executed after cutting start to end.  
#' @param unnest.data should the resulting columns stay unnested? defaults to TRUE
#' @param name name of the stream to read from, if there are streams
#' @param rerun only for patchmaster or roboocyte, rerun the cache
#' @param ... unused
#'
#' @export
get_trace<-function(df, 
                           start=0, 
                           n=NA, # this is for compatibility - unfortunately we used to call this "n", but it actually means "end"
                           #end=n,
                           maxpoints=1e12, 
                           filter_fun=unfiltered, 
                           filter_fun2=unfiltered, 
                           unnest.data=T, 
                           name=!!sym("data"),
                           
                           rerun=FALSE,
                    ...){
  
  end=n
  
 
  
  df <- get_traces_per_file(df, {{name}})
 
  
 
  df <- df %>% tidyr::unnest({{name}}) 
  if(is.na(end)) end=max(df$x)
  if(is.na(start)) start=min(df$x)
  
  
  df <- df %>% group_by(id) %>% 
     mutate(y=filter_fun(y)+yoffset) %>% 
     cut_df(start, end) %>% group_by(id) %>% 
     downsample_df(maxpoints)  %>% filter(! is.na(y))
  
  if(!unnest.data){
    df <- df %>% group_by(id) %>% filter(!is.na(id))  %>% tidyr::nest({{name}}:= any_of(c( "x", "y", "TraceTime"))) %>% ungroup #%>%
      #rowwise # the rowwise is just for compatibility, to produce the same output as get_trace
  }
  
  df 
}





get_traces_per_file<-function(df, name){
  
  df %>% 
    
    # prepare for the split: add file column
    mutate(ptrs.file=purrr::map_chr(ptrs,"file")) %>% 
    
    #group-split by file
    group_by(file) %>% group_split()  %>% 
    
    # add the ptrs class to each list element
    purrr::map(add_ptrs_class_to_df) %>%
    
    # call apropriate S3 method
    purrr::map(get_traces_of_file, name={{name}}) %>% 
    
    # remove ptrs class again
    purrr::map(remove_ptrs_class) %>% 
    
    # re-combine split
    purrr::list_rbind() %>% 
    
    # remove file column 
    select(-ptrs.file)
  
}


#' Internal helper
#'
#' @param df internal use
#' @param name  internal use 
#' @param rerun internal use
#'
#' @description
#' This function is exported for technical reasons but is not
#' intended for direct user use.
#'
#' @keywords internal
#' @export
get_traces_of_file <- function(df, name, rerun=TRUE) {
  UseMethod("get_traces_of_file")
}



# helper functions:
# add to df the class of the first element in column ptrs 
add_ptrs_class_to_df<-function(df){
  ptrs_class <- class(df$ptrs[[1]])[1]
  class(df)<-c(ptrs_class, class(df))
  df
}

# remove what we have added with add_ptrs_class
remove_ptrs_class<-function(df){
  class(df)<-class(df)[-1]
  df
}



# helper function used by read.xxx functions to set the class of the ptrs
add_ptrs_class <- function(., ptrs_class){ mutate(., ptrs = purrr::map(ptrs, add_class, ptrs_class))}

# padavan
add_class<-function(x,newclass){class(x)<-c(newclass,class(x));x}













