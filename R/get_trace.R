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
  # HAMAMATZU
  if(all(df$ptrs %>% purrr::map_lgl(inherits,"ptrs_HAMA"))){
    
    filename <- df$ptrs %>% purrr::map_chr("file") %>% unique
    wells=unique(df$well)
    
    df <- full_join(df,  read_HAMAMATSU_traces(filename, wells, tracedata_to = {{name}}) , by="well") 
    
  }else{
    # Patchmaster or Roboocyte
    df <- df %>% mutate({{name}}:=  df$ptrs %>% purrr::map(\(ptr){get.ephysdata_fast(ptr, rerun=rerun)}) )
  } 
  
  
  #return(df %>% unnest(data))
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


















