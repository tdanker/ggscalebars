#' Streams
#' 
#' Functions to prefetch the data into a "stream". Default name is "data" and will be used by cursors etc by default.
#'
#' streams can be a huge performance boost, mostly when it comes to using cursors.   
#' streams ensure that all cursors use the same filter (which was a big design flaw before).
#' 
#' streams boost performance even if we do not reduce the maxpoints. if we do, it is even more.
#' streams allow us to co-plot and co-analyse different filter settings. For this, we can have then as 
#'  long or wide, and we can convert between them. 
#'
#' in wide format, the default stream is (currently) called 'data', the others are named ~.str
#' in long format, all streams are in the 'data' column, and there is a column called "stream" which holds the name
#'
#' long format will have all cursors beeing calculated on all streams
#' and plot all streams (unless they are not filtered out, which is easy to do)
#' streams can also easily get colored and faceted using standard ggplot features
#'
#' on the other hand, in wide format, only the default stream is used by default for plotting and cursoring
#' the other streams can be addressed by low-level plotting and by the stream parameter of the cursors. 
#' this is good if we want to control things in great detail, using each stream differently. 
#' 
#' 
#' Using streams is optional, because often this is not what we want: 
#' cursors just operate on a large data set, and get what they need very efficiently by reading just a part of the data. 
#'            and are e.g. just used for statistics or filtering. 
#'            We do not always want to blow up the dataframe, and often we will 
#'            need the stream data only on a small subset (e.g.examples that we plot). 
#'            ==> only the user can decide if the stream data is useful to be kept for beeing reused.


# So both long and wide format have their rationale. 

#' @param df ephys data
#'
#' @param name optional; name of the stream
#' @param start optional start of the stream in seconds, in case only part of the trace should be included 
#' @param end optional end of the stream in seconds
#' @param maxpoints optional parameter for downsampling to approximately this many points
#' @param filter_fun optional filter function operating on y 
#' @param filter_fun2 optional filter function operating on x/y
#' @param check_name (internal)
#'
#' @export
#' @describeIn add_trace_ add a stream "wide" variant
add_trace_ <- function(df, name="default", start=0, end=NA, maxpoints=1e9, filter_fun=unfiltered, filter_fun2=unfiltered, check_name=T){
  
  if(check_name && stringr::str_ends({{name}}, ".str")) 
    warning("name of stream should not end with str when using add_trace_. Use add_trace (without the underscore) instead")
  
  xplain_name<- if(missing(name)) "(the default)" else ""
  
  if({{name}} %in% names(df))
    stop(glue::glue("this is add_trace_, while trying to add a stream with name {{{name}}} {xplain_name}: a column with the name {{{name}}} is already present in your data set - this would override existing data. ")  )
  
  get_trace(df, 
            
            start=start, n=end, maxpoints = maxpoints, filter_fun = filter_fun, filter_fun2 = filter_fun2, 
            force_read = T, 
            unnest.data = F, name={{name}})
}




#' @describeIn add_trace_ add a stream "long" variant
#' @export 
add_trace <- function(df, name="default", start=0, end=NA, maxpoints=1e9, filter_fun=unfiltered, filter_fun2=unfiltered, check_name=F){
  
  # if the user not already did it, we add .str to the name, which signals pivot_longer_streams that it should be included in the pivoted set of streams. 
  if(!stringr::str_ends(name, ".str")) name=paste0(name, ".str")
  
  # this step will only be executed if there are already pivoted streams
  if("stream" %in% names(df))
    df %>% pivot_wider_streams -> df
  
  # add stream.str and pivot to long format
  df <-
   df %>% 
    tibble::rowid_to_column() %>% 
    add_trace_({{name}}, start=start, end=end, maxpoints=maxpoints, filter_fun=filter_fun, filter_fun2 = filter_fun2, check_name = check_name) %>% 
    pivot_longer_streams() %>% 
    arrange(rowid) %>% select(-rowid) 
  
  if(! length(unique(df$stream))==1 ){
   df <-
     df %>% mutate(id=paste(id,stream))  
  }
  
  df
  
  
}


# helper functions used by add_trace
pivot_longer_streams <- 
  . %>% tidyr::pivot_longer(cols=c(any_of("data"), ends_with(".str")), names_to="stream", values_to = "data", names_pattern = "(.*)\\.str")



pivot_wider_streams <-
  . %>% tidyr::pivot_wider(names_from = stream, values_from = data, names_glue ="{stream}.str" )


