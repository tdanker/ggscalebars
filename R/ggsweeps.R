#' Visualize  Ephys data using ggplot
#'
#' 
#' @param df data to be plotted, typically from get_PATCHMASTER
#' @param start,end currently deprecated, could be used when automatically setting up a stream
#' @param filter_fun,filter_fun2 used when automatically setting up a stream 
#' @param maxpoints how many points to draw. speeds up plotting if set to low values
#' @param style control appearance of bars (topspace etc) 
#' @param xoffset ofsetting of traces on the x-axis. use "real", "none" for overlay, or numeric.  
#' @param yoffset ofsetting of traces on the y-axis.  numeric.
#' @param ... further arguments passed to geom_trace 
#'
#' @return a ggplot object
#' @export
#' 
#' @examples 
#'  library("ephysdata")
#'  read_PATCHMASTER(ephysdata::examplefile("herg")) %>% filter(exp==1, ser==1, swp==1, trc=="Imon-1") %>% ggsweeps()
#'  
ggsweeps<-function(df,
                   start=NA, #fixme make consistent to filter_fun (ditch or keep both )
                   end=NA, 
                   filter_fun=unfiltered, 
                   filter_fun2=unfiltered, 
                   maxpoints=1000,
                   style             = ggsweeps.defaultstyle, #fixme make a styler-function
                   
                   xoffset= c("none", "realtime"),
                   yoffset=0,
                  
                   
                   ...){
  
  bottomspace       = style$bottomspace
  topspace          = style$topspace 
  height            = style$height 
  space             = style$space 
  bar.colors        = style$bar.colors
  axis.bottom.style = style$axis.bottom.style 
  axis.left.style   = style$axis.left.style
  
  assertthat::assert_that(
    is.na(start), 
    msg="using start value in ggsweeps is deprecated now"
  )
  
  assertthat::assert_that(
    is.na(end), 
    msg="using end value in ggsweeps is deprecated now"
  )
  
  assertthat::assert_that(
    NROW(df)>0, 
    msg="plotting with ggsweeps failed because there are 0 rows of data to plot"
  )
  
  assertthat::assert_that(
    "ptrs" %in% names(df), 
    msg="plotting with ggseeps requires a sepecial column named 'ptrs' in your data set, which is missing"
  )
  
  df <- df %>% ungroup
  
  df <- calculate_offsets(df, xoffset=xoffset , yoffset=yoffset)
  
  suppressMessages(suppressWarnings({
   
   
      
      # add a stream if we dont have one
      
      if(! "data" %in% names(df)) {
        warning("adding a stream in ggsweeps - consider adding earlier!")
        df %>% add_trace(start=start, end=end, filter_fun=filter_fun, filter_fun2=filter_fun2, maxpoints=maxpoints) -> df
        
        }
      
    
   
      
      # we try to not duplicate trace data even if we have multiple rows with different cursor 
      # annotations. (see tests for add_cursor_points(long=TRUE) how we could get into such a situation). 
      # if we have such duplicated rows, this brings an enourmous performance boost.
      
      
      # first we get all columns that do not vary for the same id - the trace data is among them. 
      nonvarying_cols <- df %>% 
        group_by(id) %>% 
        summarise(across(everything(), ~ n_distinct(.x))) %>% 
        select(where(~ all(.x == 1))) %>% 
        names()
      
      # now, we can distinct all the trace data, and also keep the nonvarying cols, in case we want to use them for trace coloring etc. 
      TRACES <-
        df %>% select(id, any_of(nonvarying_cols), data) %>% distinct()%>% tidyr::unnest(data) %>% group_by(id)
      
     
    
      # we add the varying cols as "meta" attribute so that we can use them to plot cursor results
      attr(TRACES, "meta") <- df %>% select(-data)
    
             p <- 
               TRACES %>% 
               mutate(x=x+xoffset, y=y+yoffset) %>% #tidyr::nest(data_=c(x, y)) %>% ungroup %>%
               
               ggplot(aes(x,y, group=id)) + 
               
               #geoms_cursor_annotations_under +
               
               geom_line(..., na.rm=T)+ 
               
               xlab("seconds") + 
               ylab(get_yunit(df))+               
               geoms_cursor_annotations(df)
             
      
          
          p <- add_bars(p, df, TRACES,topspace, bottomspace, height, space, bar.colors)
          
          p
    
  }))
}

geoms_cursor_annotations <- function(df){
  cnames <- df %>% select(contains("csr")) %>%  names
  cnames %>% purrr::map(get_annots, df)
}

add_bars<-function(p, df, TRACES,topspace, bottomspace, height, space, bar.colors){
  if( (df %>% select(contains(".bar")) %>% NCOL) > 0 ){
    
    df <- df %>% select(contains(".bar"), swp, xoffset, yoffset, id)
    
    p<- 
      p + 
      auto_bars(df, space = space, topspace = topspace, height = height, colors = bar.colors) + 
      geom_blank(
        aes(x_=NULL, y=y_), 
        data= get_geom_blank_limits(df, TRACES, bottomspace,topspace, height, space), 
        inherit.aes = F)
  }
  p
}


get_geom_blank_limits<-function(df, TRACES, bottomspace,topspace, height, space){
  # from an ephys df containing bars, and the TRACES data to plot, get the limits for a geom_blank
  # which will be used to shift the plotted data downwards to have room for the bars. 
  maxline <-
    # how many lines do our bars use ?
    # bars have a line parameter. The maximum of any of these is what we need to callculate how much space we will need. 
    df %>%  
    select( contains(".bar")) %>% 
    tidyr::pivot_longer(contains(".bar")) %>% 
    tidyr::hoist(value, "line") %$% line %>% max
  
  space_for_bars<-
    # space needed as fraction of the canvas height
    bottomspace+topspace+((height+space)*maxline)
  
  data.frame(x_=min(TRACES$x), y_=max(TRACES$y) + diff(range(TRACES$y)) * (space_for_bars/(1-space_for_bars)))
}

get_yunit<-function(df){
  yunit<-""
  if(stringr::str_ends(df$ptrs[[1]]$file, ".dat")){
    if(stringr::str_ends(df$ptrs[[1]]$file, "Export_Datatable.dat")){
      yunit<-"\u00b5A"
    }else{
      yunit<-"nA"
    }
  } 
  if(stringr::str_ends(df$ptrs[[1]]$file, ".r2d")) yunit<-"\u00b5A"
  if(stringr::str_ends(df$ptrs[[1]]$file, ".TXT")) yunit<-"Units"
  if("unit" %in% names(df$ptrs[[1]]))              yunit<-unique(df$ptrs[[1]]$unit)
  return(yunit) 
}

calculate_offsets <- function(df, xoffset= c("none", "realtime"), yoffset) {
  # handling of xoffset and yoffsets
  if(is.character(xoffset)){
    xoffset<-match.arg(xoffset)
    
    if(xoffset=="realtime"){
      df <- df %>%  mutate(xoffset = as.numeric(swp.start-swp.start[1]))
      
    }
    
  }else{
    assertthat::assert_that(
      is.numeric(xoffset), 
      msg=glue::glue("error in ggsweeps(... : xoffset should be either numeric or a string like 'none' or 'realtime' "))
    
    df <- df %>%  mutate(xoffset = (as.numeric(swp)-as.numeric(swp[1]))* .env$xoffset)
    
  }
  
  df <- df %>%  mutate(yoffset = (as.numeric(swp)-as.numeric(swp[1]))* .env$yoffset)
}


ggsweeps.defaultstyle = list(
  bottomspace=0,
  topspace=.01, 
  height=.05, 
  space=.01, 
  bar.colors="grey90",
  axis.bottom.style="none", 
  axis.left.style="none"
)

# helper for ggsweeps, gets the annotation fun from the attribute of the cursor column:
get_annots<-function(csrname, data, level){
  csr_basename = stringr::str_remove(csrname, ".csr")
  if(isTRUE(getOption("ephys4.cursor_annots_not_from_attr"))){
    fun = data[[csrname]][[1]]$annotation[[1]]
   
  }else{
    
    #the old style
    fun =data[[csrname]]%>% attr("annot", exact = TRUE)
  }
  
  
  if(!is.null(fun)) fun(csr_basename) 
  # this will call a function like e.g. point_.annot (typically defined in cursor_annotations.R)
  
}

# helper for ggsweeps, gets the annotation fun from the attribute of the cursor column:
get_annots_under<-function(csrname, data, level){
  csr_basename = stringr::str_remove(csrname, ".csr")
  fun=data[[csrname]]%>% attr("annot_under", exact = TRUE)
  if(!is.null(fun)) fun(csr_basename)
  
  
}
