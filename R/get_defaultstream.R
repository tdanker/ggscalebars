# this function used to work with get_trace, and was a HUGE performance bottleneck. 
# 




get_defaultstream<-function(ephysdata, yoffset,
                        start=0, 
                        end=NA, 
                        maxpoints=1e12,
                        filter_fun=unfiltered, 
                        filter_fun2=unfiltered, 
                        force_read=F, test_skip_filters=FALSE, HEKA_tracecache=TRUE,
                        ...
){
  
  
  
  
  
  
 
    if(is.na(end)) end=max(ephysdata$x)
    if(is.na(start)) start=min(ephysdata$x)
    
    # for compatibility, we have to allow that filter_fun2 accepts only one parameter(the current trafe data).
    # If a filter_fun_2 accepts a second parameter, it will get the current row.
    # this function allows to call the filter safely even if it accepts only one parameter.
    # safecall_filter_fun2<-function(filter_fun2, ephysdata, data){
    #   if(length(formals(filter_fun2))==1){
    #     filter_fun2(ephysdata)
    #   }else{
    #     filter_fun2(ephysdata, data)
    #   }
    # }
    
    ephysdata %>% 
      mutate(y=filter_fun(y)+ yoffset) %>% 
      tidyr::unnest(y) %>% 
      cut_df(start, end) %>% 
      
      #safecall_filter_fun2(filter_fun2,., data) %>%
      
      #select(x,y) %>% 
      #filter_fun2 %>%
      
      downsample_df(maxpoints) -> ephysdata
  
  
  ephysdata
  
}






