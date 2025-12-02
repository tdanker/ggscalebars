test_that("add_cursor_point produces correct results", {
  read_PATCHMASTER(ephysdata::examplefile("hergDRC"),2) %>% 
    add_cursor_point("peak", 2.28,2.3,max)->cursor_results
  expect_snapshot_value(cursor_results$id, style = "json2")
  expect_snapshot_value(cursor_results$peak, style = "json2")
})


test_that("add_cursor_point with condition produces correct results", {
  read_PATCHMASTER(ephysdata::examplefile("hergDRC"),2) %>% 
    add_cursor_point("peak", 2.28,2.3,max, condition=swp==5)->cursor_results
  expect_snapshot_value(cursor_results$id, style = "json2")
  expect_snapshot_value(cursor_results$peak, style = "json2")
})


# cursors_bars
# 
# here, we check adding all type of cursors 
suppressWarnings(library(vdiffr))


heka <- read_PATCHMASTER( ephysdata::examplefile("NaV"), ser=2) 

test_that("we can use streams with cursors",{
  # here, we create a custom stream and then use it with the cursor. 
  expect_no_error({
    heka      %>% slice(c(4,6)) %>%
      add_stream_("mystream",  filter_fun = bf0.2) %>% 
      add_stream_("unfiltered") %>% 
      add_cursor(name = "peak_unfiltered",start=0.007, end=0.02,cfun = point_, fun=min, stream = unfiltered) %>%
      add_cursor(name = "peak",start=0.007, end=0.02,cfun = point_, fun=min, stream = mystream) %>% 
      ggplot(aes(x,y, group=id)) + 
      geom_cursor_range_("peak",col = alpha("red", .03),fill = alpha("red", .03)) + 
      geom_line(data=. %>% unnest(unfiltered), alpha = .2) + 
      geom_line(data=. %>% unnest(mystream)) + 
      geom_cursor_point_("peak_unfiltered", color="red",alpha=.2) + 
      geom_cursor_point_("peak", color="red") 
  })
  
  expect_no_error({
    heka      %>% slice(c(4,6)) %>%
      
      add_stream("unfiltered") %>% 
      add_stream("mystream",  filter_fun = bf0.2) %>% 
      mutate(stream=factor(stream, levels=c( "unfiltered","mystream") )) %>%
      
      add_cursor(name = "peak",start=0.007, end=0.02,cfun = point_, fun=min, stream = data) %>% 
      ggplot(aes(x,y, group=paste(stream,id), color=stream)) + 
      geom_cursor_range_("peak",col = alpha("red", .03),fill = alpha("red", .03)) + 
      
      geom_line(data=. %>% unnest(data) %>% arrange((stream)) ) + 
      
      geom_cursor_point_("peak") + 
      
      scale_color_manual(values = c( unfiltered="gray",mystream="black"))+ theme_bw()
  })
  
  
})

test_that("we can use default-stream 'data' with cursors without naming it",{
  testthat::skip(message = "this works but we want to simplyfy it in add_cursor")
  # here, we create a custom stream and then use it with the cursor. 
  expect_no_error({
    stop("this works but we want to simplyfy it in add_cursor")
    heka      %>% slice(c(4,6)) %>%
      add_stream() %>% 
      add_cursor(name = "peak",start=0.007, end=0.02,cfun = point_, fun=min) %>% 
      unnest(data) %>%
      ggplot(aes(x,y, group=id)) + geom_cursor_range_("peak") + geom_line() + geom_cursor_point_("peak", color="red") 
  })
  
})


# test_that("cursor from stream gets the same result", {
#   #this wont work at the moment since we require to have a stream now. 
# heka %>% add_stream() -> heka_str
# csr_fun=function(y){
#   min(y, na.rm=TRUE)
# }
#   expect_no_error(
#     bench::mark( check=TRUE,filter_gc = FALSE,
#                  heka_str  %>% add_cursor_point("peak", 0.02, 0.22, csr_fun) %>% select(peak)
#                  ,
#                  heka      %>% add_cursor_point("peak", 0.02, 0.22, csr_fun)%>% select(peak)
#     ) 
#   )
# })


test_that("curosr annots look the same, coming from attribute or cursor resultlist ", {
  expect_no_error(
    {
      library(patchwork)
      options("ephys4.cursor_annots_not_from_attr"=FALSE)
      heka      %>% 
        add_stream %>% 
        add_cursor_point("peak", 0.01, 0.012, min) %>% ggsweeps() -> p1
      
      
      options("ephys4.cursor_annots_not_from_attr"=TRUE)
      heka      %>% 
        add_stream %>% 
        add_cursor_point("peak", 0.01, 0.012, min) %>% ggsweeps() -> p2
      print(p1 + p2)
      
      
    }
    
  ) 
  
  # when we use add_cursor (not add_cursor_point), we get the point_annot without the range. 
  # its the same for both, so its ok. 
  expect_no_error(
    {
      library(patchwork)
      options("ephys4.cursor_annots_not_from_attr"=FALSE)
      heka      %>% 
        add_stream %>% 
        add_cursor("peak", 0.01, 0.012, point_,fun=min) %>% ggsweeps() -> p1
      
      
      options("ephys4.cursor_annots_not_from_attr"=TRUE)
      heka      %>% 
        add_stream %>% 
        add_cursor("peak", 0.01, 0.012, point_,fun=min) %>% ggsweeps() -> p2
      print(p1 + p2)
      
      
    }
    
  ) 
  
  
  if(FALSE){
    options("ephys4.cursor_annots_not_from_attr"=FALSE)
    
    # in this case (old behaviour), we completely loose the cursor annotations. 
    bind_rows(
      heka_str %>% slice(c(7))     %>%  add_cursor("peak", 0.01, 0.012, point_,fun=min), 
      heka_str  %>% slice(c(6))    %>%  add_cursor_point("peak", 0.01, 0.012, min)
    )    %>% ggsweeps() -> p1
    
    options("ephys4.cursor_annots_not_from_attr"=TRUE)
    
    # now, we get the cursor annotations which are first in the list. This is better, but not really good. 
    bind_rows(
      heka_str %>% slice(c(7))     %>%  add_cursor("peak", 0.01, 0.012, point_,fun=min), 
      heka_str  %>% slice(c(6))    %>%  add_cursor_point("peak", 0.01, 0.012, min)
    )    %>% ggsweeps() -> p2
    
    print(p1 + p2)
  }
  
})


test_that("if we add stream before using cursors, everything is ok ", {
  expect_no_error(
    
    
    heka      %>% 
      add_stream %>% 
      add_cursor_point("peak", 0.01, 0.012, min) %>% ggsweeps()
  ) 
  
})

test_that("cursor_points works without error", {
  skip("fixme: cursor_points is currently broken")
  # reactivate the example when fiexed!
  expect_no_error({
    examplefile = file.path(ephysdata::get_examples_path(), "cardiopatch/21-07-Cardio.dat")
    read_PATCHMASTER(examplefile,exp=23, ser=3,  trc="Vmon") %>% filter(serlabel=="CC0") %>%
      add_cursor_points("starts", fun=min, direction = "below", long = TRUE) %>%
      mutate(ends.x=lead(starts.x), .after=starts.x) %>% slice(1:6) %>%
      add_cursor("min", start=starts.x, end=ends.x, cfun = min) %>%
      ggsweeps(maxpoints = 1e12) + xlim(0,6)+
      ylab("mV") +
      cowplot::theme_cowplot(12)
  }
    
  )
  
})




# #heka_str  %>% head(1) %>% add_cursor_point("peak", 0.02, 0.22, csr_fun) %>% ggsweeps  # cursor ist sictbar
# #heka      %>% head(2) %>% add_cursor_point("peak", 0.02, 0.22, csr_fun) %>% ggsweeps  # cursor ist nicht sichtbar
# test_that("get_trace_ preserves attributes of cursors", {
#   expect_true(
#     heka      %>% head(2) %>% add_cursor_point("peak", 0.02, 0.22, csr_fun) %>%  get_trace %>%
#       .$peak.csr %>% attributes %>% names =="annot"
#     #heka      %>% head(2) %>% add_cursor_point("peak", 0.02, 0.22, csr_fun) %>% add_stream() %>%  .$peak.csr %>% attributes %>% names =="annot"
#   )
# })



# cursor_bar_heka <-
#   heka %>%  head(3) %>% add_stream() %>%
#   add_bar(name = "barx", start=0.01,end=0.03) %>%
#   add_cursor_point("peak", 0.01,0.012,min) %>% 
#   add_cursor_level("lvl", 0.01,0.012,min ) 




# test_that("add_cursors and add_bars just works", {
#   
#   
#   
#   
#   expect_snapshot_output(  {
#     cursor_bar_robo %>% print.data.frame() 
#   })
#   
#   
#   
#   expect_snapshot_output(  {
#     cursor_bar_heka  %>% print.data.frame()
#   })
#   
#   
#   
#   expect_snapshot_output(  {
#     cursor_bar_hama %>% print.data.frame() 
#   })
#   
#   
#   
# })

# test_that("add_cursors and add_bars just works (json2)", {
#   
#   expect_snapshot_value( style="json2", ignore_attr = TRUE, {
#     cursor_bar_heka 
#   })
#   
# })


# test_that("add_cursors and add_bars => ggplosts", {
# 
#   expect_doppelganger("bar and cursor plot heka", {
#     cursor_bar_heka %>% ggsweeps()
#   })
# 
# })
