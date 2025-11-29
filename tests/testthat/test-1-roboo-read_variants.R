list.files("cache_robotraces/", full.names = T) %>% unlink

test_that("read_Roboo gets the same basic columns regardless of reading method", {
  expect_equal(
    auto<- read_ROBOO(ephysdata::examplefile("OO_GABA"), get_exported_datatable = "auto", verbose = F) %>% select( id, file,well,swp,swp.start,xoffset,yoffset),
    raw<- read_ROBOO(ephysdata::examplefile("OO_GABA"), get_exported_datatable = "F", verbose = F) %>%   select(-ptrs)%>% 
      filter(id %in% auto$id) #some aborted sweeps are missing
  )
  expect_equal(#tolerance = .01,
    auto2<- read_ROBOO(ephysdata::examplefile("OO_r2d"), get_RNAinfo = T, get_exported_datatable = T, verbose = F) %>%  select(RNA.Name,RNA.Concentration, id, file,well,swp,swp.start,xoffset,yoffset),
    raw2 <- read_ROBOO(ephysdata::examplefile("OO_r2d"), get_RNAinfo = T, get_exported_datatable = F, verbose = F) %>%  
      filter(id %in% auto2$id) %>% #some aborted sweeps are missing
      select(RNA.Name,RNA.Concentration, id, file,well,swp,swp.start,xoffset,yoffset)
    )
})



test_that("read_traces gets the same traces from Roboo data regardless of reading method", {
  
  
  all_there                <- read_ROBOO(ephysdata::examplefile("OO_r2d"), verbose=F)    
  all_there_traces         <- all_there %>% get_trace()
  
  all_there_but_raw        <- read_ROBOO(ephysdata::examplefile("OO_r2d"), verbose=F, get_exported_datatable = F) %>% 
    filter(id %in% all_there$id) # some aborted sweeps may be missing    
  all_there_but_raw_traces <- all_there_but_raw %>% get_trace()
  
  expect_equal( NROW( all_there_traces         %>% filter(is.na(y)) ) ,0 )
  expect_equal( NROW( all_there_but_raw_traces %>% filter(is.na(y)) ) ,0 ) 
  
  trace_diff<-max(abs(all_there_traces$y - all_there_but_raw_traces$y))
  trace_span<-max(all_there_traces$y)- min(all_there_traces$y)
  expect_lt(trace_diff, 0.06)
  expect_lt(trace_diff/trace_span, 0.001)
  
  #skip("Fixme: ggsweeps does not work here yet, skipping test")
  vdiffr::expect_doppelganger("test_read_Exported_Traces", all_there %>% head(3) %>% ggsweeps())
  vdiffr::expect_doppelganger("test_read_raw_Traces", all_there_but_raw %>% head(3) %>% ggsweeps())
})

#read_ROBOO(ephysdata::examplefile("OO_GABA"), get_exported_datatable = "F", verbose = F) %>%   filter(id %in% c("0626.1-A1-5", "0626.1-A1-4")) %>% 
#  ggsweeps() + facet_wrap(~swp)
