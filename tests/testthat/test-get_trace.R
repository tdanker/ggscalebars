test_that("get_trace does not deliver NA rows", {
  
  RESULT        <- read_ROBOO(ephysdata::examplefile("OO_r2d"), verbose=F, get_exported_datatable = F) %>% head(3) %>% get_trace()
  
  
  
  expect_equal( NROW( RESULT         %>% filter(is.na(y)) ) ,0 )
 
})


