
options(data_files_path = ephysdata::get_examples_path())



test_that("streams have names", {
  
  heka <- read_PATCHMASTER( ephysdata::examplefile("NaV"), ser=2) 
  
  heka %>% add_trace(name="test") -> heka_str
  
  expect_true(
    heka_str$stream %>% unique == "test"                 
  )  
})


test_that("we can have multiple streams long-format", {
  
  heka <- read_PATCHMASTER( ephysdata::examplefile("NaV"), ser=2) 
  
  heka %>% add_trace(name="test") %>% add_trace("test2")-> heka_str
  
  expect_true(
    all(heka_str$stream %>% unique == c("test","test2")                  )
  )  
})

test_that("we can have multiple streams wide-format", {
  
  heka <- read_PATCHMASTER( ephysdata::examplefile("NaV"), ser=2) 
  
  heka %>% add_trace_(name="test")  %>% add_trace_("test2")-> heka_str
  
  expect_true(
    all(c("test","test2") %in% names(heka_str)                  )
  )  
})


test_that("streams streams work for Roboo and HEKA and HAMA", {

options(data_files_path = ephysdata::get_examples_path())

expect_no_error({
  read_PATCHMASTER("VG_Blocker.dat", exp = 1, ser = 2, cache_rerun = TRUE) %>% head(3) %>% add_trace_(name = "s1") 
  read_ROBOO("roboocyte/0626.1.r2d", cache_rerun = TRUE) %>% head(3) %>% add_trace_("s1")
  read_ROBOO("roboocyte/0626.1.r2d",get_exported_datatable = FALSE) %>% head(3) %>% add_trace_("s1")
  read_HAMAMATSU("HAMAMATSU/cardiomyocytes.TXT") %>%  head(3) %>% add_trace_("s1") # this uses read_HAMAMATZU_traces_
}) 
  

  
expect_no_error({  
  read_PATCHMASTER("VG_Blocker.dat", exp = 1, ser = 2) %>%  head(1) %>% add_trace()
  read_ROBOO("roboocyte/0626.1.r2d") %>% head(1) %>% add_trace()
  read_ROBOO("roboocyte/0626.1.r2d",get_exported_datatable = FALSE) %>% head(1) %>% add_trace()
  read_HAMAMATSU("HAMAMATSU/cardiomyocytes.TXT") %>%  head(1) %>% add_trace() # this uses the already present data column as a stream and names it "default"
})
  

})
  
all_types <- list(
  HEKA      = read_PATCHMASTER("VG_Blocker.dat", exp = 1, ser = 2) %>% head(2), 
  ROBOxport = read_ROBOO("roboocyte/0626.1.r2d") %>% head(2), 
  ROBO_r2d  = read_ROBOO("roboocyte/0626.1.r2d",get_exported_datatable = FALSE) %>% head(2),
  HAMA      = read_HAMAMATSU("HAMAMATSU/cardiomyocytes.TXT") %>% head(2) 
)

# for all_types ...      test-operation                                                        test_outcome         test-expectation
  all_types          %>% purrr::map(add_trace, name="s1")                                 %>% purrr::map(NROW) %>% purrr::map(expect_equal,2) 
  all_types          %>% purrr::map(add_trace)                                            %>% purrr::map(NROW) %>% purrr::map(expect_equal,2) 
  all_types          %>% purrr::map(add_trace, maxpoints=30) %>% purrr::map(tidyr::unnest, data) %>% purrr::map(NROW) %>% purrr::map(expect_gt,55)
  all_types          %>% purrr::map(add_trace, maxpoints=30) %>% purrr::map(tidyr::unnest, data) %>% purrr::map(NROW) %>% purrr::map(expect_lt,60)
  

 
 
 
