test_that("read_HAMAMATZU_traces works", {
  
  expect_no_error({
    Hfile <- file.path(  ephysdata::get_examples_path(), "HAMAMATSU/cardiomyocytes.TXT")
    
    
    
    # read exactly the wells that are needed, rather than all wells. We should do this per file, of course. For now we assume that all is from one file. 
    read_HAMAMATSU_traces (Hfile,c("A1", "B5")  )  
    
    
    
   
  })
  
  
  
})
