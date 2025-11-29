test_that("read_HAMAMATZU works", {
  expect_no_error({
  
    
    read_HAMAMATSU(  
      file.path(  ephysdata::get_examples_path(), "HAMAMATSU/cardiomyocytes.TXT")
      ) -> HAMAMATZU_PLATE
    
    
    
  })
  
  expect_equal(  
    NROW(HAMAMATZU_PLATE), 384
  )
  
  expect_equal(
    names(HAMAMATZU_PLATE), c("id" ,       "file",      "well"  ,    "swp"  ,     "swp.start",      "ptrs"  ,    "xoffset" ,  "yoffset")
  )
  
  expect_s3_class(HAMAMATZU_PLATE$ptrs[[1]], "ptrs_HAMA")
  
 
  
})
