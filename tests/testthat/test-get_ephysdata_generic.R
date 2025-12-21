# get_trace
# 
# here, we check promises that we make for reading on all platforms

list.files("cache_robotraces/", full.names = T) %>% unlink

robo.r2d <- read_ROBOO(       ephysdata::examplefile("OO_r2d")  ,get_exported_datatable = FALSE  )  %>% head(2)
robo     <- read_ROBOO(       ephysdata::examplefile("OO_r2d")  ,get_exported_datatable = TRUE   )  %>% head(2)
heka     <- read_PATCHMASTER( ephysdata::examplefile("NaV"), ser=2) %>% head(2)


hama     <- read_HAMAMATSU(   ephysdata::examplefile("HT_cm")     ) %>% head(2)

#library(purrr)


# make sure that we do not get weird errors when using read_xxx function in notebooks:
test_that("use list, not tibble, for storing ptrs", {
  
  expect_equal(
    heka$ptrs[[1]] %>% class, c("list", "ptrs_heka")
  )
 
  expect_equal(
    robo.r2d$ptrs[[1]] %>% class , c("list", "ptrs_robo")
  )
  
  expect_equal(
    robo$ptrs[[1]] %>% class , c("list", "ptrs_robo_from_exported_data")
  )

  expect_equal(
    hama$ptrs[[1]] %>% class ,  c("list", "ptrs_HAMA")
  )
})







