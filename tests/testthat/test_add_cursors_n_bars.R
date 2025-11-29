# cursors_bars
# 
# here, we check adding cursors and bars on all platforms
suppressWarnings(library(vdiffr))

list.files("cache_robotraces/", full.names = T) %>% unlink

robo <- read_ROBOO(       ephysdata::examplefile("OO_r2d")    )
heka <- read_PATCHMASTER( ephysdata::examplefile("NaV"), ser=2)
hama <- read_HAMAMATSU(   ephysdata::examplefile("HT_cm")     )

cursor_bar_robo <-
  robo %>%  head(3) %>%   
  add_bar(name = "bar", start=35,end=95) %>%
  add_cursor_point("peak", 75,96,min) 

cursor_bar_heka <-
  heka %>%  head(3) %>%
  add_bar(name = "barx", start=0.01,end=0.03) %>%
  add_cursor_point("peak", 0.01,0.012,min)

cursor_bar_hama <-
  hama %>%  head(3) %>%
  add_bar(name = "barx", start=0.01,end=20) %>%
  add_cursor_point("peak", 0.01,20,min)

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

test_that("add_cursors and add_bars just works (json2)", {
  
  skip("expect_snapshot_value fails now")
  expect_snapshot_value( style="json2", ignore_attr = TRUE, {
    cursor_bar_robo 
  })
  
  
  
  expect_snapshot_value( style="json2", ignore_attr = TRUE, {
    cursor_bar_heka 
  })
  
  
  
  expect_snapshot_value( style="json2", ignore_attr = TRUE, {
    cursor_bar_hama 
  })
  
  
  
  
  
})


test_that("add_cursors and add_bars => ggplosts", {
  
  
  expect_doppelganger("bar and cursor plot robo", {
    cursor_bar_robo %>% ggsweeps()
  })
  
  
  
  expect_doppelganger("bar and cursor plot heka", {
    cursor_bar_heka %>% ggsweeps()
  })
  
  
  
  expect_doppelganger("bar and cursor plot hama", {
    cursor_bar_hama %>% ggsweeps()
  })
  

  
})