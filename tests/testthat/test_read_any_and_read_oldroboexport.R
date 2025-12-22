

test_that("read_Roboocyte_exported_oocyte_dat works as expected",{
  #oo_export<-fs::path(ephysdata::get_examples_path(), "roboocyte", "17-05-23.NR1_2A.P2.G1.dat" ) #will be there after package update
  oo_export<-ephysdata_examplefile("roboocyte/17-05-23.NR1_2A.P2.G1.dat")
  vdiffr::expect_doppelganger("read old robo exports",
    read_Roboocyte_exported_oocyte_dat(oo_export) %>% ggsweeps(xoffset="realtime")
  )
  
  expect_no_error(
    read_Roboocyte_exported_oocyte_dat(oo_export) %>% check_ephysdata()
  )
})
