test_that("read_any works as expected",{
  x=(1:100)/20
  y=sin(x)
  anydata<-data.frame(id="test", swp=1:3, x=rep(x,3), y=rep(y,3))%>% mutate(id=paste(id,swp))
  vdiffr::expect_doppelganger("read any data set",
    make_ephysdata(anydata)  %>% ggsweeps(xoffset="realtime")
  )
  
})

test_that("read_Roboocyte_exported_oocyte_dat works as expected",{
  #oo_export<-fs::path(ephysdata::get_examples_path(), "roboocyte", "17-05-23.NR1_2A.P2.G1.dat" ) #will be there after package update
  oo_export<-examplefile("roboocyte/17-05-23.NR1_2A.P2.G1.dat")
  vdiffr::expect_doppelganger("read old robo exports",
    read_Roboocyte_exported_oocyte_dat(oo_export) %>% ggsweeps(xoffset="realtime")
  )
})
