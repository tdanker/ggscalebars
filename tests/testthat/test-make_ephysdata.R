test_that("make_ephysdata works as expected",{
  x=(1:100)/20
  y=sin(x)
  anydata<-data.frame(id="test", swp=1:3, x=rep(x,3), y=rep(y,3))%>% mutate(id=paste(id,swp))
  vdiffr::expect_doppelganger("read any data set",
                              make_ephysdata(anydata)  %>% ggsweeps(xoffset="realtime")
  )
  
  expect_no_error(
    make_ephysdata(anydata) %>% check_ephysdata()
  )
  
})
