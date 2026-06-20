test_that("scalebars auto-length work even when data range is very small", {
 
  # we usually do not have a problem with small data ranges as long as we set xlength and ylength manually. 
  # here, auto-calculation of xlength and ylength is tested for this case
  
  
  # generate example data with very small data range in both x and y axes
  x=seq(-10,10, length.out=100)
  testdata <- data.frame(x=x/10, y=dnorm(x)/(3*1e2))
  p<- ggplot(testdata, aes(x,y)) + geom_line()
  
  
  # inner bars:
  vdiffr::expect_doppelganger("inner bars1", {
  p + scalebars(x=.1,y=.1, yunit="pA", xunit = "ms", yfactor=1000, xfactor=1000, expand = F,xlength = .5,ylength = .00025)
  })
  
  vdiffr::expect_doppelganger("inner bars2", {
    p + scalebars(x=.1,y=.1, yunit="pA", xunit = "ms", yfactor=1000, xfactor=1000, expand = F                               )
    
  })
 
  
  # outer bars
  
  vdiffr::expect_doppelganger("outer bars1", {
    p + scalebars(yunit="pA", xunit = "ms", yfactor=1000, xfactor=1000, expand = F,xlength = .25,ylength = .0005)
  })
  
  vdiffr::expect_doppelganger("outer bars2", {
    p + scalebars(yunit="pA", xunit = "ms", yfactor=1000, xfactor=1000, expand = F                                       )
  })
  
                           

})
