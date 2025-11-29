test_that("scalebars work on ggplot >3.5.0", {
  skip("scalebars in ggplot 3.5.x still throw some warnings")
  expect_no_error(
    {
      # the new ggplot 2.3.5 offers all we need to draw scalebars and topbars
      # (before we where using a lot of packages and proprietary code to do this in ephys3)
      # devtools::install_version("ggplot2", version="3.4.4", lib=".")
      
      # install.packages("ggplot2", lib=".")
      library(ggplot2, lib=".") #load ggplot 3.5.x
      library(patchwork)
      library(dplyr)
      devtools::load_all(); (data.frame(x=(1:50)/10, y=(sin((1:50)/10)*15+15)) %>% ggplot(aes(x,y)) + geom_line() + geom_point(na.rm=T) ->p)+ scalebars()
      
      p + scalebars(ybar.x = .1, ybar.y=.1, xbar.y=.1, xbar.x=.1, ylength = 3)  
      
      p + scalebars(xunit="m")
      
      p + 
        geom_topbar_(0.1,1, line=1,label = "just", border="white" )+ 
        geom_topbar_(1,3, line=2,label = "test", border="white")+ 
        geom_topbar_(3,4, line=3, label="the best", border="white")+ 
        scalebars(xunit="m")
      
      
      
      
    }
  )
})
