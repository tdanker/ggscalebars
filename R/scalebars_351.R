#' scalebars
#' 
#' One or both axes can be replaced by scalebars. 
#' 
#' This is a shortcut to do the combination of coord_scalebars and theme_scalebars. 
#'
#' @inheritParams theme_scalebars 
#' @inheritParams coord_scalebars
#'
#' @export
#'
#' @examples
#' df <- data.frame(x = 1:31, y = 30*sin((-15:15)*.2))
#' ggplot(df, aes(x, y)) + 
#'   geom_line() +
#'   scalebars(ylength=3, yunit="Kaese", xunit="Meilen")
scalebars<-function(
  x=0,
  y=0,
  xbar.x=x, 
  xbar.y=y,    
  ybar.x=x, 
  ybar.y=y, 
  xlength=NA, 
  ylength=NA, 
  xunit="", 
  yunit="", 
  xlabel=NA, 
  ylabel=NA, 
  xfactor=1, 
  yfactor=1,
  xlim = NULL, 
  ylim = NULL,
  expand.x=c(0.05,0,0.05,0),
  expand.y=c(0.05,0,0.05,0),
  expand = TRUE,
  clip="on", 
  top = waiver(), 
  left = "bar", 
  bottom = "bar", 
  right = waiver(), 
  gap = 0.01,
  size=12,
  lwd=1.2,
  xlab.pos=c("bottom", "top"),
  ylab.pos=c("left", "right")
){
  
  
   
    
    list(
       theme_scalebars(
            size     = size    ,
            lwd      = lwd     ,
            xlab.pos = xlab.pos,
            ylab.pos = ylab.pos
          ),
       coord_scalebars(
         x      =x,
         y      =y,
         xbar.x =xbar.x ,
         xbar.y =xbar.y ,  
         ybar.x =ybar.x ,
         ybar.y =ybar.y ,
         xlength=xlength,
         ylength=ylength,
         xunit  =xunit  ,
         yunit  =yunit  ,
         xlabel =xlabel ,
         ylabel =ylabel ,
         xfactor=xfactor,
         yfactor=yfactor,
         xlim   =xlim   ,
         ylim   =ylim   ,
         expand.x=expand.x,
         expand.y=expand.y,
         expand =expand ,
         clip   =clip   ,
         top    =top    , 
         left   =left   ,
         bottom =bottom ,
         right  =right  , 
         gap    =gap 
       )
    )
  

}
