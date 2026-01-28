#' Themes for scalebars
#'
#' @param size font size of the label
#' @param lwd  line size or the bar
#' @param xlab.pos should the label be on top of or below the bar
#' @param ylab.pos should the label be left or right of the bar
#'
#' @return theme elements suitable to control the appearance of scalebars
#' @export
#'
theme_scalebars<-function(size=12,
                         lwd=1.2,
                         xlab.pos=c("bottom", "top"),
                         ylab.pos=c("left", "right"), base_theme=theme_bw
)
{

  xlab.pos=match.arg(xlab.pos)
  ylab.pos=match.arg(ylab.pos)
  x.vjust=1
  y.vjust=-.2

  if(xlab.pos=="top")
    x.vjust=3+size/2.5

  if(ylab.pos=="right")
    y.vjust=-6

  
    theme(panel.border=element_blank(),
          axis.line=element_line(linewidth=lwd, lineend = "square"),
          #axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.text.y = element_text(angle = 90, size=size, hjust=0.5, vjust=y.vjust),
          axis.text.x = element_text(vjust=x.vjust, size=size),
          axis.ticks.length = unit(.4,"lines")
    )
}



#' @describeIn theme_scalebars for horizontal bars
#' @export
theme_scalebar_h<-function(size=12,
                         lwd=1.5,
                         xlab.pos=c("bottom", "top")
)
{
  
  xlab.pos=match.arg(xlab.pos)
  x.vjust=1

  if(xlab.pos=="top")
    x.vjust=3+size/2.5
  


    theme(#panel.border=element_blank(),
          axis.line.x = element_line(linewidth=lwd, lineend = "square"),
          #axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(vjust=x.vjust, size=size),
          axis.ticks.length.x = unit(c(.4),"lines")
    )
}



#' @describeIn theme_scalebars for vertical bars
#' @export
theme_scalebar_v<-function(size=12,
                           lwd=1.5,
                           ylab.pos=c("bottom", "top")
)
{
  
  ylab.pos=match.arg(ylab.pos)
  y.vjust=2
  
  if(ylab.pos=="right")
    y.vjust=-6
  
  
  
  theme(#panel.border=element_blank(),
    axis.line.y = element_line(linewidth=lwd, lineend = "square"),
    #axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(angle = 90, size=size, hjust=0.5, vjust=y.vjust),
    axis.ticks.length.y = unit(c(.4),"lines")
  )
}


