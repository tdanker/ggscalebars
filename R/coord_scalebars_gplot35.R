#' Cartesian coordinates with scalebars instead of axes
#' 
#' One or both axes can be replaced by scalebars. 
#' 
#' By default, both axes are replaced by scalebars originating at the lower-left corner of the plot. 
#' 
#' Scalebars are typically drawn in the axis areas, 
#' but can also be put inside the panel area using one of the positioning parameters.
#' 
#' Appearence of the scalebars is governed by the active theme, using axis.line for the bar and axis.text for the label. 
#' It is mandatory that at least axis.line is set in the theme. 
#' \link{theme_scalebars} offers a convenient way to tweak the appearance of the scalebars.
#' \link{scalebars} provides a conveniant wrapper funtion that applies the theme automatically. 
#'
#' @param x,y set position of both scalebars, in npc coordinates (0 = bottom|left, 1 = top|right).  
#' @param xbar.x,xbar.y,ybar.x,ybar.y optionally, set position for a scalebar individually 
#' @param xlength,ylength optionally set length of scalebar, in data coordinates. If zero, the bar label is empty by default.  
#' @param xunit,yunit optionally add a unit to a scalebar label
#' @param xlabel,ylabel manually override any scalebar label 
#' @param xfactor,yfactor optional factor for the automatic generated labels
#' @inheritParams ggplot2::coord_cartesian 
#'
#' @export
#'
# a list of a coord_Cartesian and optionally the outer scalebars. 
# As with the gg344 system, we add a Coord, which also draws the inner scalebars if requested
# outer scalebars are added now via a "capped" scale_y_continuous, a new feature of the gg355 system 
coord_scalebars <- function (
  x=0,y=0,
  xbar.x=x, xbar.y=y,    
  ybar.x=x, ybar.y=y, 
  xlength=NA, ylength=NA, 
  xunit="", yunit="", 
  xlabel=NA, ylabel=NA, 
  xfactor=1,  yfactor=1,
  xlim = NULL, ylim = NULL, 
  expand.x=c(0,0,0,0), expand.y=c(0,0,0,0),
  expand = TRUE,
  clip="on", 
  top = waiver(), 
  left = "bar", 
  bottom = "bar", 
  right = waiver(), 
  gap = 0.01) 
{
  
  # provide an easy option to switch off x or y bar entirely: 
  if(isTRUE(xlength==0) & is.na(xlabel)) xlabel=""
  if(isTRUE(ylength==0) & is.na(ylabel)) ylabel=""
  
  colour=NULL
  y_inner=F
  x_inner=F
  
  if (is.character(top)){
    if (top == "bar")
      if (xbar.y == 0) {
        top <-list(
          
          scale_x_continuous( guide = guide_axis(cap = "both", title = ""),                           # add manual scalebar
                              limits=xlim,
                                       position = "top",
                              breaks = .breaks(xbar.x, xlength,xlim, expand.x,xfactor), labels=.labels(xlength,xfactor, xunit), expand = expand.x),
          theme(axis.ticks.x =  element_blank(), axis.line = element_line())
          
        )
      } else{
        top <- scale_x_continuous( guide = NULL)
        x_inner = T
      } else{
        top <-  list(
          #scale_x_continuous( guide = guide_axis(cap = top, title = ""))
        )
      }
  } else(
    top <- list()
  ) 
   
  
  if (is.character(bottom)){
    if (bottom == "bar")
      if (xbar.y == 0) {
        bottom <-list(
          
          scale_x_continuous( guide = guide_axis(cap = "both", title = ""),                           # add manual scalebar
                              limits=xlim,
                              breaks = .breaks(xbar.x, xlength,xlim, expand.x, xfactor), labels=.labels(xlength,xfactor, xunit),expand = expand.x),
          theme(axis.ticks.x =  element_blank(), axis.line = element_line())
          
        )
      } else{
        bottom <- scale_x_continuous( guide = NULL)
        x_inner = T
    } else{
        bottom <-  list(
          scale_x_continuous( guide = guide_axis(cap = bottom, title = ""))
        )
    }
  }
  
  if (is.character(left)){
    if (left == "bar") {
      if (ybar.x == 0) {
        
        if(is.na(ylabel)){
          ylabel <- stringr::str_trim(paste(ylength*yfactor, yunit))
        }
        
        left <- list(
          
          scale_y_continuous( guide = guide_axis(cap = "both", title = ""),                           # add manual scalebar
                              breaks = .breaks(ybar.y, ylength,ylim, expand.y,yfactor), labels=.labels(ylength,yfactor, yunit), limits=ylim,expand = expand.y),
          theme(axis.ticks.y =  element_blank(), axis.line = element_line())
          
        )
          
      } else{
        left <- scale_y_continuous( guide = NULL)
        y_inner = T
      }
    } else{
     
      
      left <- list(
        scale_y_continuous( guide = guide_axis(cap = left, title = ""))
      )
    }
  }
      
    
 
  if (is.character(right)){
    if (right == "bar") {
      if (ybar.x == 0) {
        
        if(is.na(ylabel)){
          ylabel <- stringr::str_trim(paste(ylength*yfactor, yunit))
        }
        
        right <- list(
          
          scale_y_continuous( guide = guide_axis(cap = "both", title = ""), position="right",                          # add manual scalebar
                              breaks = .breaks(ybar.y, ylength,ylim, expand.y,yfactor), labels=.labels(ylength,yfactor, yunit), limits=ylim,expand = expand.y),
          theme(axis.ticks.y =  element_blank(), axis.line = element_line())
          
        )
        
      } else{
        right <- scale_y_continuous( guide = NULL)
        y_inner = T
      }
    } else{
      
      
      right <- list(
        #scale_y_continuous( guide = guide_axis(cap = right, title = ""))
      )
    }
  }else(
    right <- list()
  ) 
    
  
  
          
  list(
    
  
  ggproto(NULL, ggplot2::CoordCartesian,

          limits = list(x = NULL, y = NULL),
          top=top,
          bottom=bottom,
          left=left, y_inner=y_inner,
          right=right,
          expand = expand,
          default = FALSE,
          clip = clip,
          render_fg = function(panel_params, theme) {

            xrange_ <- CoordCartesian$range(panel_params)$x
            xrange<-diff(xrange_)

            theme_ <- theme
            if(is.null(theme$axis.line) && is.null(theme$axis.line.x)) stop("coord_scalebar: axis.line.x not set in theme")
            if(is.null(theme$axis.line) && is.null(theme$axis.line.y)) stop("coord_scalebar: axis.line.y not set in theme")
            if(inherits(theme_$axis.line, "element_blank") && is.null(theme$axis.line.x)) warning("coord_scalebar: axis.line.x not set in theme")
            if(inherits(theme_$axis.line, "element_blank") && is.null(theme$axis.line.y)) warning("coord_scalebar: axis.line.y not set in theme")
            if(inherits(theme_$axis.line, "element_blank") && inherits(theme_$axis.line.x, "element_blank")) warning("coord_scalebar: axis.line.x is element_blank")
            if(inherits(theme_$axis.line, "element_blank") && inherits(theme_$axis.line.y, "element_blank")) warning("coord_scalebar: axis.line.y is element_blank")



            if(is.na(xlength)){
              #print(xrange_)
              xlength <- auto_bar.length(xrange_*xfactor)/xfactor
              #print(xlength)
            }
            if(is.na(xlabel)){
              xlabel <- stringr::str_trim(paste(xlength*xfactor, xunit))
            }

            xbar.x[2]<-xbar.x[1]+xlength/xrange
            xbar.y[2]<-xbar.y

            yrange_<- CoordCartesian$range(panel_params)$y
            yrange<-diff(yrange_)
            #ybar.y=y[1]
            #ybar.x=x[1]

            if(is.na(ylength)){
              ylength <- auto_bar.length(yrange_*yfactor)/xfactor
            }
            if(is.na(ylabel)){
              ylabel <- stringr::str_trim(paste(ylength*yfactor, yunit))
            }

            ybar.y[2]<-ybar.y[1]+ylength/yrange
            ybar.x[2]<-ybar.x
            yticks=theme$axis.ticks.length

            if(x_inner){
              inner_bars_x <- grid::gList(
                element_render(theme, "axis.line.x", x=xbar.x, y=xbar.y, colour=colour),
                element_render(theme, "axis.text.x", label=xlabel, x=xbar.x[1]+diff(xbar.x)/2, y=grid::unit(xbar.y[1], "npc")-yticks, colour=colour)
              )
            }else{
              inner_bars_x <- NULL
            }


            if(y_inner){
              inner_bars_y <- grid::gList(
                element_render(theme, "axis.line.y", x=ybar.x, y=ybar.y, colour=colour),
                element_render(theme, "axis.text.y", label=ylabel, y=ybar.y[1]+diff(ybar.y)/2, x=grid::unit(ybar.x[1], "npc")-yticks, colour=colour)
              )
            }else{
              inner_bars_y <- NULL
            }

            grid::gList(
             inner_bars_x,
             inner_bars_y
            )

          }

  ),
  left, 
  bottom, 
  top,
  right
  )
  
}


.breaks=function(ystart, ylength,limits, expan, xfactor){
  
  function(y){
    #print(paste("ystart:",ystart))
    if(is.null(limits)){
      
      yrange=range(y)
    }else{
     
      yrange=limits
    }
 
    if(is.na(ylength)){
      ylength <- auto_bar.length(c(0, yrange)*xfactor)/xfactor
    }
    
    
    ywidth<-diff(yrange)
    if(!is.null(limits)){
      yrange[1]<-yrange[1] - ywidth*expan[1] # mult left
      yrange[1]<-yrange[1] -        expan[2] # add left
      #yrange[1]<-yrange[1] + ywidth*.01
      yrange[2]<-yrange[2] + ywidth*expan[3] # mult right
      yrange[2]<-yrange[2] +        expan[4] # add right
    }
    
    
    breaks=c(yrange[1]+diff(yrange)*ystart, yrange[1]+diff(yrange)*ystart+ylength/2, yrange[1]+diff(yrange)*ystart+ylength)
    #print(breaks)
    breaks
  }
}

.labels=function(ylength,yfactor, yunit){
  
  function(breaks){
    #print(breaks)
    ylabel <- stringr::str_trim(paste(diff(range(breaks))*yfactor, yunit))
    c("", ylabel, "")
  }
}


auto_bar.length <- function(range){
  
  round(diff(pretty(range, n = 7)[1:2]),2)
} 


