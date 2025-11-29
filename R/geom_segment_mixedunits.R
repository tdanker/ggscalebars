

geom_segment. <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA, arrow = NULL, 
                          inherit.aes = TRUE, x=-Inf, xend=Inf, y=-Inf, yend=Inf, ...) {
  layer(
    geom = GeomSegment., mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, x=x, y=y, xend=xend, yend=yend, arrow = arrow,  ...)
  )
}

GeomSegment. <- ggplot2::ggproto("GeomSegmentMixedunits", ggplot2::GeomSegment,
                                 
                                 required_aes =c(ggplot2::GeomSegment$required_aes, "x.","y.", "xend.", "yend."),
                                 optional_aes =c("arrow"),
                                 draw_panel = function(data , panel_params, coord) {
                                   
                                   if(!inherits(data$x.,     "character")) data$x     =data$x. 
                                   if(!inherits(data$y.,     "character")) data$y     =data$y.
                                   if(!inherits(data$xend.,  "character")) data$xend  =data$xend.
                                   if(!inherits(data$yend.,  "character")) data$yend  =data$yend.
                                   
                                   coords <- coord$transform(data , panel_params)
                                   
                                   if(inherits(data$x.   , "character")){ coords$x    <- as.numeric(data$x.   )}
                                   if(inherits(data$y.   , "character")){ coords$y    <- as.numeric(data$y.   )}
                                   if(inherits(data$xend., "character")){ coords$xend <- as.numeric(data$xend.)}
                                   if(inherits(data$yend., "character")){ coords$yend <- as.numeric(data$yend.)}
                                   
                                   grid::segmentsGrob(
                                     
                                     coords$x,
                                     coords$y,
                                     coords$xend,
                                     coords$yend,
                                     arrow=coords$arrow,
                                     
                                     
                                     gp = grid::gpar(col = coords$colour, fill=coords$colour, lwd= coords$size, lineend="butt", lty=coords$linetype)
                                   )
                                 }
)