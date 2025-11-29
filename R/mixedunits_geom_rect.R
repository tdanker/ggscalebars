geom_rect. <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA, 
                          inherit.aes = FALSE, x=Inf, y=Inf, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, ...) {
  layer(
    geom = GeomRect. , mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,  xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, ...)
  )
}

GeomRect. <- ggplot2::ggproto("GeomrectMixedunits", ggplot2::GeomRect,
                                 
                                 required_aes =c(ggplot2::GeomRect$required_aes, "xmin.","ymin.", "xmax.", "ymax.")
                                   ,
                                 draw_panel = function(data , panel_params, coord) {
                                  

                                   if(!inherits(data$xmin.,     "character")) data$xmin  =data$xmin. 
                                   if(!inherits(data$ymin.,     "character")) data$ymin  =data$ymin.
                                   if(!inherits(data$xmax.,     "character")) data$xmax  =data$xmax.
                                   if(!inherits(data$ymax.,     "character")) data$ymax  =data$ymax.
                                   
                                   coords <- coord$transform(data , panel_params)
                                   
                                   
                                   if(inherits(data$ymin., "character")){ coords$ymin = as.numeric(data$ymin.)}
                                   if(inherits(data$ymax., "character")){ coords$ymax  = as.numeric(data$ymax.)}
                                   if(inherits(data$xmin., "character")){ coords$xmin  = as.numeric(data$xmin.)}
                                   if(inherits(data$xmax., "character")){ coords$xmax  = as.numeric(data$xmax.)}

                                   grid::rectGrob(
                                     coords$xmin,
                                     coords$ymin,
                                     coords$xmax-coords$xmin,
                                     coords$ymax-coords$ymin,
                                     hjust=0, vjust=0,
                                     gp = grid::gpar(col = coords$barborder, fill=coords$barfill)
                                   )
                                 }
)                           
                                
                                
 