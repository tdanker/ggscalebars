geom_text. <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, x=Inf, y=Inf, ...) {
  layer(
    geom = GeomText., mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, x=x, y=y, ...)
  )
}


GeomText. <- ggplot2::ggproto("GeomTextMixedunits", ggplot2::GeomText,
                              required_aes =c(ggplot2::GeomText$required_aes, "x.","y.", "label.col", "label.size"),
                             
                              
                              draw_panel = function(data , panel_params, coord) {
                                
                                if(!inherits(data$x., "character")) data$x   =data$x.
                                if(!inherits(data$y., "character")) data$y   =data$y.
                                
                                
                                coords <- coord$transform(data , panel_params)
                                
                                
                                if(inherits(data$x.   , "character")){ coords$x    <- as.numeric(data$x.   )}
                                if(inherits(data$y.   , "character")){ coords$y    <- as.numeric(data$y.   )}
                                
                                coords <- coords %>% dplyr::group_by(label,x,y,size, colour) %>% dplyr::filter(group==group[1])
                                grid::textGrob(
                                  coords$label,
                                  coords$x,
                                  coords$y,
                                  
                                  hjust=coords$hjust,
                                  
                                  vjust=coords$vjust, 
                                  
                                  gp = grid::gpar(col = coords$label.col, fontsize=coords$label.size )
                                )
                              }
)