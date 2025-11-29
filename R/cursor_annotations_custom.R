#' customize point annotations
#'
#' customize the appearence of the annotations of  \code{add_cursor_point}
#' 
#' @param point.color point color
#' @param range.color color of the borders of the cursor range
#' @param range.fill.color fill color of the cursor range
#'
#' @return a cursor annotation
#' @export
#'
#' @examples
#' read_PATCHMASTER( ephysdata::examplefile("NaV"), ser=2) %>% slice(6) %>%
#'add_cursor_point("test", start = 0.01, end = 0.013, fun=min, 
#'                 annot = point_annotation( 
#'                   point.color="orange", 
#'                   range.color = "grey")
#')	%>% ggsweeps()
point_annotation<-function(point.color="orange", range.color=point.color, range.fill.color=range.color){
  function(name){
    list(
      geom_cursor_range_(name, col = alpha( range.color,.2), fill=alpha(range.fill.color, 0.05)),  
      geom_cursor_point_(name, size=2, color=point.color) 
    )
  }
}
