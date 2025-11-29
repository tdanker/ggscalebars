






#' Title
#'
#' @param start 
#' @param end 
#' @param line 
#' @param style 
#' @param filter_expr condition when the bar should be drawn; mostly used in conjunction with facetting. 
#'
#' @export
#'
#' @examples
geom_topbar<-function(
             start,
             end,
             line = 1,
             #fixed.y = NA,
             #sweeps = "all",
             #label.sweeps = sweeps,
             label = "",
             
             line_to.x=start,
             line_to.x2=end,
             line_to.y=label.y,
             line_to.y2=label.y,
             line_to.color=fill,
             line_to.size=1,
             line_to.linetype=1,
             line_to.arrow=arrow(length=unit(0,"mm")),
             #bar.mapping=NA,
             label.x =label.xpos(label.position, start, end),
             label.y =label.ypos(label.position, line, style),
             label.position=c("center", "above", "below", "left", "right"),
             fill = "grey",
             border = {{fill}},
             label.col = fill,
             label.size=10,
             get_data=unfiltered,
             hjust = label.hjust(label.position),
             vjust = label.vjust(label.position),
             style=ggsweeps.defaultstyle,
             filter_expr=TRUE,
             ...) {#start, end, line=1, label.x=start + (end - start)/2, style=ggsweeps.defaultstyle,filter_expr=TRUE){
  
  list(
    geom_rect..(xmin.=start, xmax.=end, na.rm=T,
                ymin.=as.character(1-style$height*line-style$space*(line-1)-style$topspace),
                ymax.=as.character(1-style$height*(line-1)-style$space*(line-1)-style$topspace),
                data=. %>% get_data %>% filter({{filter_expr}}) %>%
                  head(1), # prevents overplotting multiple times
                fill=fill, size=1,
                color=border
                ),
    geom_text.(x.=label.x,
               y.=label.y, na.rm=T,
               label=label, label.col=label.col, label.size=label.size, vjust=vjust,hjust=hjust, show.legend=F,
               data=. %>% get_data %>% filter({{filter_expr}}) %>%
                 head(1) # prevents overplotting multiple times
               #data=NULL
               ),
    geom_segment.(x.=start, xend.=line_to.x, y.=label.y, yend.=line_to.y, color=line_to.color, size=line_to.size, arrow=line_to.arrow, linetype=line_to.linetype,na.rm=T,
                  data=. %>% get_data %>% filter({{filter_expr}}) %>% 
                    head(1)),
    
    geom_segment.(x.=end, xend.=line_to.x2, y.=label.y, yend.=line_to.y2, color=line_to.color, size=line_to.size, arrow=line_to.arrow, linetype=line_to.linetype,na.rm=T,
                  data=. %>% get_data %>% filter({{filter_expr}}) %>% 
                    head(1)),
    geom_blank(
      data=. %>% get_data %>% filter({{filter_expr}}) %>% mutate(...x=start), 
      stat="summary", 
      fun=fun_topspace(
           line   *style$height +
          (line+1)*style$space+
           style$bottomspace+
            
           style$topspace), na.rm=T,
      aes(x=...x) # this solves a problem: what if x is mapped to something else?
    ) 
  )
}


label.hjust<-function(position=c("center", "above", "below", "left", "right")){
  position=match.arg(position)
  #print(position)
  switch(position, center=0.5, above = 0.5, below=0.5, left=1, right=0)
}
label.vjust<-function(position=c("center", "above", "below", "left", "right")){
  position=match.arg(position)
  switch(position, center=0.4, above = -.2, below=1, left=0.4, right=0.4)
}

label.xpos<-function(position=c("center", "above", "below", "left", "right"), start, end){
  position=match.arg(position)
  switch(position, center=start+(end-start)/2, above = start+(end-start)/2, below=start+(end-start)/2, left=start, right=end)
}

label.ypos<-function(position=c("center", "above", "below", "left", "right"), line, style){
  position=match.arg(position)
  y=1-style$height*(line-1)-style$height/2-style$space*(line-1)-style$topspace
  y=switch(position, center=y, left=y, right=y, above=y+style$height/2, below=y-style$height/2)
  #print(position)
  as.character(y)
}

fun_topspace<-function( space_for_bars = .1){
  function(y){
    y=y[!is.na(y)]
    #print(theme_get()$legend.key.size)
    max(y, na.rm=T) + diff(range(y)) * (space_for_bars/(1-space_for_bars))    
    
  }
}


