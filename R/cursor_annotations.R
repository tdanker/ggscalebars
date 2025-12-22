#' Cursor annotations 
#'
#' geoms which help to visualise curso results in a ggsweeps plot. 
#' 
#' @name cursorannotations
NULL

#' @describeIn cursorannotations
#' show range of cursor and point in a ggplot -customizeable
#' @export
annot_range_point <- function(range.color = alpha("black", 0.2),
                              point.color = "blue",
                              point.shape = 21,
                              point.fill = NA,
                              point.size = 3,
                              point.stroke = 1) {
  function(name) {
    list(
      geom_cursor_range_(name, col = range.color),
      geom_cursor_point_(
        name,
        size = point.size,
        color = point.color,
        shape = point.shape,
        fill = point.fill,
        stroke = point.stroke
      )
    )
    
  }
}

#' @describeIn cursorannotations
#' show range of cursor and point in a ggplot -customizeable
#' @export
annot_range_model <- function(range.color = alpha("black", 0.2),
                              range.fill = alpha("black", 0.1),
                              line.color="red",
                              line.width=1
                              ) {
  function(name) {
    list(
      geom_cursor_model_range_(name, col = range.color, fill = range.fill),
      geom_cursor_model_predict(
        name,
        linewidth = line.width,
        color = line.color
      )
    )
    
  }
}



#' @describeIn cursorannotations
#' show range of cursor in a ggplot
#' @export
geom_cursor_range_ <- function(name, fill=alpha("grey70", .05) , col="grey70", linetype=5){  
  name<-paste0(name, ".csr")
  list(
    geom_rect(aes(xmin=st, xmax=en, ymin=-Inf, ymax=Inf), inherit.aes = FALSE,   data= . %>% get_data({{name}}, select=c("st", "en")), fill=fill),
    
    geom_vline(aes(xintercept=st),  data= . %>% get_data({{name}}, select=c("st", "en"))  , color=col, linetype=linetype ) ,
    geom_vline(aes(xintercept=en),  data= . %>% get_data({{name}}, select=c("st", "en")) , color=col, linetype=linetype ) 
    
   
    
  )}


geom_cursor_model_range_ <- function(name, fill=alpha("grey70", .05) , col="grey70", linetype=5){  
  name<-paste0(name, ".csr")
  list(
    geom_rect(aes(xmin=st+x0, xmax=en+x0, ymin=-Inf, ymax=Inf), inherit.aes = FALSE,   data= . %>% get_data({{name}}, select=c("st", "en", "x0")), fill=fill),
    
    geom_vline(aes(xintercept=st+x0),  data= . %>% get_data({{name}}, select=c("st", "en", "x0"))  , color=col, linetype=linetype ) ,
    geom_vline(aes(xintercept=en+x0),  data= . %>% get_data({{name}}, select=c("st", "en", "x0")) , color=col, linetype=linetype ) 
    
    
    
  )}




get_data<-function(df, name, st=st, en=en, select=NA){
  
  if(! is.null(attr(df, "meta"))){
    df<-attr(df, "meta")
   # print("yea!")
  } 
   
  df %>% select(- any_of(c("x", "y"))) %>% distinct %>% tidyr::unnest_wider({{ name }}) ->df
  
  if("model" %in% names(df)){
    df %>%  apply_model_pred({{st}},{{en}}) ->df
  }
  
  df %>% 
    tidyr::unnest(any_of(c("x", "y"))) %>% #this is needed for cursor_points (plural)
    apply_offsets -> df
  
  if(!all(is.na(select))){
    df %>% ungroup %>%
        select(all_of(select)) -> df  
  }
  
  df %>%
    distinct
}



apply_model_pred <-function(df, st, en){
  pred<-function(model, st, en){
    predict(model[[1]], data.frame(x=seq(st,en, length.out = 20)))
  }
  
  pred_<- purrr::possibly(pred , otherwise = rep_len(NA_real_, length.out = 20))
  
  df %>% rowwise %>% 
  mutate(
    pred=list(
      data.frame(
        x=seq({{st}},{{en}}, length.out = 20), 
        y=pred_(model, {{st}},{{en}} )
      )
    )
  ) %>%  
  tidyr::unnest(pred)}


 apply_offsets <- 
   . %>%
   #mutate(st=st+xoffset, en=en+xoffset) %>% 
   mutate(across(any_of(c("x", "st", "en")), \(x) x+xoffset)) %>% 
   mutate(across(any_of("y"), \(y) y+yoffset)) 
   


#' @describeIn cursorannotations
#' AP
#' @export
geom_cursor_AP_ <- function(name){  
  name<-paste0(name, ".csr")
  list(
    
    #geom_vline(aes(xintercept=start),  data= .%>% tidyr::unnest_wider({{ name }}) %>% distinct, color=col, linetype=linetype ) ,
    #geom_vline(aes(xintercept=end),  data= .%>% tidyr::unnest_wider({{ name }}) %>% distinct, color=col, linetype=linetype ) ,
    geom_segment(aes(x=.onset_position, y=peak, xend=.onset_position, yend=Vm),  data= .%>% tidyr::unnest_wider({{ name }}) %>% distinct, col="red", lty=3),
    geom_segment(aes(x=.onset_position, y=.level_30, xend=.APD30_end, yend=.level_30),  data= .%>% tidyr::unnest_wider({{ name }}) %>% distinct, col="red", lty=3),
    geom_segment(aes(x=.onset_position, y=.level_50, xend=.APD50_end, yend=.level_50),  data= .%>% tidyr::unnest_wider({{ name }}) %>% distinct, col="red", lty=3),
    geom_segment(aes(x=.onset_position, y=.level_90, xend=.APD90_end, yend=.level_90),  data= .%>% tidyr::unnest_wider({{ name }}) %>% distinct, col="red", lty=3),
    geom_segment(aes(x=start, y=Vm, xend=end, yend=Vm),  data= .%>% tidyr::unnest_wider({{ name }}) %>% distinct, col="blue", lty=2)
    
  )}


#' @describeIn cursorannotations
#' show name of cursor as label
#' @export
geom_cursor_label_ <- function(name, ..., filter=TRUE){ 
  name<-paste0(name, ".csr")
  list(
    geom_text(aes(label=name),  data= .%>% tidyr::unnest_wider({{ name }}) %>% tidyr::unnest(c(x,y)  ) %>% dplyr::filter({{filter}}),... )
  )
}

#' @describeIn cursorannotations
#' show label on given y position (e.g. on top of plot)
#' @export
geom_cursor_toplabel_ <- function(name, toplevel, ...){ 
  .name<-paste0(name, ".csr")
  list(
    geom_text(aes(label=name, y=.toplevel, x=st+(en-st)/2),  
              data= .%>% tidyr::unnest_wider({{ .name }}) %>% rowwise %>% mutate(.toplevel={{toplevel}}),... )
  )
}


#' @describeIn cursorannotations
#' show bar across cursor range 
#' @export
geom_cursor_xbar_ <- function(name, toplevel, ...){ 
  name<-paste0(name, ".csr")
  list(
    geom_segment(aes(x=st, xend=en, y=.toplevel, yend=.toplevel),  
                 data= .%>% tidyr::unnest_wider({{ name }}) %>% rowwise %>% mutate(.toplevel={{toplevel}})  ,... )
  )
}


#' @describeIn cursorannotations
#' show point cursor results in a ggplot
#' @export
geom_cursor_point_ <- function(name, ...){ 
  name<-paste0(name, ".csr")
  list(
    geom_point( data= . %>% get_data({{name}}), na.rm=T
                  , ... )
  )
}

#' @describeIn cursorannotations
#'  show level cursor results in a ggplot
#' @export
geom_cursor_hline_ <- function(name,...){  
  name<-paste0(name, ".csr")
  list(
    geom_segment(aes(.data$st,.data$y, xend=.data$en, yend=.data$y),
                 data= . %>% get_data({{name}}), na.rm=T
                 , ... ) 
      
    
  )
}

#' @describeIn cursorannotations
#' show extrapolated level cursor results in a ggplot
#' @export
geom_cursor_hline_extended <- function(name, x1, x2,  ...){ 
  name<-paste0(name, ".csr")
  list(
 geom_segment(aes(x1,y, xend=x2, yend=y), na.rm=T,
              data= .%>% tidyr::unnest_wider({{ name }})  %>% tidyr::unnest(y) %>% 
                mutate(st=st+xoffset, en=en+xoffset, y=y+yoffset)) , ...
 
)}

#' @describeIn cursorannotations
#' show predicted values of a mode in a ggplot
#' @export
geom_cursor_model_predict <- function(name, linewidth=2, ...){ 
  name<-paste0(name, ".csr")
  pred<-function(model, st, en){
    predict(model[[1]], data.frame(x=seq(st,en, length.out = 20)))
  }
  pred_<- purrr::possibly(pred , otherwise = rep_len(NA_real_, length.out = 20))
  
  list(
    geom_line( 
      data= .%>% get_data({{ name }}) %>% 
        rowwise %>% mutate(pred=list(data.frame(.x=seq(st2,en2, length.out = 20)-x0, 
                                                .y=pred_(model, st2-x0, en2-x0 )
        ))) %>%  tidyr::unnest(pred)%>% mutate(x=.x+xoffset+x0, y=.y+yoffset)
      , linetype=5, na.rm=TRUE, ... ),
    geom_line( 
      data= .%>% get_data({{ name }})%>% 
        rowwise %>% mutate(pred=list(data.frame(
          
          # while st2 and en2 are not affected by xoffset, 
          # st and en are recalculated in the cursor and thus have to be corrected for xoffset
          .x=seq(
            st-xoffset, 
            en-xoffset, 
            length.out = 20
            ), 
          
          .y=pred_(model, 
            st-xoffset, 
            en-xoffset
            )
          
          ))) %>%  tidyr::unnest(pred)%>% mutate(
          x=.x+xoffset+x0,  
          y=.y+yoffset)
      , linewidth=linewidth, na.rm=TRUE, ... )
  )
}

#' @describeIn cursorannotations
#' a bar annotation
#' @export
bar_.annot <- function(name, color="blue"){
  list(
    #geom_cursor_range_(name, col = alpha("black", 0.2)),  
    #geom_cursor_point_(name, size=2, color=color) 
  )
}


#' @describeIn cursorannotations
#' a combination of range and point annotation
#' @export
point_.annot <- function(name, color="blue"){
  list(
    geom_cursor_range_(name, col = alpha("black", 0.2)),  
    geom_cursor_point_(name, size=2, color=color) 
  )
}

#' @describeIn cursorannotations
#' just simple point annotation
#' @export
point_.annot_simple <- function(name, color="blue"){
  list(
    #geom_cursor_range_(name, col = alpha("black", 0.2)),  
    geom_cursor_point_(name, size=2, color=color) 
  )
}


#' @describeIn cursorannotations
#' a combination of range and level annotation
#' @export
level_.annot <- function(name){
  list(
    geom_cursor_range_(name, col = alpha("black", 0.2)),  
    geom_cursor_hline_(name) 
  )
}

#' @describeIn cursorannotations
#' a combination of range and model_predict annotation
#' @export
model_.annot <- function(name){
  list(
    geom_cursor_model_range_(name, col = alpha("black", 0.2)),  
    geom_cursor_model_predict(name) 
  )
}

#' @describeIn cursorannotations
#' a combination of range and model_predict annotation
#' @export
peaks_multy_.annot <- function(name){
  list(
    geom_cursor_point_(name, col = alpha("red", 1))  
  )
}


