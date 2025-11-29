#' Add bar markers to a ggsweeps plot
#' 
#' add_bar is meant to be called before ggsweeps. It has to be used together with auto_bars, which will be called 
#' after ggsweeps to draw the bars (see example).
#'
#' @param ephysdata a tibble with ephys data
#' @param name a unique name for the bar. 
#' @param line defines the height where the bar is drawn
#' @param fixed.y (optional) fixed y position (ranging from 0=top to 1=bottom). If set, line is ignored. 
#' @param sweeps a numeric vector specifying for which sweeps the bar should be drawn, or a string "all", "first", or "last"
#' @param label.sweeps a numeric vector specifying for which sweeps the label should be drawn, or a string "all", "first", or "last"
#' @param label optionally specify label if different from name
#' @param label.x x-coordinate for the label
#' @param fill fill color of the bar
#' @param border border color of the bar
#' @param label.col label color of the bar 
#' @param label.size label size of the bar  
#' @param hjust,vjust position adjustment for the label of the bar
#' @param ... further parameters are ignored
#' @param start 
#' @param end 
#' 
#' @export
#'
#' @examples
#' 
#' read_ROBOO(ephysdata::examplefile("OO_GABA")) %>%  head(3) %>%   
#' add_bar(name = "GABA",                        start=35,end=95, line=2, sweeps=list(0,1,2)) %>%
#'   add_bar(name = "EFX",                         start=00,end=95, line=1, sweeps=1          ) %>%
#'   add_bar(name = "EFX2",  label="", fill="orange", start=0, end=20, line=1, sweeps=list(0,2  )) %>%
#'   add_cursor_point("peak", 75,95,min) %>%
#'   ggsweeps + 
#'   facet_wrap(~swp)  + 
#'  scale_y_continuous(expand = expansion(mult = c(0.01, .11))) 
#' 
#' 
add_bar <-
  function(ephysdata,
           name,
           start,
           end,
           line = 1,
           fixed.y = NA,
           sweeps = "all",
           label.sweeps = sweeps,
           label = name,
           bar.mapping=NA,
           label.x = {{start}} + 0.5 * ({{end}} - {{start}}),
           fill = NA,
           border = {{fill}},
           label.col = "black",
           label.size=10,
           hjust = 0.5,
           vjust = 0.5,
           ...) {
    assertthat::assert_that(
      !missing(start), !missing(end), 
      msg=glue::glue("error in add_bar(name= {name}... : please provide start and end values for the bar "))
    
    assertthat::assert_that(
      "ptrs" %in% names(ephysdata), 
      msg="add_bar requires a sepecial column named 'ptrs' in your data set, which is missing"
    )
    
    if(is.na(fixed.y)){
      fixed.y=0
    }else{
      line=0
    }
    
    # helper function that safely tests if a symbol exists and is NA. 
    # returns FALSE if the symbol either is assigned to a variable that is not NA, or essems to not exist. 
    # the latter case happens if it is a symbol to be resolved by NSE later. 
    is_NA <- purrr::possibly( is.na, otherwise = FALSE, quiet = T)
    # this gets us a warning if we test it more than once. ALso see:
    # https://stackoverflow.com/questions/20596902/r-avoiding-restarting-interrupted-promise-evaluation-warning
    
    is_NA_fill<-is_NA({{fill}})
    
    # make behaviour consistant with older versions:
    if(is_NA(bar.mapping) & is_NA_fill){
      #if(is.na(border)) border<- "grey"
      fill<-"grey"
    } 
    
    
    name2=paste0(name, ".bar")
    
    # helper function to unwrap the fill argument which was wrapped by from aes() to have a consistenant interface
    get_fill<-function(mapping){
      
      
      if(!is_NA_fill) return(NA)
      
      if(is.na(mapping)) return (NA)
      
      value= mapping[["fill"]]
      if(is.null(value)){
        NA
      }else{
        rlang::quo_get_expr(value)
      }
    }
    #bar.mapping=rlang::quo_get_expr(aes(fill=test)[["fill"]])
    bar.mapping=get_fill(bar.mapping)
    
    ephysdata<-ephysdata %>%
      rowwise %>% 
      mutate(  !!name2  := list( bar_(.data, start={{start}}, 
                                      end={{end}},
                                      line={{line}},
                                      fixed.y={{fixed.y}},
                                      sweeps={{sweeps}}, 
                                      label.sweeps={{label.sweeps}}, 
                                      label={{label}}, 
                                      label.col={{label.col}},
                                      label.size={{label.size}},
                                      
                                      #bar.mapping=rlang::quo_get_expr(aes(fill=test)[["fill"]]), # Input must be a vector, not a symbol.
                                      #bar.mapping=aes(fill=test)[["fill"]],                     # must be a vector, not a quosure
                                      #bar.mapping={{aes(fill=test)[["fill"]]}}, #arg must be a symbol
                                      #bar.mapping={{rlang::quo_get_expr(aes(fill=test)[["fill"]])}},  #arg must be a symbol
                                      bar.mapping={{bar.mapping}}, # works with direct arg (not wrapped in aes()),
                                      #bar.mapping=list(bar.mapping), # strategy where we disnetangle later
                                      fill={{fill}},
                                      border={{border}}, 
                                      hjust={{hjust}}, 
                                      vjust={{vjust}},
                                      label.x={{label.x}},
                                      ...) ),
               .after=id
      ) 
    
  }