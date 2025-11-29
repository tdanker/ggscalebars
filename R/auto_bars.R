# all calculations here
get_xBars <- function(df, space=.035, height=.03, topspace=space){
  
  if(NCOL( df %>% select(contains(".bar"))) ==0) 
    stop("you are trying to plot bars, but there are no bars defined. 
         may be you want to define a bar first or remove 'auto_bars()' ?
         type '? add_bar' for help on how to work with bars")
  
 bars<-   
  df %>% 

    tidyr::pivot_longer(contains(".bar")) %>% tidyr::unnest_wider(value) %>%
    dplyr::mutate( bar=fill %>% stringr::str_remove(".bar"),
            #line=as.numeric(factor(name))-1, 
            label=label, 
            ymax=    as.character(-1*{{topspace}} + 1- {{height}}    - (line-1)*({{height}}+{{space}}) - fixed.y), 
            ymin=    as.character(-1*{{topspace}} + 1            - (line-1)*({{height}}+{{space}}) - fixed.y), 
            y_label= as.character(-1*{{topspace}} + 1- {{height}}*.4 - (line-1)*({{height}}+{{space}}) - fixed.y),
    ) %>% dplyr::group_by(name) %>%

    # mutate(      sweeps= ifelse(      sweeps== "first",  list(as.numeric(   (swp)[1])),list(      sweeps)))  %>%
    # mutate(label.sweeps= ifelse(label.sweeps== "first",  list(as.numeric(   (swp)[1])),list(label.sweeps)))  %>%
    # mutate(      sweeps= ifelse(      sweeps== "last",   list(as.numeric(rev(swp)[1])),list(      sweeps)))  %>%
    # mutate(label.sweeps= ifelse(label.sweeps== "last",   list(as.numeric(rev(swp)[1])),list(label.sweeps)))  %>%
    # mutate(      sweeps= ifelse(      sweeps== "all",    list(as.numeric(    swp    )),list(      sweeps)))  %>%
    # mutate(label.sweeps= ifelse(label.sweeps== "all",    list(as.numeric(    swp    )),list(label.sweeps)))  %>%
    mutate(      sweeps = case_when(
            sweeps=="first" ~ list(as.numeric(   (swp)[1])),
            sweeps=="last"  ~ list(as.numeric(rev(swp)[1])),
            sweeps=="all"   ~ list(as.numeric(    swp    )),
      TRUE ~ list(sweeps)
    )) %>% 
   mutate( label.sweeps = case_when(
     label.sweeps=="first" ~ list(as.numeric(   (swp)[1])),
     label.sweeps=="last"  ~ list(as.numeric(rev(swp)[1])),
     label.sweeps=="all"   ~ list(as.numeric(    swp    )),
     TRUE ~ list(label.sweeps)
   )) %>%

    mutate(label= ifelse(swp %in% unlist(label.sweeps), label, "")) %>%
    dplyr::filter(swp %in% unlist(sweeps))  %>% mutate(st=st+as.numeric(xoffset), en=en+as.numeric(xoffset), label.x=label.x+as.numeric(xoffset))
  
  if(inherits(bars$label.x, "list")) rlang::warn("dont mix strings and numbers for x.label!")
  
  bars
}     


# this does not calculate anything, called from inside ggsweeps
auto_bars <- function(space=.01, height=.05, topspace=space, show.legend=FALSE, colors="grey85"){
  
  list(
    geom_rect..(#..version
      
      aes(xmin.=st, xmax.=en, 
                   ymax.=ymax,
                   ymin.=ymin,
                   barborder=border,
                   fill=bars, #aes_fill, #fill, #rect.. uses fill instead of barfill
                   fill2=fill
                   ), 
                   show.legend=show.legend,
               data= . %>% attr("meta") %>% get_xBars( space=space, height = height, topspace=topspace) 
    ) ,  
    geom_text.(aes(
                   x.= unlist(label.x), 
                   y.= y_label,
                   label.col =label.col, 
                   label=label, 
                   label.size=label.size,
                   hjust=hjust, 
                   vjust=vjust), 
                   # control bar labels via options
                   # this is rather a hack
                   # if we would switch to draw bars in the coord we could use theme elements
                   # sounds like the best option...
                   #color=getOption("ephys4.bars.label.color"),  
               data= . %>% attr("meta") %>% get_xBars( space=space, height = height, topspace=topspace) 

    ),
    scale_fill_brewer() # replaces the default scale_fill_discrete, to avoid error "Must request at least one colour from a hue palette"
    )
}



