#' Make a ggDRC plot
#'
#' For more info, see \href{../doc/drc.html}{\code{vignette("Dose resoponse curves", package = "ephys4")}}
#'
#' @param df2         results from drc_fit
#' @param type        type of plot
#' @param bars        error bars on/off
#' @param means       means on/off
#' @param show_n      number of n  on/off
#' @param obs         observations on/off
#' @param vline       vertical line indicating the IC50value on/off
#' @param xlimits     xlimits
#' @param xlabels     xlabels
#' @param confidence  set to true to plot the confidence interval as a ribbon. 
#' @param confidence.alpha  the alpha value of the confidence ribbon 
#' @param ...         arguments are forwarded to geoms
#' @param point.size,line.size,bar.size style parameters 
#' @param show_IC50 if TRUE, the IC50 values will be shown in the legend
#' @param legend.labels optional, labels for the legends
#' @param drop_conc0 if TRUE (the default), no data point will be shown for the control condition ("0mM compound") 
#' @param colors optional, the colors to be used fot the curves
#'
#' @family drc methods
#'
#' @return a ggPlot obbject
#' @export
#'
#' @examples
#' read_PATCHMASTER(ephysdata::examplefile("hergDRC"),2) %>% 
#' add_cursor_point("peak", 2.28,2.3,max) %>% 
#'   drc_plan_HEKA(3)  -> tagged4drc
#' 
#' tagged4drc %>% filter(drc_sweep) %>% ggsweeps(mapping=aes(col=factor(conc))) 
#' 
#' tagged4drc  %>% 
#'   drc_get_lpresults_(peak,  normalize = TRUE) %>% drc_fit %>% drc_plot
#' 
#' 
#' 
#' 
drc_plot <- function(df2,
                     type=c( "all", "average", "bars", "none", "obs", "confidence")[1],
                     bars=type=="bars",
                     means=type=="bars" || type=="average",
                     obs=type=="all",
                     confidence=FALSE, 
                     confidence.alpha=0.2,
                     point.size=2,
                     line.size=1,
                     bar.size=line.size,
                     show_n=F,
                     show_IC50=F,
                     vline=F,
                     xlimits=c(.0001,100),
                     xlabels=scales::format_format(drop0trailing=TRUE, scientific = FALSE),
                     legend.labels=NULL,
                     drop_conc0=T,
                     colors=scales::hue_pal()(df2 %>% summarise(n=n()) %>% unlist),
                     ...
){
  
  # plot raw data, model and ED50 line
  observations<-function(x) data.frame(y=mean(x), n=length(x))

  IC50values= df2 %>% tidyr::unnest_legacy(coefs) %>% filter(names == "IC50:(Intercept)")

  p<- df2 %>%
    
    # make sure that colors are assigned in the same order as they appear in the input data frame
    # and not in alphabetical order (this is neccessary if IC50 values etc are added to the legend later)
    mutate(compound=factor(compound, levels=compound)) %>%
  
    tidyr::unnest_legacy(data) %>% group_by(concentration) %>% dplyr::mutate (nn=n()) %>%
    {if(drop_conc0) filter(., concentration!=0) else .} %>%
    ungroup %>% mutate(concentration=ifelse(concentration==0,xlimits[1], concentration)) %>%
    ggplot(aes(concentration, response, color = compound)) +
    scale_x_log10(limits=xlimits, labels = xlabels, minor_breaks=10^(-4:3)%o%(2:10))

  if(show_IC50)
    p <- p+ scale_colour_manual(values=colors,  labels=drc_labels(df2))
  if(!is.null(legend.labels))
    p <- p+ scale_colour_manual(values=colors, labels=legend.labels)

  if(isTRUE(obs))
    p <- p + geom_point( aes(concentration, response), size=point.size )

  p <- p +
    geom_line(aes(x, pred, color = compound), data =	df2 %>% tidyr::unnest_legacy(pred), na.rm=TRUE, size=line.size, ...)

  if(means)
    p <- p + geom_point( stat="summary", fun.data=mean_se, size=point.size,...)
  
  
  if(isTRUE(bars))
   
    p <- p + geom_errorbar( stat = "summary", fun.data=mean_se, width=0.15, size=bar.size)

  if(confidence){
    if(! ("Upper" %in% names(df2 %>% tidyr::unnest_legacy(pred))))
    {
      cli::cli_warn(
        c("no confidence intervals found in drc model", i="please use add_confidence=TRUE in your call to drc_fit when plotting confidence intervals")
      )
    }else{
    p <- p+ geom_ribbon(aes(x, pred, fill = compound, ymin=Lower, ymax=Upper), data =	df2 %>% tidyr::unnest_legacy(pred), color=NA, alpha=confidence.alpha, na.rm=TRUE, size=line.size, ...)
    
    }
  }
    
  if(show_n)
    p <- p + geom_text(aes(concentration, response, label=paste0("(",stat(n), ")"), color = compound), show.legend=F, stat="summary", fun.data=observations, nudge_x = .15, nudge_y = .02, size=3)

  if(vline)
    p <- p + geom_segment(aes( x=x, y=0.5, xend=x, yend=-Inf, color = compound),	linetype = 5, data = df2 %>% tidyr::unnest_legacy(coefs) %>% filter(names == "IC50:(Intercept)"))

  p
}

