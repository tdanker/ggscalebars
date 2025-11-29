library(stringr)
library(readr)
library(tidyr)
library(dplyr)
library(crayon)
# ------- user level -------------

get_applications_from_df<-function(df){
  df %>% group_by(run, swp) %>%
    mutate(application=loglines %>% xtract_pattern(">>>(.*)", 2), .before=id)  %>%
    mutate(bars=list(loglines %>% xtract_pattern(">>>(.*)", which="all")), .before=id) %>%
    group_by(run) %>% calculate_offsets("realtime", 0) %>%
    mutate(xend=lead(xoffset, default=Inf),.after=xoffset) %>% unnest(bars) %>%
    extract(bars, regex="([[:alnum:]]+)s: ([[:alnum:]]+) (.*)", into=c("opening_time","valve", "valve_content"),convert = T) %>%
    
    # we will allow a hastag on each valvecontent line. everything past the hashtag will be stripped off from valve content and placed in a seüarete "hastag" column. 
    # this may be used as a marker, for example which of the many applications is to be used for a certain analysis purpose, such as "REFERENCE", "BASELINE", "COMPOUND" etc. 
    separate(valve_content, into=c("valve_content", "hashtag"), sep="#", fill="right" )%>%
    
    
    group_by(id) %>%
    mutate(close_time=lead(opening_time) ,.after=opening_time) %>%
    mutate(close_time=if_else(!is.na(close_time),as.double(close_time),xend-xoffset))
}

get_applications<-function(p){
  p$data %>% attr("meta") %>% get_applications_from_df()
    
}



amend_valve_contents<-function(df, valves, new_valve_contents){
  
  if(length(valves)!=length(new_valve_contents))
    cli::cli_abort("in amend_valve_contents: valves and new_valve_contents habe to be vectors of the same length")
  
  function(df){
    df %>% rowwise %>% 
      mutate(valve_content = if_else(valve %in% valves, paste0("",new_valve_contents[which(valve==valves)]), valve_content))
  }
  
}


#' Annotate Roboocyte plots
#'
#' draws bars on a ggsweeps plot to show the applications, valve conent will apear in the legend
#' this is only for Roboocyte and makes some assumptions on how we annotate the logs, as described in "get_logs_per_sweeps()". 
#'
#'
#' @param p 
#'
#' @return an annotated ggplot
#' @export
#'
#' @examples
annotate_applications<-function(p){
  
 get_applications(p)  -> B_
   
 
  B_ %>% filter(valve!=1)->B_
  
 
  
   p +
      geom_segment(data=B_, aes(x=opening_time + xoffset, xend=close_time+xoffset, y=0, yend=0, color=valve_content), linewidth=2)+
      geom_segment(data=B_, aes(x=opening_time + xoffset, xend=opening_time+xoffset, y=0, yend=-Inf, color=valve_content), alpha=.5, linewidth=.1, lty=3)+
      geom_segment(data=B_, aes(x=close_time + xoffset, xend=close_time+xoffset, y=0, yend=-Inf, color=valve_content), alpha=.5, linewidth=.1, lty=3)+
      geom_rect(data=B_, aes(xmin=close_time + xoffset, xmax=opening_time+xoffset, ymin=0, ymax=-Inf, fill=valve_content), alpha=.05, inherit.aes = F)  +
      theme(legend.position = "bottom") + guides(fill=guide_legend(title=""),color=guide_legend(title=""))
  
}



# fixme: do we want to export hits function?
annotate_applications2<-function(p, colors=c("red", "orange", "blue"), style=geom_topbars.defaultstyle, modifyer=unfiltered ){
  # this version is *much* faster than using topbars
  # plus it does not need fancy "absolute" cooridnates
  # plot building example: 150ms instead of 250
  # printing example:      100ms instead of 400
  # -----------------------------------------------
  #                        250              650
  
  
  B_<-
    get_applications(p) %>% modifyer 
  
  
  
  B_ %>% filter(valve!=1)->B_
  
  # split up the components of valve_content for topbars
  B_%>% select(id, opening_time, close_time, xoffset, valve, valve_content) %>% 
    mutate(vc_components=str_split(valve_content, "\\+") ) %>% 
    unnest_longer(vc_components)%>%
    ungroup %>% 
    mutate(start=opening_time+xoffset, end=close_time + xoffset, .keep="unused") %>% 
    mutate( label=vc_components) %>% mutate(label=str_trim(label)) %>% ungroup %>% mutate(line= as.numeric(as.factor(label))) %>% 
    mutate(line=max(line)-line+1)-> B__
  
   p %>% annotate_df(B__ = B__)
}

annotate_df<-function(p, colors=c("red", "orange", "blue"), style=geom_topbars.defaultstyle, B__ , ylim=NA, label.size=3, label.size.valves=label.size, labels.spread=0.2){
  # like annotate from logs, but naunally supplying the bars
  
  # this version is *much* faster than using topbars
  # plus it does not need fancy "absolute" cooridnates
  # plot building example: 150ms instead of 250
  # printing example:      100ms instead of 400
  # -----------------------------------------------
  #                        250              650
  
  
  
  
  
  
  B__ %>% group_by(label, line) %>% summarise(x=max(end)) -> LABS 
  
  LABS$label.line <- LABS$line - (( mean(LABS$line)-LABS$line )*labels.spread)
  
  top=    max(p$data$y) + style$bottomspace
  if(!is.na(ylim[2])) top=ylim[2]
  bottom= min(p$data$y)
  if(!is.na(ylim[1])) bottom=ylim[1]
  yrange=top-bottom
  
  top = top + style$bottomspace * yrange
  
  xmax= max(p$data$x)
  
  h=yrange * style$height 
  L=yrange * (style$height +style$space) 
  
  Labs.shift=xmax*.01
  
  
  p  + 
    {if((!is.na(ylim[1])|!is.na(ylim[2]))) coord_cartesian(ylim=ylim)} +
    geom_rect(aes(xmin=start, xmax=end, ymin=top+line*L, ymax=top+line*L+h, fill=label), data=B__, inherit.aes = F, na.rm=T) + 
    geom_segment(aes(x=start, xend=start, y=top+line*L, yend=-Inf, color=label), data=B__, inherit.aes = F, lty=3, na.rm=T)+ 
    geom_segment(aes(x=end, xend=end, y=top+line*L, yend=-Inf, color=label), data=B__, inherit.aes = F, lty=3, na.rm=T)+
    geom_text(aes(x=start+(end-start)/2,y=top, label=valve), size=label.size.valves,  vjust=0, data=B__, inherit.aes = FALSE, na.rm=T)+
    geom_text(aes(x+Labs.shift,y=top+label.line*L+h/2 , label=label), hjust=0, vjust=.2, size=label.size,  data=LABS, inherit.aes = FALSE, na.rm=T) + 
    theme(legend.position = "none") + scale_fill_manual(values=colors)
  
}




# similar to annotate_applications, but using geom_topbars, per component of valve_content
# this assumes that the components of a valve-content are separated by " + ", like "GABA 10mM + His"
#' @export
annotate_valves<-function(p, line=4, colors=c("red", "orange", "blue"), style=geom_topbars.defaultstyle){
  
  
  get_applications(p)  -> B_
  
  
  #B_ %>% filter(valve!=1)->B_
  
  
  # make topbar infos for valves
  B_%>% select(id, opening_time, close_time, xoffset, valve, valve_content) %>% 
    mutate(vc_components=str_split(valve_content, "\\+") ) %>% 
    unnest_longer(vc_components)%>%
    ungroup %>% 
    mutate(start=opening_time+xoffset, end=close_time + xoffset, .keep="unused") %>% 
    mutate( label=valve) %>% mutate(label=str_trim(label)) %>% ungroup %>% mutate(line=.env$line) -> B_valves
  
  
  p   + geom_topbars(B_valves, colors=rep("transparent", line), style)
  
}






#' get the full log of one (or several ) runs of a plate.
#' output is via cat and colorised 
#' #' @export
logs_show<- function(Plate,       
                     from=-1,      
                     to=  from    
){
  get_oo_log_runs_df(Plate, 
                         from, 
                         to)
}









# get logs for each recording.
# this is very useful for automatic plotting. 
# this only works if 
#     - every oocyte starts with             "Recording oocyte in"
#     - every recording starts with          "*-"
#     - for later extraction of application information, each valve opening should
#        print a logline starting with  ">>>" (see logs_add_app family of functions)
#' @export
logs_per_sweep<-function(
    plate , #filename without the ending
    run=F, 
    recording.pattern="\\*-",
    oo.pattern ="Recording oocyte in",
    oo="..", 
    add_r2d=F, 
    
    HDIFF=1   # set to 0 in Summer!
){
  get_logs_per_run(plate) %>% 
    # count up oocytes:
    group_by(run) %>% mutate(OO=str_detect(loglines, oo.pattern) %>% cumsum) -> df
  
  #set HDIFF automatically
  df[[1, "run_start"]] %>% as.character %>% as.POSIXct(tz = 'Europe/Berlin')->LOGDATE
  as.POSIXlt(LOGDATE)->x
  HDIFF= 1-x$isdst
  
 
  
  if(max(df$OO)==0) cli::cli_abort(c(   "No information about recorded oocytes found in the log", 
                                        i="Each oocyte should have a line in the logs starting with '{oo.pattern}'"))
  
  # group by oocyte:
  df %>% 
    group_by(run, OO) %>% #nest(loglines=loglines) %>%
    # get Oocyte name:
    mutate(OO=loglines %>% xtract_pattern(paste0(oo.pattern, ".*(..)\n"))) %>%
    # get number of log lines:
    #mutate(nLog=loglines %>% NROW ) %>%
    
    # nest loglines
    group_by(run, OO) %>% nest(loglines=loglines) %>% 
    filter(!is.na(OO)) %>%
    # fill up XX oocytes
    ungroup -> df 
  
  df %>%   mutate(OO.=fill_wells(OO), .after=OO) %>% 
    
    # count up sweeps (aka recordings)
    unnest(loglines) %>% 
    group_by(run, OO) %>%
    mutate(swp=str_detect(loglines, recording.pattern) %>% cumsum) %>%
    group_by(run, OO, swp) %>% 
    nest(loglines=loglines) ->log
  
  
  run.given= !isFALSE(run)
  oo.given=  !isFALSE(oo)
  
  if(run.given){
    # filter first by run here
    log %>% filter(run %in% .env$run)->log
  }else{
    stopifnot(oo.given) # we cannot miss both arguments
    
  }
  ###### why did we do it not here before? added this when working with "normal oocytes" and to avoid some errors, see ERROREPORT in LABREPORTS of the GABA-PRIOJECT MCSCIENCES.prj
  if(oo.given){
    log %>% filter(OO %>% str_detect(.env$oo))->log
  }
  ##################################################
  
  if(add_r2d)
    log %>% add_r2d(HDIFF) ->log
  
  
  if(oo.given){
    log %>% filter(OO. %>% str_detect(.env$oo))->log
  } 
  
  log %>% select(-any_of("OO"), OO=OO.) %>% relocate(any_of(c("id", "plate")), run, swp, OO, loglines) 
  
}





## ----show applications
# function that gets runs, oos, and the nth application (of all recordings)
# high level function useful for getting an overview
#' @export
logs_add_app.n<-function(log,n, name=paste0("app", n)){
 log %>% group_by(run, OO, swp) %>% 
    mutate(!!name :=loglines %>% xtract_pattern( ">>> (.*)\n", which =n), .before=loglines) 
}

logs_add_app.swp.n<-function(log,swp,n){
  log %>% group_by(run, OO, swp) %>% filter(swp==.env$swp)%>%
    mutate(app1 =loglines %>% xtract_pattern( ">>> (.*)\n", which =n), .before=loglines) 
}

logs_add_app.all<-function(log){
  log %>% group_by(run, OO, swp) %>% 
    mutate(app1 =list(loglines %>% xtract_pattern( ">>> (.*)\n", which ="all")), .before=loglines) %>% 
    unnest(app1) %>%  
    mutate(m=row_number()) %>% 
    pivot_wider(values_from = app1, names_from=m, names_prefix = "app.")
}

# 
# ------------ xxxxxxxxxxx     not exported      xxxxxxxxxxxxxxxxx -----------------------
# 




#' get the full log of one (or several ) runs of a plate.
#' output is a data frame
#' we do not need to export this because there is a wrapper
get_oo_log_runs_df<- function(Plate,     
                              from=-1,     
                              to=  from, 
                              runs=NA
){
  
  
  
  
  plate_log<-read_lines(paste0(Plate, ".log"))
  
  
  script_starts <- plate_log %>% str_which(">----- MessageLog start -----<")+1
  script_ends   <- plate_log %>% str_which(">----- MessageLog end -----<")-1
  
  
  if(missing(runs)){
    n=from #sorry
    m=to
    
    
    
    stopifnot(n!=0)
    stopifnot(m!=0)
    
    if(n<0){
      n=length(script_starts) +n +1
    }
    if(m<0){
      m=length(script_starts) +m +1
    }  
    runs=n:m
  }
  
  
  runs=runs[runs>0]
  runs=runs[runs <= length(script_starts)]

  
  purrr::map_df(runs, function(run){
    s= script_starts[run]
    e= script_ends[run]
    
    if(length(s)>0 && !is.na(s)){
      lines <- s : e
      loglines <- plate_log[lines]
      data.frame(plate=Plate, run=run, loglines=loglines)%>% 
        structure(class=c("roboolog", "data.frame")) 
      
    }else{
      data.frame(run=NA, loglines=NA)
    }
    
    
    
  })
}





# helper. This can be called only from inside getlogs_per_sweep, do not export! 
add_r2d <-function(logspersweep, HDIFF){
  
  
  
  
  
  
  if(NROW(logspersweep)==0) return(data.frame(id=NA, run=NA, OO.=NA, swp=NA, loglines=NA))
  
  
 
  
    if( NROW(logspersweep %>% filter(swp!=0))==0){
      cli::cli_warn(c("there are recordings in the data file, but no matching recording-info in the logs", 
                      i=" each recording should appear in the log with  a line starting with a special marker"))
      
      r2d <- logspersweep %>% get_r2d(HDIFF) #%>% select(-run, -swp) 
      r2d %>% mutate(OO.=well, loglines=NA)
    
    } else {
      r2d <- logspersweep   %>% filter(swp!=0) %>% get_r2d(HDIFF) %>% select(-run, -swp) 
      logs <- logspersweep  %>% filter(swp!=0)
      
      
      #r2d->>.R2D
      #logs->>.LOGS
      r2d <- r2d %>% filter(well %in% unique(logs$OO) ) # this should help solving a bug where there seems to be one more recording in the r2d. it is not a real solution!!!!!
      
      if( NROW(r2d) != NROW(logs) ) cli::cli_abort(c("number of recordings in data file does not match the number of recodings in the logs", 
                                                     x=" there are {NROW(r2d)} recordings in the data file, and {NROW(logs)} recording infos found in the log",
                                                     i=" each recording should appear in the log with  a line starting with a special marker"
      ))
      bind_cols(r2d, logs) %>% 
        select(id, run, OO., swp, swp.start, swp_start, xoffset, yoffset, loglines, ptrs, data, any_of("cumsum"))
      
    }
    
   
}


#######  functions that add logs to r2d ######################
#' add just the run no. from logfile to recordings (faster than add_logs)
#' @param r2d  data frame from read_Roboocye
add_logs_run<-function(r2d){
  
  r2dfile=r2d$ptrs[[1]]$file %>% unique
  stopifnot(length(r2dfile)==1)
  
  logfile= r2dfile %>% str_replace(".r2d$", "")
  log=logs_per_sweep(logfile)
  
  r2d %>% 
    #get #run from log
    rowwise %>% 
    mutate(run= log %>% filter(run_start < swp_start & run_end > swp_start)  %>% pull(run) %>% unique) 
}


#' add all logs from logfile to recordings (sweeps)  
#' @param r2d  data frame from read_Roboocye
add_logs<-function(r2d){
  
  r2dfile=r2d$ptrs[[1]]$file %>% unique
  stopifnot(length(r2dfile)==1)
  
  logfile= r2dfile %>% str_replace(".r2d$", "")
  log=logs_per_sweep(logfile)
  
  r2d %>% 
    #get #run from log
    rowwise %>% 
    mutate(run= log %>% filter(run_start < swp_start & run_end > swp_start)  %>% pull(run) %>% unique) %>% 
    # calculate run_swp
    group_by(run) %>% 
    mutate(run_swp=as.numeric(swp)-as.numeric(swp[1])+1) %>%
    group_by(run, swp) -> r2d  
  
  # try to detec if we have "XX" oocyte names. 
  # this means that we have not impaled the oocyte  in this run, but in a previous run.  
  XX=("XX" %in% log$OO)
  
  if(!XX){
    r2d %>% mutate(log=list(log %>% filter(run_start < swp_start & run_end > swp_start& run_swp == swp & well== OO. ) )) -> r2d
  }else{
      r2d %>% mutate(log=list(log %>% filter(run_start < swp_start & run_end > swp_start& run_swp == swp  ) )) -> r2d
    }
    
  
  r2d %>% 
    rename(run_=run) %>% # hack to avoid name clash
    tidyr::unnest(log, keep_empty = TRUE) %>% 
    ungroup %>% 
    mutate(well=OO., swp=run_swp) %>% 
    select(-run_,-run_,  -run_start, -run_end,  -OO, -OO., -run_swp)
}






# helper function for function that add logs to r2d
# get logs per sweep



# ---------- helper2 -----------------

# this file contains the logfuns that are shared btw. GABA and APTinNYX
# if "Well"=="XX", replace with the last !="XX" value of all previous values. 
# 
fill_wells<-function(X){
  for (i in 1: length(X)) {
    if(!is.na(X[i])){
      
      if(X[i]=="XX"){
        X[i]<-X[i-1]
      }
      
    }
  }
  X
}





#' helper to colorise output of logfiles
colorize_word<-function(text, word, color){
  str_replace_all(text, word, color("\\1"))
}







#' helper to extract parts from the loglines
#' helper to extract parts from the loglines
xtract_pattern<-function(loglines, p, which=1){
  loglines %>% unlist %>% paste(collapse = "\n") %>% str_match_all( ., pattern = p) -> X
  #print(X)
  if(is.character(which) && which=="all"){
    # return all
    X[[1]][ ,2]
  }else{
    if(NROW(X[[1]]) >= which){
      X[[1]][which,2]
    }else{
      NA
    }
  }
}

# -------------------- helpers -------------

get_streamed_cached<-function(plate, run, selection=TRUE, fifun=unfiltered, fifun2=unfiltered, HDIFF=1, rerun=F){
    
  xfun::cache_rds(file="cached_stream", # this cache is used by "add_r2d". Is this working good? what about interference with the other caches ? 
                  dir = getOption("cache_robotraces", default = "cache/"),
                  hash = list(plate, run, HDIFF, deparse(body(fifun)), deparse(body(fifun2))), clean = F, rerun = rerun,
                  {
                    
                    # get run from logs                  
                    run_=get_logs_per_run(plate %>% str_remove(".r2d")) %>% filter(run==.env$run) %>% head(1)
                    
                    # get r2d for this run (beware of the date bug! this version works only in winter)
                    read_ROBOO(plate, get_exported_datatable=FALSE) %>% 
                      mutate(swp_start=lubridate::as_datetime(swp.start)-lubridate::dhours(HDIFF)) %>% 
                      filter(swp_start>    run_$run_start, swp_start< run_$run_end) -> r2d
                    
                    # add logs to r2d for auto legend
                    annotated_sweeps <-
                       r2d %>% add_logs_run() %>% filter(run==.env$run) #%>% #add_logs() %>% 
                    #  group_by(run, swp) %>%  #it is important to group for xtract_pattern to work!
                    #  mutate(application=loglines %>% xtract_pattern(">>>(.*)", 1), .before=id) 
                    
                    
                    # calculate offsets and add stream
                    annotated_sweeps[selection,] %>% 
                      group_by(well) %>% 
                      calculate_offsets("realtime", 0) %>%  
                      add_stream(filter_fun=fifun, filter_fun2=fifun2, maxpoints=300) 
                    
                  })
}

#' @export
print.roboolog<-function(x, .oo="(Recording oocyte in well.*\\n)", ...){
  x <- unnest(x, loglines) 
  out= paste(x$loglines, collapse = "\n")
  out=out %>% str_replace_all("Script finished: OK", "")
  out=colorize_word(out, "(.*)", blue $ bold)
  out=colorize_word(out, .oo , bgBlack $ white)
  
  out=colorize_word(out, "(==>.*\\n)", silver $ reset)
  out=colorize_word(out, "(>>>.*\\n)", white $ bold $ bgGreen)
  out=colorize_word(out, "(-=>.*\\n)", red )
  cat(out)
  cat("\n\n\n")
}





add_swp_start<-function(df, HDIFF){
  df %>% 
    mutate(swp_start=lubridate::as_datetime(swp.start)) %>% 
    mutate(swp_start = swp_start - lubridate::dhours(HDIFF))
}

# helper function for add_r2d
get_r2d <-function(logspersweep, HDIFF){
  file=unique(logspersweep$plate)
  r2d=read_ROBOO(paste0(file, ".r2d"),get_exported_datatable=FALSE) %>% add_swp_start(HDIFF)
  #r2d=
  
  stopifnot(NROW(r2d)>0)
  stopifnot(length(file)==1)
  logspersweep %>% ungroup %>% select(plate,  run_start, run_end, run) %>% distinct %>% 
    purrr::pmap_df(\(plate, run_start, run_end,run, ...){
     #r2d %>% filter( swp_start>run_start, swp_start<run_end )
      get_streamed_cached(paste0(plate, ".r2d"), run, HDIFF = HDIFF)
  })
  
}





# FIXME this could be part of  get_oo_log_runs_df ?
#' get_logs_per_run
# This assumes that our roboocyte script:
# logs "Recording oocyte in ..." per oocyte
# logs "*-" per recording
# logs ">>> .s:  ...." per valve opening. 

# based on this, we can assign log infos, especially valve openings, to sweeps. 
# before we even do this, we can assign sweeps to runs, which is very useful. 

# get_logs_per_run
# just a wrapper around get_oo_log_runs_df which adds dates and optionally also the plate
# FIXME codedup with get_runs_per_sweep
get_logs_per_run<-function(
    plate
){
  get_oo_log_runs_df(plate,1, -1) %>% #tidyr::nest(log=loglines) %>% unnest(log) %>% 
    group_by(run) %>%
    mutate(run_start =loglines %>% xtract_pattern( "==> Date: (.*)\n", which = 1) %>% lubridate::as_datetime()) %>%
    mutate(run_end   =loglines %>% xtract_pattern( "==> Date: (.*)\n", which = 2) %>% lubridate::as_datetime())
}


geom_topbars.defaultstyle = list(
  bottomspace=0,
  topspace=.01, 
  height=.02, 
  space=.01, 
  bar.colors="grey90",
  axis.bottom.style="none", 
  axis.left.style="none"
) 

# make topbars from a dataframe with topbar parameters, one bar per line
# experimental, but already used in annotate_applications2(). 
# ! too many parameters are hard coded for a general release, but for the current use case, this is fine. 
geom_topbars_fixed.x<-function(df, colors, style=geom_topbars.defaultstyle, label.x=400){
  label.x_ <- label.x
  make_topbar<-function(start,end, line,label,label.x=label.x_, ...){
    return(list(geom_topbar(
      start, 
      end, 
      line = line, 
      style = style, 
      label=label, 
      label.col="black", 
      fill = colors[line], 
      label.x = label.x, 
      label.position = "right", 
      label.size =9, 
      line_to.y=-Inf, 
      line_to.y2=-Inf, 
      line_to.linetype = 3
    )))
  }
  
  
  df %>% purrr::pmap(make_topbar)
}

# position = "centered", no fixed x
geom_topbars<-function(df, colors, style=geom_topbars.defaultstyle){
  
  make_topbar<-function(start,end, line,label, ...){
    return(list(geom_topbar(
      start, 
      end, 
      line = line, 
      style = style, 
      label=label, 
      label.col="black", 
      fill = colors[line], 
      
      
      label.size =9, 
      line_to.y=-Inf, 
      line_to.y2=-Inf, 
      line_to.linetype = 3
    )))
  }
  
  
  df %>% purrr::pmap(make_topbar)
}

# this helper should be called whenever we read Roboocyte files, be it .log, .r2d or similar. 
# this ensures that the robocyte is not running and we are not making the roboocyte crash. 
# it will not ask again within 1 minute. 
assert_robo_not_running<-function(){
  return(NULL) # this is not really ready to use, so it stays inactive for a while
  if(!exists("Robo_confirmed_not_running_timestamp")) Robo_confirmed_not_running_timestamp <<- Sys.time() - lubridate::hours(10)

  if( Sys.time() - Robo_confirmed_not_running_timestamp > lubridate::minutes(1) ){
   if( !  try(yesno::yesno2("You are about to read Log data - Are you sure the Roboocyte is not running a script ?", yes = "Yes I am sure!")) ==1 ){
    cli::cli_abort("Aborted because you said the Roboocyte might be running")
   
  }
  
  
  }
  # reset timer:
  Robo_confirmed_not_running_timestamp <<- Sys.time()
}


