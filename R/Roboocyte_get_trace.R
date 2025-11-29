get.ROBOOCYTE <- function(file, RecordingID, FPosRecordingData,  SampleRate, ch=2,start=0, end=NA, rerun=F,  ...) {
  trace_ = xfun::cache_rds(  # Todo: test if this caching is really a good thing. 
    dir = getOption("cache_robotraces", default ="cache_robotraces/"), rerun = rerun, 
    hash = list(file, RecordingID, FPosRecordingData,  SampleRate, ch,start, end), 
    clean = F,
    file="RTraw",
    
    {
      assert_robo_not_running()
      
      
      y = NULL
      con = file(file, open = "rb", encoding="ANSI")
      
      seek(con, FPosRecordingData)
      
      FPosNextSegmentHdr <-
        suppressWarnings(readLines(con,n=800)) %>% 
        as_tibble %>% 
        filter(value %>% stringr::str_starts("FPosNextSegmentHdr")) %>%
        tidyr::separate(1, into=c("key","value"), sep="=", extra = "merge", fill="left") %>% 
        filter(key=="FPosNextSegmentHdr") %>% .$value %>% as.numeric
      
      nxt = FPosNextSegmentHdr[ch]
      
      for (i in 1:999999999) {
        seek(con, nxt)
        l = suppressWarnings(readLines(con, n = 8))
        #print(l)
        size = as.numeric(strsplit(l[8], "=")[[1]][2])
        nxt = as.numeric(strsplit(l[6], "=")[[1]][2])
        
        b = readBin(con, "integer", n = size, size = 4) #size=4
        SEGHDR = readBin(con, what = "raw", n = 8)
        
        
        
        y <- c(y, b)
        if (nxt == -1) {
          break
        }
      }
      
      close(con)
      
      x=(1:length(y)) / (SampleRate)
      #if(is.na(end)) end<-max(x)
      data.frame(
        x=x, 
        y=y/10000,
        TraceTime=0
      )  
      
      
    })
  trace_
}

get_tracefile_locale <- function(file){
  if (readr::read_lines(file, n_max = 1, skip = 1) %>% stringr::str_detect("\\.")) {
    
    readr::locale(decimal_mark = ".", grouping_mark = ",")
  }else {
    
    readr::locale(decimal_mark = ",", grouping_mark = ".")
  }
}

# read Roboocyte traces data from exported traces. 
# caching seems to work quite well
get.ROBOOCYTE_ <- function(file, RecordingID, start=0, end=NA, rerun=F,  ...) {
  trace_ = xfun::cache_rds( # caching seems to work quite well
    dir = "cache_robotraces/", rerun=rerun, 
    hash = list(file, RecordingID, start, end), 
    clean = F,
    file="RTexp",
    
    {
      if(file %>% stringr::str_ends("_Export_Datatable.dat")){
        export_traces_file <- file %>% stringr::str_remove("_Export_Datatable.dat") %>% paste0("_Export_Traces.dat")
      }else{
        export_traces_file <- file
      }
      
      locale_ = get_tracefile_locale(export_traces_file )
      trace <- readr::read_delim(export_traces_file, 
                                 "\t", 
                                 col_types = readr::cols(.default = "d"),
                                 col_select = tidyselect::ends_with(paste0("-", RecordingID)), 
                                 escape_double = FALSE, 
                                 na=c("", "N/A", "NA"), 
                                 trim_ws = TRUE ,
                                 lazy = TRUE, 
                                 locale = locale_) 
      
      
      data.frame(
        x=trace[[1]]/1000, 
        y=trace[[2]],
        TraceTime=0 
      ) 
      
    }
    
  ) 
  
  trace_ %>% filter(!is.na(x))
  
  
}