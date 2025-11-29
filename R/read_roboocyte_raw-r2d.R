read_ROBOO_r2d<-function(file, cache_rerun=F){
  xfun::cache_rds( # this is not about reading the traces, but the wellinfo. So this makes sense?
    dir = getOption("cache_robotraces", default ="cache_robotraces/"), rerun = cache_rerun, 
    hash = list(file, file.size(file)), 
    clean = F,
    file="RTrawTable",
    
    {
  
    assert_robo_not_running()
      
  Wellinfo  = data.frame(
    WellID=character(), 
    WellRecordingID=character(),
    FPosRecordingData=integer(), 
    file=character(), 
    HWSampleRate=numeric())
  
  con=file(file, open = "rb")
  
  seek(con, get_entry_num(con, "FPosFirstRecordingHdr",3) )
  
  
  repeat{
    
    WellID   =          get_entry_num(con, "WellID",1,50) 
    RecordingID       = get_entry_num(con, "RecordingID",0,50)
    WellRecordingID   = get_entry_num(con, "WellRecordingID",0,50)
    TimeStampStart    = get_entry    (con, "TimeStampStart",0,1)
    HWSampleRate      = get_entry_num(con, "HWSampleRate",6,50)
    FPosRecordingData = get_entry_num(con, "FPosRecordingData",0,50)
    anchor<-seek(con)
    SampleRate      = get_entry_num(con, "SampleRate",6,50)
    seek(con, anchor)
    
    Wellinfo <- 
      rbind(Wellinfo, data.frame(
                          well=WellID,
                          swp  = WellRecordingID, 
                          RecordingID  = RecordingID, 
                          TimeStampStart   = (lubridate::ymd_hms(TimeStampStart) + lubridate::hours(2)) %>% lubridate::seconds() %>% as.double(),
                          FPosRecordingData= FPosRecordingData, 
                          file             = file,
                          file_             = basename(file),
                          HWSampleRate     = HWSampleRate, 
                          SampleRate     = SampleRate))
    
    nxt_rec <- get_entry_num(con, "FPosNextRecordingHdr",2,50)
    
    if( nxt_rec == -1 ){ break  }
    
    seek(con, nxt_rec )

  }
  
  close(con)
 

   Wellinfo %>% 
    mutate( swp=factor(as.numeric(swp)+1) ) %>% 
    tidyr::nest(ptrs=c(file, FPosRecordingData, RecordingID, HWSampleRate, SampleRate)) %>%
    tidyr::hoist(ptrs, "RecordingID", .remove = FALSE) %>% # we need to have RecordingID both as a column and inside ptrs ... 
    mutate(well=WellCode(well+1), 
           file_=file_ %>% stringr::str_remove(".r2d$"),
           id=paste(file_, well, swp, sep="-")) %>% 
    rename(file=file_) %>% 
    relocate(id, file,well) %>% 
    rename(swp.start=TimeStampStart) %>% relocate(swp.start,  .after=swp) %>%
    select(-RecordingID) %>%
    mutate(xoffset=0, yoffset=0) %>% relocate(ptrs, .before = xoffset)
  
  
    })

}



# 1 => "A1"
WellCode <- function(W){
  paste0( LETTERS[ floor((W-1) / 12) +1 ],  ((W-1) %% 12) +1 ) 
}

# "A1" => 1
WellID<-function(WellCode){
  LETTER <- stringr::str_sub(WellCode,1,1)
  NUMBER <- stringr::str_sub(WellCode,2)
  
  (which(LETTERS==LETTER)-1) * 12 + as.numeric(NUMBER)
}



#' @export
get_recordinginfos<-function(rpf_file){
  assert_robo_not_running()
  rpf<- xml2::read_xml(rpf_file) %>% xml2::as_list()
  
  WellSampleInfos <-
    tibble::as_tibble(rpf$Robo2$Roboinject$WellSampleInfos) %>%
    tidyr::unnest_wider(col = names(.)) %>% 
    tidyr::unnest_wider(SampleInfo) %>%  
    select(-SampleInfoKey) %>% # these are redundant columns
    select(-PropInject) %>% 
    tidyr::unnest(cols = names(.)) %>% 
    tidyr::unnest(cols = names(.)) 
  
  Samples <-
    tibble::as_tibble(rpf$Robo2$Roboinject$Samples) %>%
    tidyr::unnest_wider(col = names(.)) %>% 
    tidyr::unnest_wider(Sample) %>%  
    tidyr::unnest_wider(PropInject) %>% 
    tidyr::unnest(cols = names(.)) %>% 
    tidyr::unnest(cols = names(.)) 
  
  
  
  
  recordings<-
    tibble::as_tibble(rpf$Robo2$Roboocyte2$RecordingInfos)  %>% 
    tidyr::unnest(cols = names(.)) %>% 
    tidyr::unnest(cols = names(.))%>% 
    tidyr::unnest(cols = names(.)) %>% 
    tidyr::unnest_wider(col = names(.))%>% 
    relocate(ROIs) %>%
    tidyr::unnest(cols = names(.)[-1])%>% 
    tidyr::unnest(cols = names(.)[-1]) %>% 
    tidyr::hoist(ROIs, base.right=list("BaselineROI", "Right",1,1)) %>%
    tidyr::hoist(ROIs, base.left =list("BaselineROI", "Left",1,1)) %>%
    tidyr::hoist(ROIs, .right     =list("AllAnalysisRegions", "AllAnalysisROIList", "ROI", "Right",1,1)) %>%
    tidyr::hoist(ROIs, .left      =list("AllAnalysisRegions", "AllAnalysisROIList", "ROI", "Left" ,1,1)) %>% 
    # valves, Gilson, and ClampVoltage/current
    tidyr::hoist(ROIs, rcinfo=list("AllROIRecInfos", "AllAdditionalROIRecInfos", "ROIRecordingInfo")) %>% 
    tidyr::unnest_wider(rcinfo, names_sep = ".", simplify = T,) %>% 
    tidyr::unnest(contains("rcinfo"))%>% 
    tidyr::unnest(contains("rcinfo")) %>%
    relocate(contains("."),ROIs,  .after = last_col()) %>% 
    select(-contains("Rec"), -contains("SampleRate"), -contains("DCOffset")) %>% 
    tidyr::nest(Recinfo=c(Canceled, contains("Relevant"), contains("rcinfo")),  ) %>% 
    relocate(contains("Timestamp"), .after=Series) 
  
  
  RNAinfo <- 
    left_join(WellSampleInfos, Samples, by=c("SampleID"="ID")) 
  
  left_join(recordings, RNAinfo, by=c("WellID"="WellIndex")) %>%
    group_by(WellID) %>% mutate(WellID=factor(as.numeric(WellID)), swp=factor(row_number()-1), .after=WellID) %>% mutate(well=factor(WellCode(as.numeric(WellID)+1)))
}


get_rpf_info <-function(file.rpf){
  assert_robo_not_running()
  rpf<-xml2::read_xml(file.rpf)
  
  tibble(
    WellID =       rpf %>% xml2::xml_find_all(".//WellID") %>% xml2::xml_integer() ,
    RecordingID =       rpf %>% xml2::xml_find_all(".//RecordingID") %>% xml2::xml_integer() ,
    WellRecordingID =   rpf %>% xml2::xml_find_all(".//WellRecordingID") %>% xml2::xml_integer(), 
    TimestampStart  =   rpf %>% xml2::xml_find_all(".//TimestampStart") %>% xml2::xml_text() ,
    IsIVCurve=   rpf %>% xml2::xml_find_all(".//IsIVCurve") %>% xml2::xml_text(),
    IVCurveID=   rpf %>% xml2::xml_find_all(".//IVCurveID") %>% xml2::xml_text(),
    IVRecordingIndex=   rpf %>% xml2::xml_find_all(".//IVRecordingIndex") %>% xml2::xml_text(),
    Valve=   rpf %>% xml2::xml_find_all(".//Valve") %>% xml2::xml_text()
  ) 
}



# internal helper function
get_entry_num<-function(...){
  as.numeric(
    get_entry(...) 
  )
}

get_entry<-function(con, name, skip, .max_search=1){
  
  readLines(con, n=skip)
  
  for (i in 1:.max_search ){
    line  <- readLines(con, n=1)
    #print(seek(con))
    split <- strsplit(line,"=")
    if(split[[1]][1]==name){
      if (i > 1) 
        print(paste(name," please skip ",i-1, " more lines"))
      break
    } 
  }
  if(!(split[[1]][1]==name)) stop("found: ", split[[1]][1], "       expected: ", name)
  split[[1]][2] 
}
