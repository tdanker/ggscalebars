# cut from start to end (in seconds)
cut_df<-function(ephysdata, start, end){
  ephysdata %>% group_split() %>% purrr::map_df(cut_group, start, end)
}
cut_group<-function(ephysdata, start, end){
  samplerate<-diff(ephysdata$x[1:2])
  points=ceiling((end-start)/samplerate)
  #downsample<- 1 # NOT HERE!   ceiling(points/maxpoints)
  #print(paste( "#start:", start, "#end:", end, "#points:", points, "#samplerate:", samplerate, "#downsample:" ,downsample))
  OFFSET<-floor(ephysdata$x[1]/samplerate)
  START<-ceiling(start*(1/samplerate))+1-OFFSET
  END<-floor(end*(1/samplerate))+1-OFFSET
  if(START<0) cli::cli_abort(c("error in subseting data stream: start-value ({start}) is below starting point of available data ({ephysdata$x[1]})", 
                               "i"="did you provide cursor limits (start-end) outside the range of available data of your current stream?"))
  ephysdata[START:END, ]
}
