# donwsample to maxpoints
downsample_df<-function(x,maxpoints){
  x %>% group_split() %>% purrr::map_df(downsample_group, maxpoints)
}
downsample_group<-function(x,maxpoints){
  points=NROW(x)
  downsample<- ceiling(points/maxpoints)
  x[(1:floor(points/downsample))*downsample, ]
}