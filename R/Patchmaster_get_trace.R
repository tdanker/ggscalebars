# get (partial) data from HEKA Patchmaster files:
# this is currently not used at all. will probably be removed soon. 
get.Patchmaster_cached<-function(file, trc., swp., start=0, end=NA, rerun=F,  ...){
  
  trace_ = xfun::cache_rds(  # this chaching is only making things worse. use get.Patchmaster_noCache for better performance without caching.
    dir = get_cachedir("cache_HEKAtraces"), # currently not used
    rerun = rerun, 
    hash = list(file, trc., swp.,  start, end), 
    clean = F,
    file="HEKAraw",
    
    {
      
      con=file(file, "rb")
      seek(con, trc. + 104)
      Xinterval <- readBin(con, "double", size = 8)
      start= floor(start/Xinterval)
      if(!is.na(end)){
        end= ceiling(end/Xinterval)
        n=end-start  
      }else{
        n=NA
      }
      
      y <- getTrace_(con=con, ptr=trc., start = start, n=n)
      nDatapoints<- attr(y, "nDatapoints_")
      TraceTime=readAny(swp., con, 56,"double",8)
      
      x <- (start+(1:length(y))) * Xinterval
      
      close(con)
      
      data.frame(y=y, x=x, TraceTime=TraceTime)  
      
    })
  trace_
}



# version that does no caching (very fast)
get.Patchmaster_noCache<-function(file, trc., swp., start=0, end=NA, y_only=F,  ...){
  file=get_file(file)
  con=file(file, "rb")
  seek(con, trc. + 104)
  Xinterval <- readBin(con, "double", size = 8)
  start= floor(start/Xinterval)
  if(!is.na(end)){
    end= ceiling(end/Xinterval)
    n=end-start  
  }else{
    n=NA
  }
  
  y <- getTrace_(con=con, ptr=trc., start = start, n=n)
  nDatapoints<- attr(y, "nDatapoints_")
  TraceTime=readAny(swp., con, 56,"double",8)
  
  x <- (start+(1:length(y))) * Xinterval
  
  close(con)
  if(y_only) return( y)
  
  data.frame(y=y, x=x, TraceTime=TraceTime) 

}
