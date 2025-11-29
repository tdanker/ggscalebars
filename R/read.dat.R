# read .pul 'treeinfo' which contains pointers to trace data
read.bundletree <- function(myfile, bundlename = ".pul", con=NA) {
  finally_close_con=is.na(con)
  if(is.na(con)){
    con <- file(myfile, "rb")
  }
  seek(con, 0)
  signature <- rawToChar(readBin(con, what="raw", 8))  #readChar(con, 8)
  
  if(signature=="DAT2"){
    # we are in a *.dat bundle file and have to find the start position
    #x=seek(con)
    #version <- readChar(con, 32)
    #seek(con,x)
    version <- rawToChar(readBin(con, what="raw", 32))
    #print(version)
    #print(version2)
    
    time <- readBin(con, "double")
    nitems <- readBin(con, "int", size = 1)
    liddle_endian <- readBin(con, "logical")
    
    
    reserved <- readBin(con, what="raw", 11) 
    
    bundleitems <- do.call(rbind, (lapply(0:nitems, function(item) {
      start <- readBin(con, "int", size = 4)
      end <- readBin(con, "int", size = 4)  #end
      name <- rawToChar(readBin(con, what="raw", 8))  #readChar(con, 8)
      data.frame(
        start = start,
        end = end,
        name = name,
        stringsAsFactors = F
      )
    })))
    
    start <- subset(bundleitems, name == bundlename)$start
    seek(con, where = start)
  }else{
    # we are in *.pul or *.pgf file, starting at 0
    start <- 0 
  }
  
  seek(con, where=start )
  magic <- readChar(con, nchars = 4)
  stopifnot(magic == "eerT")
  
  nLevels <- readBin(con, "int", size = 4)
  lvl_sizes <- lapply(1:nLevels, function(i) {
    readBin(con, "int", size = 4)
  })
  
  tree <- pm_load_nodes(con, nLevels, lvl_sizes, 1)
  
  if(finally_close_con )
     close(con)
  tree
}


# recursively load all nodes of a HEKA tree (can be .pul , .amp, etc)
pm_load_nodes <- function(con, nlevels, lvl_sizes, level) {
  stopifnot(level <= nlevels)
  size <- lvl_sizes[[level]]  #size of the data block
  dataptr <- seek(con)
  # skip the 'data block'
  seek(con, seek(con) + size)
  nchildren <- readBin(con, "int", size = 4)
  if (nchildren == 0) {
    node <- "trace"
    attr(node, "dataptr") <- dataptr
  } else {
    node <- lapply(1:nchildren, function(child) {
      pm_load_nodes(con, nlevels, lvl_sizes, level + 1)
    })
    attr(node, "dataptr") <- dataptr
  }
  node
}




# function to get a trace data from a file and a 'treeinfo' node (ptr)
# start= first datapoint to read
# n number of datapoints to read
getTrace_ <- function(con, ptr, start=0, n=NA, read_data=T, name="", con_dat=con, step=1, tracefilter=NA) {
  SIZE = 2 
  tracename<-name
  # ptr <- attr(ptr, "dataptr")
  #con = file(file, "rb")
  seek(con, ptr + 40)
  offset <- readBin(con, "int", size = 4)
  nDatapoints_ <- readBin(con, "int", size = 4)
  
  nDatapoints <- nDatapoints_ - start
  if(!is.na(n)){
    nDatapoints <- min(nDatapoints, n)
  }
  
  seek(con, ptr + 96)
  Unit = readBin(con, "char")
  if (Unit == "V")
    Unit_ = 1000
  else
    Unit_ = 1e+09
  seek(con, ptr + 72)
  DataScaler = readBin(con, "double", size = 8)
  seek(con_dat, where = offset+start*SIZE)
  if(read_data){
    trace <-
      readBin(con_dat,
              what = "int",
              size = SIZE,
              n = nDatapoints) * Unit_ * DataScaler
  }else{
    trace <- NA
  }
  
  seek(con, ptr + 104)
  Xinterval <- readBin(con, "double", size = 8)
  
 
  
  #close(con)
  attr(trace, "Xinterval") <- Xinterval
  attr(trace, "nDatapoints_") <- nDatapoints
  attr(trace, "name") <- tracename
  
  # filter *before* downsampling
  if(!identical(NA, tracefilter)){
    trace<-tracefilter(trace)
  }
  
  #downsampling
  if(step!=1){
    idx   <- seq(1, length(trace), by = step)
    trace <- trace[idx]
  }
  
  
  trace
}

# label (offset4) only for exp, ser, swp, trace
# text (offset 36 for exp and ser, offset120 for root)
readlabel <- function(ptr, con, offset=4) {
  seek(con, where = ptr + offset)
  readBin(con, "char")
}



readAny <- function(ptr, con, offset, what, size) {
  seek(con, where = ptr + offset)
  readBin(con, what, size = size)
}



