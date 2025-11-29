get.ephysdata_fast <- function(ptrs,  ...) {
  UseMethod("get.ephysdata_fast")
}

#' @noRd
get.ephysdata_fast.ptrs_heka <- function(ptr, ...) {
  
  get.Patchmaster_noCache( ptr$file, ptr$trc., ptr$swp.) %>% select(x,y,TraceTime)
}

#' @noRd
get.ephysdata_fast.ptrs_robo <- function(ptr, rerun, ...) {
  
  get.ROBOOCYTE(
    file = ptr$file,
    RecordingID = ptr$RecordingID,
    FPosRecordingData = ptr$FPosRecordingData,
    SampleRate = ptr$SampleRate,
    ch = 3,
    rerun = rerun

    ) 
  
}

#' @noRd
get.ephysdata_fast.ptrs_robo_from_exported_data<- function(ptr, rerun, ...) {
  
  get.ROBOOCYTE_(
    file = ptr$file,
    RecordingID = ptr$RecordingID,
    rerun = rerun
  ) 
  
}

# helper functions used by read.xxx function to set the class
add_class<-function(x,newclass){class(x)<-c(class(x),newclass);x}
add_ptrs_class <- function(., ptrs_class){ mutate(., ptrs = purrr::map(ptrs, add_class, ptrs_class))}


