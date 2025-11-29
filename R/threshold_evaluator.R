# function to evaluate chunks, defined as data above or below a threshold
# the threshold can be a fixed value or a curve (a vector with same length as the data)
# if the threshold contains NAs, it also defines a ROI (the non-NA region)
# returns a data frame with the result of eval_fun and the chunks borders
threshold_evaluator <-function(xdata, ydata, threshold, eval_fun, direction=c("above","below"), min_chunksamples=1, start_plus=0, end_minus=0){
  
  if(length(threshold)==1)
    threshold=rep(threshold,length(xdata))
  # get indices of all data points above (or below) the threshold
  direction<-match.arg(direction)
  if(direction=="above")
    idx_selected<-which(ydata>threshold)
  else
    idx_selected<-which(ydata<threshold)

  #print(idx_selected)
  # get chunk starts and ends: a chunk ends if the distance to the next index is > min_chunksamples
  idx_selected_ChunkEnds<-which(c(idx_selected[-1],NA)-idx_selected > min_chunksamples)
  # plus, of course, the last of the selcted points
  idx_selected_ChunkEnds<-c((idx_selected_ChunkEnds-end_minus),length(idx_selected))
  #print(idx_selected_ChunkEnds)
  # the starts are derived from the ends
  idx_selected_ChunkStarts<-c(1,(idx_selected_ChunkEnds[-length(idx_selected_ChunkEnds)]+1)+ start_plus) 
  idx_selected[idx_selected_ChunkStarts[1]]->>TEST
  #analyse chunks
  if(length(idx_selected_ChunkStarts)>0 && (!is.na(idx_selected[idx_selected_ChunkStarts[1]]))){
    result<-do.call(rbind, lapply(1:length(idx_selected_ChunkStarts), function(chunkNr){
      CHUNK_START <- idx_selected[idx_selected_ChunkStarts[chunkNr]]
      CHUNK_END <- idx_selected[idx_selected_ChunkEnds[chunkNr]]
      
      CHUNK<-  CHUNK_START : CHUNK_END
      result_y<-eval_fun(ydata[CHUNK])
      result_x<-xdata[CHUNK][min(which(ydata[CHUNK]==result_y))]
      result=data.frame(x=result_x, y=result_y, chunk_start=CHUNK_START, chunk_end=CHUNK_END)
    }))
  }else{
    result=data.frame(x=numeric(), y=numeric(), chunk_start=numeric(), chunk_end=numeric())
  }
  
  invisible(result)
}


