#' Cursors 
#'
#' mainly for internal use. 
#' 
#' @param data a tibble with ephys data
#' @param start,end start and end of cursor range
#' @param st2,en2 start and end of extended cursor range
#' @param fun a function taking a vector and returning a summary value, like min, max or median
#' @param model_fun a function representing a model, see \link{model_funs}
#' @param th_fun function that defines a threshod for mulit-peaks
#' @name cursors
NULL

#' @describeIn cursors
#' general case for min, max, first, etc: whatever identifies a single data point
#' @export
point_<-function(data, fun, start, end, condition, annot, ...){
 if(condition){
   y=fun(data$y, ...)
   x=data$x[which.min(abs(data$y-y))]
   st=min(data$x)
   en=max(data$x)
   list(x=x, y=y, st=st, en=en, 
        annotation=list(annot))
 }else{
   list(x=NA, y=NA, st=NA, en=NA, 
        annotation=list(annot))
 }
  
  
}
attr(point_, "annot") <- point_.annot


#' @describeIn cursors
#' AP (cardiac). this improved version skips the first 50 samples after the maximum found so that "spikes" are excluded from 
#' finding the maximum. It also has improved perfomance, because AP_ had some bugs. 
#' @export
AP2_<-function(data,  fun, start, end, annot=geom_cursor_AP_, ...){
  
  max_add=50 #this means that we skip thwe first 50 samples after the maximum when we determine the "range_post"
  # we then search again for a maximum within "range_post", so this is then the "real maximum" after any potential QR-overshoot. 
  
  #threshold =  #not sure what it does. (or did, now deactivated) 
  # see below where we ssek the onset position. 
  # - when Vm+10 is higher, we take Vm+10  
  # it then seems that we take as onset_position either the point where we have maximum velocity, or 
  # the point where we cross either "thresold" or Vm+10, whichever occures first.
  
  d = NULL
  d$Y = data$y
  d$X = data$x
  
  range_total <- 1:length(d$Y)
  last <- function(x) rev(rev(x)[1])
  
  max_in_range <- which.max(d$Y[range_total])
  range_pre = range_total[1:max_in_range]
  range_post = range_total[(max_in_range+max_add):length(range_total)]
  
  
  max_postion = range_post[which.max(d$Y[range_post])]  #range_total[max_in_range]
  min_position = range_post[which.min(d$Y[range_post])]
  
  # helper function to get the velocity of a range
  dVdT <- function(p, show = F) {
    d_ = 5 # number of samples to look ahead 
    upstroke_voltage <- (d$Y[p + d_] - d$Y[p]) * 0.001
    upstroke_time <- (d$X[p + d_] - d$X[p])
    velocity <- upstroke_voltage/upstroke_time
    # if (show) 
    #   arrows(d$X[p], d$Y[p], d$X[p + d_], d$Y[p + d_], 
    #          col = max(1, floor(velocity)), lty = 1, len = 0, 
    #          lwd = 3)
    velocity
  }
  velocites = sapply(range_pre, dVdT)
  max_velocity = max(velocites)
  max_velocity_position <- range_pre[which.max(velocites)]
  amplitude <- d$Y[max_postion] - d$Y[min_position]
  Vm <- d$Y[min_position]
  
  # find onset_position
  # threshold = max(threshold, Vm + 10)
  level90 <- d$Y[max_postion] - amplitude * 0.9
  # onset_position = range_pre[which(d$Y[range_pre] > threshold)]
  # onset_position = min(onset_position, max_velocity_position)
  onset_position= max_velocity_position
  
  APD90_end <- range_post[which(d$Y[range_post] < level90)[1]]
  APD90 = d$X[APD90_end] - d$X[max_postion]
  level50 <- d$Y[max_postion] - amplitude * 0.5
  APD50_end <- range_post[which(d$Y[range_post] < level50)[1]]
  APD50 = d$X[APD50_end] - d$X[max_postion]
  level30 <- d$Y[max_postion] - amplitude * 0.3
  APD30_end <- range_post[which(d$Y[range_post] < level30)[1]]
  APD30 = d$X[APD30_end] - d$X[max_postion]
  data.frame(start, end, APD90, APD50, APD30, max_velocity, Vm, amplitude, 
             peak = d$Y[max_postion], .min_position = d$X[min_position], 
             .max_postion = d$X[max_postion], .onset_position = d$X[onset_position], 
             .APD90_end = d$X[APD90_end], .level_90 = level90, 
             .APD50_end = d$X[APD50_end], .level_50 = level50, 
             .APD30_end = d$X[APD30_end], .level_30 = level30, 
             annotation=list(annot))
}
attr(AP2_, "annot") <- geom_cursor_AP_


#' @describeIn cursors
#' AP (cardiac) (THIS VERSION IS SOFT-DEPRECATED). PLEASE USE AP2_. 
#' @export
AP_<-function(data, fun, start, end, annot=geom_cursor_AP_, ...){
   
  d = NULL
  d$Y = data$y
  d$X = data$x
  range_total <- 1:length(d$Y)
  last <- function(x) rev(rev(x)[1])
  threshold = -30
  max_in_range <- which.max(d$Y[range_total])
  range_pre = range_total[1:max_in_range]
  range_post = range_total[max_in_range:length(range_total)]
  max_postion = range_total[max_in_range]
  min_position = range_post[which.min(d$Y[range_post])]
  dVdT <- function(p, show = F) {
    d_ = 5
    upstroke_voltage <- (d$Y[p + d_] - d$Y[p]) * 0.001
    upstroke_time <- (d$X[p + d_] - d$X[p])
    velocity <- upstroke_voltage/upstroke_time
    if (show) 
      arrows(d$X[p], d$Y[p], d$X[p + d_], d$Y[p + d_], 
             col = max(1, floor(velocity)), lty = 1, len = 0, 
             lwd = 3)
    velocity
  }
  velocites = sapply(range_pre, dVdT)
  max_velocity = max(velocites)
  max_velocity_position <- range_pre[which.max(velocites)]
  amplitude <- d$Y[max_postion] - d$Y[min_position]
  Vm <- d$Y[min_position]
  threshold = max(threshold, Vm + 10)
  level90 <- d$Y[max_postion] - amplitude * 0.9
  onset_position = range_pre[which(d$Y[range_pre] > threshold)]
  onset_position = min(onset_position, max_velocity_position)
  APD90_end <- range_post[which(d$Y[range_post] < level90)[1]]
  APD90 = d$X[APD90_end] - d$X[max_postion]
  level50 <- d$Y[max_postion] - amplitude * 0.5
  APD50_end <- range_post[which(d$Y[range_post] < level50)[1]]
  APD50 = d$X[APD50_end] - d$X[max_postion]
  level30 <- d$Y[max_postion] - amplitude * 0.3
  APD30_end <- range_post[which(d$Y[range_post] < level30)[1]]
  APD30 = d$X[APD30_end] - d$X[max_postion]
  data.frame(start, end, APD90, APD50, APD30, max_velocity, Vm, amplitude, 
             peak = d$Y[max_postion], .min_position = d$X[min_position], 
             .max_postion = d$X[max_postion], .onset_position = d$X[onset_position], 
             .APD90_end = d$X[APD90_end], .level_90 = level90, 
             .APD50_end = d$X[APD50_end], .level_50 = level50, 
             .APD30_end = d$X[APD30_end], .level_30 = level30, 
             annotation=list(annot))
}
attr(AP_, "annot") <- geom_cursor_AP_


#' @describeIn cursors
#' add a bar
#' @export
bar_<-function(data, start, end, line, fixed.y, sweeps, label,bar.mapping, fill, border, label.col, label.size, hjust, vjust, label.x, label.sweeps, annot=bar_.annot){
  
  st=start
  en=end
  line=line
  sweeps=sweeps
  fill=fill
  list(st=st, en=en, line=line, fixed.y=fixed.y, sweeps=sweeps, label=label, bars=bar.mapping, fill=fill, border=border, label.col=label.col, label.size=label.size, hjust=hjust, vjust=vjust, label.x=label.x, label.sweeps=label.sweeps, 
       annotation=list(annot))
  
}
attr(bar_, "annot") <- bar_.annot

#' @describeIn cursors
#' general case for mean, median, etc: whatever identifies a level
#' @export
level_<-function(data,  fun, start, end, annot=level_.annot, ...){
  
  y=fun(data$y)
  st=min(data$x)
  en=max(data$x)
  list(y=y, st=st, en=en, 
       annotation=list(annot))
  
}
attr(level_, "annot") <- level_.annot

#' @describeIn cursors
#' general case for models
#' @export
model_<-function(data,  model_fun, start, end, st2=start, en2=end, annot=model_.annot, x0=0,...){
  
  model_fun_ <- purrr::possibly(model_fun, list(e="no fit"))
  data$x=data$x - x0
  model=model_fun_(data)
  st=min(data$x)
  en=max(data$x)
  list(model=list(model), st=st, en=en, st2=st2, en2=en2, x0=x0,
       annotation=list(annot))
}
attr(model_, "annot") <- model_.annot 







#' @describeIn cursors
#' multiple peaks above a threshold
#' @export
peaks_multy_<-function(data, fun=max, start, end, th_fun=mean,  direction=c("above", "below"), min_chunksamples=1, start_plus=0, end_minus=0, annot=peaks_multy_.annot  ){
  
  th=th_fun(data$y)
  peaks=threshold_evaluator(data$x, data$y, threshold =th,eval_fun = fun, direction = direction, min_chunksamples = min_chunksamples, start_plus=start_plus, end_minus=end_minus)
  st=min(data$x)
  en=max(data$x)
  list(y=peaks$y, x=peaks$x, st=st, en=en, th=th, chunk_start=peaks$chunk_start, chunk_end=peaks$chunk_end, 
       annotation=list(annot)) -> res
  attr(res$y,".transform")<-length # this specifies how to auto-transform by default
  res
}
attr(peaks_multy_, "annot") <- peaks_multy_.annot 






