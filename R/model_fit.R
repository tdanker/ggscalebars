#' model functions for cursors  
#'
#' 

#' @param data the data for the model, passed by the cursor. 
#' @name model_funs
NULL

#' @describeIn model_funs
#' lm fit
#' @export
model_fun_lm<-function(data) lm(y~x, data)

#' @describeIn model_funs
#' 
#' wir kriegen log_alpha raus, alpha ist exp(log_alpha)
#' https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/ 
#' 
#' tau ist dann -1/alpha:
#' https://www.amplifier.cd/Tutorial/Exponential%20Fit/Exponential%20Fit.html
#' 
#' tau can be calculated as 1/exp(log_alpha)
#'  
#' @export
model_fun_exp<-function(data) nls(y~SSasymp(x, yf, y0, log_alpha), data=data)






  


# next step:
# https://stackoverflow.com/questions/51216981/r-fitting-a-double-exponential-growth-curve

#' @describeIn model_funs
#' exponential fit with x0 set to start of cursor (has problems with plotting!)
#' @export
model_fun_exp2<-function(data){ data<-mutate(data, x=x-x[1]);  nls(y~SSasymp(x, yf, y0, log_alpha), data=data)}