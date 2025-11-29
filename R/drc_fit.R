#' Fit drc models
#'
#' drc_fit expects a data frame with the first three columns represeting response, concentration, and compound (in that order, but names are ignored) and fits drm models for each compound.
#' The output can be passed to \link{drc_plot} or \link{drc_table}.
#'
#' For more info, see \href{../doc/drc.html}{\code{vignette("Dose resoponse curves", package = "ephys4")}}
#'
#' @param df  data frame with data to fit.
#'
#' @param params determines the type of fit
#' @param add_confidence set this to true if you want to plot confidence intervals. warning: this is slow!
#'
#' @return a tibble with a drc model for each compound
#' @family drc methods
#'
#'
#' @export
drc_fit<-function(df, params=2, add_confidence=FALSE){

  df <- dplyr::ungroup(df)
  names(df)[1:3]<-c("response", "concentration", "compound")

  # define drm function to use with map
  fct=switch(EXPR=params,NULL,drc::LL2.2, drc::LL2.3, drc::LL2.4)
  names=switch(EXPR=params,NULL,
                c("HILL", "IC50"),
                c("HILL", "max","IC50"),
                c("HILL", "min", "max","IC50")
               )

  drm.func <- function(x) {
    drc::drm(response ~ concentration,
        fct = fct(names = names),
        data = x)
  }

  predict.fun <- function(x) {
    if(add_confidence){
      X= exp(seq(-20,10, length.out = 100))
      pred=predict(x, newdata =data.frame(x =X), interval = "confidence")
      
      
      pred<-as.data.frame(pred)
      pred$x=X
      select(pred, x, pred=Prediction, Lower, Upper)
    }else{
      modelr::add_predictions(data.frame(x = exp(seq(-20,10, length.out = 100))), x)
    }
    
  }

  coefs.fun <- function(x) {
   
    dplyr::tibble(names = names(coef(x)), x = unname(coef(x))) -> coefs

    coefs[params,2]<-exp(coefs[params,2])
    coefs
  }

  table.fun <-function(x) {
    x %>% rowwise %>%
      dplyr::mutate(names=names %>% stringr::str_split_fixed(., pattern=":", n=2) %>% .[1]) %>%
      tidyr::spread(names, x)
  }

  df %>% dplyr::group_nest(compound) %>%
    dplyr::mutate(drmod = purrr::map(data, drm.func),
           pred  = purrr::map(drmod, predict.fun),
           coefs = purrr::map(drmod, coefs.fun),
           table = purrr::map(coefs, table.fun))
}
