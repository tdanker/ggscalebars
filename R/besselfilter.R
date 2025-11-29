#' Bessel filters
#'
#' 
#' @name besselfilter
#' 
#' @param x a vector to be filtered
#' @examples
#' options(data_files_path = ephysdata::get_examples_path())
#'read_PATCHMASTER("VG_Blocker.dat", exp = 1, ser = 2) %>% filter(swp %in% 1:6) %>% 
#'  add_cursor_point(name = "peak", start=.01,end=.012, fun = min, filter_fun =  bf0.2)  %>%
#'  ggsweeps( filter_fun = bf0.2)
NULL
#> NULL



#' @rdname besselfilter
#' @export
bf0.1=function(x) signal::filter(besself_string(" 3.88858936e-004 1.55543574e-003 2.33315362e-003 1.55543574e-003 3.88858936e-004
 1.00000000e+000 -3.06594882e+000 3.57378745e+000 -1.87441156e+000 3.72794676e-001"), x)

#' @rdname besselfilter
#' @export
bf0.2=function(x) signal::filter(besself_string(" 4.28742029e-003 1.71496812e-002 2.57245218e-002 1.71496812e-002 4.28742029e-003
 1.00000000e+000 -2.21797364e+000 1.97707284e+000 -8.25112417e-001 1.34611942e-001
"), x)

#' @rdname besselfilter
#' @export
bf0.5=function(x) signal::filter(besself_string(" 7.86375192e-002 3.14550077e-001 4.71825115e-001 3.14550077e-001 7.86375192e-002
 1.00000000e+000 1.21331300e-002 2.52968984e-001 -1.21331300e-002 5.23132309e-003
"), x)

#' @rdname besselfilter
#' @export
bf0.01=function(x) signal::filter(besself_string(" 5.79912375e-008 2.31964950e-007 3.47947425e-007 2.31964950e-007 5.79912375e-008
 1.00000000e+000 -3.90234020e+000 5.71129170e+000 -3.71546641e+000 9.06515834e-001
"),x)

#' @rdname besselfilter
#' @export
bf0.02=function(x) signal::filter(besself_string(" 8.84603631e-007 3.53841453e-006 5.30762179e-006 3.53841453e-006 8.84603631e-007
 1.00000000e+000 -3.80564318e+000 5.43376612e+000 -3.44985354e+000 8.21744752e-001
"),x)

#' @rdname besselfilter
#' @export
bf0.05=function(x) signal::filter(besself_string(" 3.00983359e-005 1.20393344e-004 1.80590016e-004 1.20393344e-004 3.00983359e-005
 1.00000000e+000 -3.52128953e+000 4.66464099e+000 -2.75465658e+000 6.11786688e-001
"),x)

#' @rdname besselfilter
#' @export
bf0.001=function(x)  signal::filter(besself_string( " 6.05829398e-012 2.42331759e-011 3.63497639e-011 2.42331759e-011 6.05829398e-012
      1.00000000e+000 -3.99019067e+000 5.97061529e+000 -3.97065847e+000 9.90233850e-001
 "),x)



#' @rdname besselfilter
#' @export
bf0.002=function(x) signal::filter(besself_string(" 9.64595115e-011 3.85838046e-010 5.78757069e-010 3.85838046e-010 9.64595115e-011
 1.00000000e+000 -3.98039097e+000 5.94134578e+000 -3.94151785e+000 9.80563046e-001
"),x)

#' @rdname besselfilter
#' @export
bf0.005=function(x) signal::filter(besself_string(" 3.71323639e-009 1.48529456e-008 2.22794184e-008 1.48529456e-008 3.71323639e-009
 1.00000000e+000 -3.95104968e+000 5.85422468e+000 -3.85528808e+000 9.52113148e-001
"),x)


besself_string<-function(s){
  coefs<-get_coefs(s)
  signal::Arma(
    b=as.vector(coefs[1,],'numeric'),
    a=as.vector(coefs[2,],'numeric')
  )
}

get_coefs<-function(s){
  # Split into individual numbers (by space or newline)
  numbers <- as.numeric(unlist(strsplit(s, "\\s+")))
  
  # Remove any potential empty elements
  numbers <- numbers[!is.na(numbers)]
  
  # Convert to a matrix with 2 rows and 5 columns
  matrix(numbers, nrow = 2, byrow = TRUE)
  
}


besself<-function(coef_file){
  
  coefs<-read.csv(system.file("data", coef_file, package = "ephys4"), head=F, sep=" ")
  signal::Arma(
    b=as.vector(coefs[1,-1],'numeric'),
    a=as.vector(coefs[2,-1],'numeric')
  )
}
