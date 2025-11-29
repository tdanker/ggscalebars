
# relevant Fields in PM file: 

# SeMark               =   0; (* INT32 *)
# SeLabel              =   4; (* String32Type *)  
# SeComment            =  36; (* String80Type *)  ### also nice to use
# SeSeriesCount        = 116; (* INT32 *)         ### better than our way to number ? always unique ? 
# 
# 
# GrMark               =   0; (* INT32 *)
# GrLabel              =   4; (* String32Size *)
# GrText               =  36; (* String80Size *)  ### also nice to use
# GrExperimentNumber   = 116; (* INT32 *)         ### like E-803 ? 
# GrGroupCount         = 120; (* INT32 *)         ### like the number right to the label in PM GUI ???
# 
# 
# RoRootText           = 120; (* String400Type *) ### also nice to use


write_PM_label_or_text<-function(tree, file, exp=NULL, ser=NULL, swp=NULL, string, offset, maxlength){
  if( stringr::str_length( string ) > maxlength ) 
    warning("string is too long and will be truncated")
  string   <- stringr::str_trunc(string, maxlength )    
  
  path     <- c(file, exp, ser, swp)
  filename <- attr(tree[[c(file)]], "filename")
  ptr      <- attr(tree[[path]], "dataptr") + offset
  
  con      <- file(filename, "r+b")
    seek(con, where = ptr , rw="w")
    writeBin(string, con, endian = "little")

  close(con)
}



#' Write labels and textual comments to Patchmaster Files
#'
#' @param tree a treeinfo object ( see \link{get_treeinfo} )
#' @param file the number of the file in the tree
#' @param exp  the number of the experiment in the tree
#' @param ser  the number of the series in the tree
#' @param swp  the number of the sweep in the tree
#' @param label string to write as a label
#'
#' @name writelabels
NULL

#' @describeIn writelabels
#' writes experiment label
#' @export
write_explabel<-function(tree, file, exp, label){
  write_PM_label_or_text(tree, file, exp, string=label, offset=4, maxlength = 31)  # test maxlength=32
}

#' @describeIn writelabels
#' writes experiment text
#' @export
write_exptext<-function(tree, file, exp, label){
  write_PM_label_or_text(tree, file, exp, string=label, offset=36, maxlength = 79)  # test maxlength=80
}

#' @describeIn writelabels
#' write series label
#' @export
write_serlabel<-function(tree, file, exp, ser, label){
  write_PM_label_or_text(tree, file, exp, ser, string=label, offset=4, maxlength = 31)  # test maxlength=32, recode offset to be "4"
}

#' @describeIn writelabels
#' write sweep label
#' @export
write_swplabel<-function(tree, file, exp, ser, swp, label){
  write_PM_label_or_text(tree, file, exp, ser, swp, string=label, offset=4, maxlength = 31)  # test maxlength=32, recode offset to be "4"
}


