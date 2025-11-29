

get_file <- function(filename){
  # # if we have a file with the given name in the current folder, this is prioritised
  # if(file.exists(filename)) return(filename)
  # 
  # path= paste0(getOption("data_files_path"), filename)
  # if(file.exists(path)) return(path)
  # 
  # path= paste(getOption("data_files_path"), filename, sep="/")
  # if(file.exists(path)) return(path)
  # 
  # stop(paste("could not find file", filename))
  
  # make sure that all paths end with a slash
  PATHS <- unlist(options("data_files_path")) %>% stringr::str_remove("/$") %>% paste0("/")
  
  # make sure that the current folder and the unmodified path are always in PATHS
  PATHS <- c("",  PATHS) %>% unique
  # make sure that we do not have "./", which is the same as "". 
  PATHS <- PATHS[PATHS!="./"]
  
  found_paths<-
    tibble(path=PATHS) %>% 
    mutate(path=path %>% paste0(filename)) %>% 
    mutate(found=file.exists(path)) %>% 
    filter(found)
  if(NROW(found_paths)>1) warning("file was found in more than one place. Type '? set_file_searchfolder' to get help")
  if(NROW(found_paths)<1) stop(paste("could not find file. Type '? set_file_searchfolder' to get help", filename))
  found_paths %>%  head(1) %>% pull(path)
}

#' filehelpers
#' 
#' @name  filehelpers
#'
#' @param path  file paths where your data is stored 
#' 
#' set folders where data files are stored
#' 
#' a typical use case is to do:
#' set_file_searchfolder(list("./", "../", "../../")
#' this will find the data file also if it is in a parent folder. 
#' 
#' or, if the data files are in a specific directory in your project, named "datafiles":
#' 
#' set_file_searchfolder(list(rprojroot::find_rstudio_root_file("datafiles")))
#'
#' @export
set_file_searchfolder <- function(path){
  options(data_files_path = path)
}

#' @describeIn filehelpers list your HEKA files
#' @export
list_HEKA_files<-function(){
  data_file_path <- getOption("data_files_path")
  if(is.null(data_file_path)){
     cat("there is no file path set. \n You can set it using 'set_file_searchfolder()'. ")  
     local_files <- list.files(path = ".", pattern = "\\.dat$")
     if(length(local_files)==0){
       cat("\n there are also no HEKA files in your current working directory. ")
       return()
     }else{
       cat("\n here are some possible HEKA files in your current working directory: ")
       return(local_files)
     }
     
  }
    
  list.files(data_file_path, pattern = "\\.dat$")
} 

