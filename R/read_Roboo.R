#' Title
#'
#' @param plate Roboocyte plate name
#' @param get_RNAinfo Boolean, if TRUE, try to read the RNA info from *.rpf file
#' @param get_exported_datatable specify if data should be read from the exported files or from the raw-data .r2d file. 
#' The default, "auto", will flexibly use the exported_datatable whenever it is found. Use TRUE or FALSE to force a specific behavior, 
#' and get an error if the specified file variant is not found.    
#' @param format_output do fancy formatting of the output (slower)
#' @param verbose if TRUE, will print diagnostic information about which file was found and used. 
#' @param cache_rerun if TRUE, don't use any cached results
#'
#' @return ephys-data
#' @export
#' @family ephys-data-readers
#' @examples
#' #' library(ephysdata)
#' read_ROBOO(ephysdata::examplefile("OO_GABA")) %>%  head(1) %>%  
#'  add_cursor_point(name = "peak",  39, 119, fun = min) %>%
#'  add_cursor_model(name = "exp",  110, 149, model_fun_exp) %>% 
#'  ggsweeps
read_ROBOO <- function(plate, get_exported_datatable="auto", get_RNAinfo=F, format_output=T, verbose=F, cache_rerun=F){
  # rpf -> log -> r2d / r2d.tmp  -> exp_datatable | exp_traces
  # 
  # (später mal: •	immer als erstes die rpf lesen weil auch dies interessant sein kann, selbst wenn noch kein r2d existiert)
  #     o	man sieht welche Oozyten injiziert wurden und welche recordings es gibt
  #     o	slot für logreader, um dies ggf weiter anzureichern )
  # •	dann r2d lesen wenn kein exp_datatable vorhanden ist 
  # •	exp_datable lesen wenn vorhanden
  #   o	get_RNAinfo = T|F
  #     	auto macht wenig Sinn, da rpf immer vorhanden 
  #     	T= warne, wenn Datei nicht vorhanden (sie ist zwar immer da aber trotzdem)
  # •	exp_datable lesen wenn vorhanden
  #   o	get_exported_datatable = auto|T|F
  #     	auto = lade, wenn Datei vorhanden 
  #     	T= warne, wenn Datei nicht vorhanden
  # •	sweeps können aus r2d oder aus exp_traces gelesen werden (parameter in ggsweeps, get_traces – falls wir das hinkrigen, ansonsten über Option)
  # o	read_raw = T|F|auto|(check) 
  #   	auto: wäre die Frage was geht schneller, idee ist nimm exported_traces wenn vorhanden ( falls das schneller ist ) 
  #   	T: warne, wenn Datei nicht vorhanden
  #   	F: nimm exported_datatable, warne wenn nicht vorhanden
  #   	(check): lade beides und wirf einen Fehler wenn sie unterschiedlich sind (gute Idee?)
  datatable_loaded=FALSE
  
  # try to find the file, given by the parameter plate
  if(verbose) print(plate)
  plate <- get_file(plate)
  if(verbose) print(plate)
  # remove any endings to get the bare plate name
  if(plate %>% stringr::str_ends(".r2d")){
    plate <- plate %>% stringr::str_remove(".r2d")
  }
  
  if(plate %>% stringr::str_ends("_Export_Datatable.dat")){
    plate <- plate %>% stringr::str_remove("_Export_Datatable.dat")
  }
  
  if(plate %>% stringr::str_ends(".rpf")){
    plate <- plate %>% stringr::str_remove(".rpf")
  }
  
  # error if any files that are required depending on the parameters given are missing
  if(get_RNAinfo ){
    if(!file.exists(paste0(plate, ".rpf"))){
      stop("file not found:", plate, ".rpf")
    }
  }
  
  if(isTRUE(get_exported_datatable) ){  
    stopifnot(file.exists(paste0(plate, "_Export_Datatable.dat")))
  }
  
  # decide if we should get the exported_datatable or read from raw
  if(isTRUE(get_exported_datatable) | (get_exported_datatable=="auto" && file.exists(paste0(plate, "_Export_Datatable.dat")))){
    if(verbose) print("reading from _Export_Datatable.dat")
    plate_exportfile <- paste0(plate, "_Export_Datatable.dat")
    plate_data <-read_ROBOO_exported_datatable(plate_exportfile)
    datatable_loaded=TRUE
  }else{
    if(verbose) print("reading from .r2d file")
    if(!file.exists(paste0(plate, ".r2d"))){
      stop("file not found:", plate, " extension:.r2d")
    }
    plate_data <- read_ROBOO_r2d(paste0(plate, ".r2d"), cache_rerun=cache_rerun)
  }
  
  
  if(get_RNAinfo){
    injectioninfo <- get_injection_info(plate) %>% mutate(file=basename(Plate), well=Well)
    
    plate_data <-
      left_join(plate_data, injectioninfo, by=c("file",  "well"))%>%
      select(-Plate, -Well) %>%
      #relocate(id, file, well, swp, starts_with("RNA.")) %>%
      tidyr::hoist(RNA, "RNA.Name", "RNA.Concentration") %>% relocate(RNA.Name, RNA.Concentration, .after=swp)
    
  }
  if(format_output & datatable_loaded)
    plate_data <- format_roboExport(plate_data)
  
  if(isTRUE(getOption("ephys4.ROBOO_ptrs_as_lists"))){
    plate_data$ptrs <- plate_data$ptrs %>% purrr::map(as.list)
  }
  
  if(datatable_loaded){
    plate_data %>% add_ptrs_class("ptrs_robo_from_exported_data")  
  }else{
    plate_data %>% add_ptrs_class("ptrs_robo")
  }
  
  
  
  
  
} 
