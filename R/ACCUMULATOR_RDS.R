#' function to add cursor results and metadata of an ephys plot to an "accumulator" data frame and seriealises this into an RDS file. 
#' `r lifecycle::badge("experimental")`
#' - WARNING: this does only work for oocytes! how to deal with that? 
#'   - CONS: VERYY slow. 
#'   - PROS: the DOTS mechanism is very handy, works also with no cursor results
#'   - Alternatives:
#'
#' @param . an typical ephys plot (ggplot object), were p$data has an attribute "meta"
#' @param to_file file to accumulate the results in
#' @param ... additional arguments which will generate extra columns in the accumulator file
check_in<-function(., to_file,...){
  
  
  match.call(expand.dots = FALSE)$`...`->DOTS
  
  
  
  
  
  .$data %>% attr("meta") %>% 
    mutate(plate=ptrs[[1]]$file) %>%
    mutate(...) ->df
  
  required_args<-list(
    df=as.symbol("df"),
    to_file=to_file
  )
  
  standard_columns=list( 
    plate=as.symbol("plate"), 
    OO=as.symbol("OO"), 
    run=as.symbol("run"), 
    swp=as.symbol("swp")
  )
  
  extra_columns <- names(DOTS)
  names(extra_columns) <- names(DOTS)
  
  
  do.call(update_in_place_from_df, c(required_args, standard_columns, extra_columns))
  .
}

#' function to remove data from accumulator
check_out<-function(., to_file="test.RDS",...){
  .$data %>% attr("meta") -> df
  readRDS(to_file) %>% filter(! id %in% df$id) %>% saveRDS(to_file)
}

# function that uses an RDS file as a mini-database
# primary key is id
# this function takes a single primary key and a name/value pair and creates or updates the entry accordingly
update_in_place<-function(file, id, name, value,...){
  if(! file.exists(file)){
    results<-data.frame()
  } else{
    results <-readRDS(file)
  }
  
  if(! NROW(results[results$id==id,])>0){
    
    results<-bind_rows(results, data.frame(id=id))
  }
  
  results[results$id==id,name]<-value
  saveRDS(file=file, object = results)
  invisible(results)
}





# function that pushes or updates entire data frames into accumulator files
# primary key is "id
update_in_place_from_df<-function(df, to_file, ...){
  match.call(expand.dots = FALSE)$`...`->DOTS
  
  df %>%  select(id, any_of(as.character(unlist(DOTS))))->df
  
  names(df)<-c("id", names(DOTS))
  df %>% mutate(file=to_file) %>%
    mutate(across(c(-id, -file), as.character)) %>%
    tidyr::pivot_longer(c(-id, -file)) %>% distinct %>%
    purrr::pwalk(update_in_place)
}




# this version of check_in stores just the plate/oo/run, plus any DOTS, so it is very leightweight. All analysis and plotting is thus done on the receiver side
# because it is so leightwieght, we can affor to read/store each time, so it becones very simpistic.
# the way we construct the list entry is the only thing that is specific to patch clamp and differs from oocyte work
check_in_oo <- function(., filename,...){
  
  filename<-paste0(getOption("check_in_oo_folder"), filename)
  
  match.call(expand.dots = FALSE)$`...`->DOTS
  if(is.null(DOTS)) DOTS <- data.frame(DOTS=NA)
  
  if(file.exists(filename)){
    aggregator <- readRDS(filename)
  }else{
    aggregator=list()
  } 
  
  
  experiment <- .$data %>% attr("meta") %>%  select(id,  run, plate2=plate) %>% mutate( id= id %>% str_remove("-[0-9]*$")) %>% separate(id, into=c("plate", "oo"), sep = "-") %>% distinct
  
  listname <- experiment   %>% paste(collapse = "-")
  
  aggregator[[listname]] <- data.frame(plate=experiment$plate2,  run=experiment$run, oo=experiment$oo) %>% bind_cols(as.data.frame(as.list(DOTS))) %>% select(-any_of("DOTS"))
  saveRDS(aggregator, filename)
  
  
  
  
  .
}



check_out_oo <- function(., filename,...){
  
  filename<-paste0(getOption("check_in_oo_folder"), filename)
  
  if(file.exists(filename)){
    aggregator <- readRDS(filename)
  }else{
    stop("this aggregator does not exist")
  } 
  
  
  experiment <- .$data %>% attr("meta") %>%  select(id,  run, plate2=plate) %>% mutate( id= id %>% str_remove("-[0-9]*$")) %>% separate(id, into=c("plate", "oo"), sep = "-") %>% distinct
  
  listname <- experiment   %>% paste(collapse = "-")
  
  aggregator[[listname]] <- NULL
  saveRDS(aggregator, filename)
  
  
  
  
  .
}

