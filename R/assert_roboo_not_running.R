# this helper should be called whenever we read Roboocyte files, be it .log, .r2d or similar. 
# this ensures that the robocyte is not running and we are not making the roboocyte crash. 
# it will not ask again within 1 minute. 
assert_robo_not_running<-function(){
  return(NULL) # this is not really ready to use, so it stays inactive for a while
  if(!exists("Robo_confirmed_not_running_timestamp")) Robo_confirmed_not_running_timestamp <<- Sys.time() - lubridate::hours(10)
  
  if( Sys.time() - Robo_confirmed_not_running_timestamp > lubridate::minutes(1) ){
    if( !  try(yesno::yesno2("You are about to read Log data - Are you sure the Roboocyte is not running a script ?", yes = "Yes I am sure!")) ==1 ){
      cli::cli_abort("Aborted because you said the Roboocyte might be running")
      
    }
    
    
  }
  # reset timer:
  Robo_confirmed_not_running_timestamp <<- Sys.time()
}
