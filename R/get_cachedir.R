get_cachedir<-function(cache_name){
  default_cache_basedir <- here::here("tmp/")
  cache_basedir         <- getOption("cache_basedir", default= default_cache_basedir )
  cachedir              <- getOption(cache_name, default=file.path(cache_basedir , cache_name))
  
  
  # make sure that we end with exactly 1 trailing "/":
  paste0(file.path(cachedir), "/")
}
