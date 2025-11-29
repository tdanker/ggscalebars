
  #file.copy(ephysdata::examplefile("herg"), to=".")
  ephysdata<-read_PATCHMASTER("2014-07-22_CHO hERG.dat", cache_rerun = TRUE)
  
  file <- ephysdata$ptrs[[1]]$file
  con=file(file, "rb")
  
  tr=1;  plot(getTrace_(con,ptr = ephysdata$ptrs[[tr]]$trc., start = 0, n=NA), type = "l")
  
  tr=1;  plot(getTrace_(con,ptr = ephysdata$ptrs[[tr]]$trc., start = 1100, n=100), type = "l")
  
  ephysdata$ptrs %>% purrr::map(\(ptr){getTrace_( con, ptr$trc.)}) -> ALL
  
  
  flatten_list_to_df <- function(lst) {
    n <- lengths(lst)
    data.frame(
      id = rep(seq_along(lst), times = n),
      pos = sequence(n),
      value = unlist(lst, use.names = FALSE)
    )
  }
  
  flatten_list_to_df_down <- function(lst, step = 1) {
    out <- lapply(seq_along(lst), function(i) {
      v <- lst[[i]]
      idx <- seq(1, length(v), by = step)
      if (length(idx)) {
        data.frame(
          id = i,
          pos = seq_along(idx),
          value = v[idx]
        )
      }
    })
    do.call(rbind, out)
  }
  
  
  flatten_list_to_df(ALL) %>% ggplot(aes(pos, value, group=id)) + geom_line()
  
  flatten_list_to_df_down(ALL, 20) %>% ggplot(aes(pos, value, group=id)) + geom_line()
  






