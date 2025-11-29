getTrace_optimized <- function(con, ptr, start = 0, n = NA, read_data = TRUE, name = "", con_dat_path = "") {
  SIZE <- 2
  
  seek(con, ptr + 40)
  offset <- readBin(con, "integer", size = 4, endian = "little")
  nDatapoints_ <- readBin(con, "integer", size = 4, endian = "little")
  
  nDatapoints <- nDatapoints_ - start
  if (!is.na(n)) {
    nDatapoints <- min(nDatapoints, n)
  }
  
  seek(con, ptr + 96)
  Unit <- readBin(con, "character", n = 1)
  Unit_ <- if (Unit == "V") 1000 else 1e9
  
  seek(con, ptr + 72)
  DataScaler <- readBin(con, "double", size = 8)
  
  seek(con, ptr + 104)
  Xinterval <- readBin(con, "double", size = 8)
  
  if (read_data) {
    trace <- getTraceCpp(con_dat_path, offset, start, nDatapoints_, TRUE, n, Unit, DataScaler, Xinterval)
  } else {
    trace <- NA
  }
  
  attr(trace, "Xinterval") <- Xinterval
  attr(trace, "nDatapoints_") <- nDatapoints
  attr(trace, "name") <- name
  trace
}
