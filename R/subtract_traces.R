#' subtract_traces
#'
#' @param a 
#' @param b 
#'
#' @return ephys-data
#' @export
#'
#' @examples
#' a<-read_PATCHMASTER("VG_Blocker.dat", exp = 1, ser = 2) %>% filter(swp==6)
#' b<-read_PATCHMASTER("VG_Blocker.dat", exp = 1, ser = 2) %>% filter(swp==12)
#' subtract_traces(b,a) ->x 
#' x %>% ggsweeps() + ggtitle("b -a")
#' bind_rows(a,b, x) %>% ggsweeps(mapping=aes(col=swp)) 
subtract_traces<-function(a,b){
  traces_a <- a %>% get_trace()
  traces_b <- b %>% get_trace()
  diffswp<- a %>% coldiff(b, exp) %>% coldiff(b, ser) %>% coldiff(b, swp) %>% coldiff(b, trc) %>% mutate(id=paste(id, "*"))
  diffswp$data <- list(data.frame(x=traces_a$x, y=traces_a$y-traces_b$y)) 
  diffswp
}



coldiff <- function(a, b, col) a %>% mutate( {{col}} := if_else({{col}} == b %>% pull({{col}}), 
                                                                as.character({{col}}), 
                                                                #forcats::as_factor(
                                                                paste(
                                                                  a %>% pull({{col}}),  
                                                                  b %>% pull({{col}}), sep="-"
                                                                )
                                                                #)
)
) %>% mutate({{col}}:=as.factor({{col}}))  


