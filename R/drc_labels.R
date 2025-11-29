

drc_labels<-function(model, sep=":", unit="\u03BCM"){
  model<-mutate(model, compound=stringr::str_trim( compound ))
  model<-mutate(model, compound=stringr::str_pad( compound,max(stringr::str_length(compound)), side="right" ))
  model %>% drc_table( ) %>% purrr::pmap( ~bquote(
    .(..1) ~ .(sep)  ~ .(..3) ~ .(unit)
  ))
}

# drc_labels<-function(model){
#   model%>% unnest(coefs) %>% filter(names == "IC50:(Intercept)") -> IC50values
#   lapply(1:nrow(IC50values),
#          function(x){
#            as.expression(bquote(
#              .(as.character(IC50values[x,]$compound)) ~
#                IC[50]: ~
#                .(format(IC50values[x,]$x, digits=3)) ~
#                "µM"
#            ))
#
#          })
# }
