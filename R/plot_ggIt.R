# gg_itplot<-function(series, cursorname, minutes=F, sweeps=NA, colors=rep("grey",30)){}
#   require(ggEphysAnnotations)
#   
#   results=series$results()
#   
#   if(!is.na(sweeps))
#     results <- results[sweeps,]
#   
#   if(minutes){
#     xlabel="minutes"
#     results<- mutate(results, relTime=relTime/60)
#   }else{
#     xlabel="seconds"
#   }
#   results<- mutate(results,Concentration =factor(Concentration)) 
#   labels_Y=max(results[cursorname])*1.05
#   drcresults<-get_lpresults2(results, cursor = cursorname)
#   
#   ggplot(results, aes_string(y=cursorname, x="relTime")) + 
#     geom_rect(data=drcresults, aes(y=res, ymin=-Inf,ymax=Inf, x=relTime.start, xmin=relTime.start, xmax=relTime.end,fill=Concentration), alpha=.15) +
#     geom_text(data=drcresults, aes(y=labels_Y, x=relTime.start+ (relTime.end-relTime.start)*.5, label=Concentration), hjust=.5) +
#      
#     geom_line() + xlab(xlabel)+
#     geom_segment(data=drcresults, aes(y=res,yend=res, x=relTime.start,  xend=relTime.end,color=Concentration),  size=2, alpha=1)+
#     geom_point() +
#     scale_color_manual(values=colors)+ 
#     scale_fill_manual(values=colors)+
#     theme(legend.position = "none")
#     
# }
