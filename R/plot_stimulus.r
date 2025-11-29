plotStimulus<-function(x,y,inc=rep(0,length(y)),y2=y,inc2=inc,n=1,labels=c("side","middle")[1], scaleBar=T,
                      scaleBarUnit="ms",
                      main=NULL,
                      ...){
X_=0
for(phase in 1:length(x)){
 x_=x[phase]
 x[phase]=x[phase]+X_
 X_=X_+x_

}


Y=NULL                     #y will be a list of potentials to plot.
Y[(1:length(y))*2]=y2     #we start to modyfy y so that we have each y twice ( the start and end of the line
Y[(1:length(y))*2-1]=y   #now we have each potential twice

INC=NULL                      #inc will be a list of increases to plot.
  INC[(1:length(inc))*2]=inc2
  INC[(1:length(inc))*2-1]=inc



X=NULL                     #x will be a list of x-Axis points were potential changes
X[(1:length(x))*2]=x;X
X[(1:length(x))*2-1]=x;X   #now we have each x twice, too
X<-c(0,X);X                #adding a 0 in the beginning (first line starts at 0)
X<-rev(rev(X)[-1]);X       #cut away the last, which is not needed

inset=F
if(inset){
######### Inset plotting

vp <- gridBase::baseViewports()
grid::pushViewport(vp$inner,vp$figure,vp$plot)
# push viewport that will contain the inset
grid::pushViewport(viewport(x=0.1,y=0.9,width=.3,height=.3,just=c("left","top")))
# ...or just the plotting area (coordinate system)
   opar<-par()
   par(plt=gridPLT(),new=T)
}





all_y<-c(y+inc*n,y,y2,y2+inc2*n)
YLIM<-c(min(all_y)-diff(range(all_y))*0.5,max(all_y)+diff(range(all_y))*0.2)





    plot(X,Y, type="l",xlab="", ylab="",...,xlim=c(max(x)*-.2,max(x)*1.3),
          ylim=YLIM ,
          main=main,
          axes=F)


    for(N in 1:n){
        Y_=Y+(INC*(N-1))
      lines(X,Y_,...)
    }
    xpos=0
    for( phase in 1:length(x)){
      xposMID=xpos+(x[phase]-xpos)/2
      xposEND=xpos+(x[phase]-xpos)
      xposBEGIN=xpos
      if(inc[phase]<=0){pos=3}else{pos=1}
      if(inc[phase]<=0){pos2=1}else{pos2=3}
      if(inc2[phase]<=0){posRamp=3}else{posRamp=1}
      if(inc2[phase]<=0){pos2Ramp=1}else{pos2Ramp=3}

      if(labels=="side"){
        if(phase<length(x)){
           pos=2
           pos2=2
           xposMID<-xposBEGIN #shift label to Begin of segment
        }else{
           pos=4
           pos2=4
           xposMID<-xposEND #shift label to Begin of segment
        }
      }

      if(y2[phase]==y[phase] && inc[phase]==inc2[phase]){  #not a ramp
        if(phase==1){
          text(xposMID,y[phase],y[phase], cex=0.7,pos=pos)
        }else{
            if(!y[phase]==y[phase-1]){  #label at begin only if different from next segment
               text(xposMID,y[phase],y[phase], cex=0.7,pos=pos)
            }
        }
        if(!inc[phase]==0 & n>1){
          text(xposMID,y[phase]+inc[phase]*(n-1),y[phase]+inc[phase]*(n-1), cex=0.7,pos=pos2)
        }
       }else{ #Ramp
          if(!y2[phase]==y2[phase+1]){  #label at end only if different from next segment
            text(xposEND,y2[phase],y2[phase], cex=0.7,pos=posRamp)
          }
          if(!y[phase]==y[phase-1]){  #label at begin only if different from next segment
            text(xposBEGIN,y[phase],y[phase], cex=0.7,pos=pos)
          }

          if(!inc2[phase]==0 & n>1){
            text(xposEND,y2[phase]+inc2[phase]*(n-1),y2[phase]+inc2[phase]*(n-1), cex=0.7,pos=pos2Ramp)
          }
          if(!inc[phase]==0 & n>1){
            text(xposBEGIN,y[phase]+inc[phase]*(n-1),y[phase]+inc[phase]*(n-1), cex=0.7,pos=pos2)
          }
      }
      
      xpos=x[phase]
    }
    if(scaleBar){
      barY=YLIM[1]+diff(range(YLIM))*0.15
      barX=pretty(c(0,xpos),5)[2]

      lines(c(xpos-barX,xpos),c(barY,barY))
      text(mean(c(xpos-barX,xpos)), barY, paste(barX, scaleBarUnit), pos=1,offset=0.2, cex=0.85)
    }
    if(inset){
    # pop all viewports from stack
      grid::popViewport(4)
    par(opar)
    }
}


