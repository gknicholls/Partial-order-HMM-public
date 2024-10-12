#example for Kevin - plotting the DOI U and Z averages
rm(list=ls())

showDOIs<-function(doi.plot,V.mean,V.sd,cil.plot,B,T,sV=NA,plot.U=TRUE,tau.pm.plot=NA) {
  n.doi.plot=length(doi.plot)
  par(mfrow=c(ceiling(n.doi.plot/2),2),oma=c(4,4,2,2))
  b2d=sapply(cil.plot,function(x){x$diocese})
  d2t=d2b=vector('list',n.doi.plot)
  x.vals=B+(1:T)-1
  if (plot.U) {
    ysc.max=3    
    ysc.min=-3
  } else {
    ysc.max=max(V.mean,na.rm=TRUE)
    ysc.min=min(V.mean,na.rm=TRUE)
  }
  for (k in 1:n.doi.plot) {
    d2b[[k]]=grep(doi.plot[k],b2d)
    par(mai=c(0,0,0,0))
    if (k!=n.doi.plot && k!=(n.doi.plot-1)) {par(xaxt='n')} else {par(xaxt='s')}
    if (k%%2==0) {par(yaxt='n')} else {par(yaxt='s')}
    if (plot.U) {
      plot(x.vals,rep(0,T),ylim=c(ysc.min,ysc.max),type='n',yaxp=c(-2,2,4))
    } else {
      plot(x.vals,rep(0,T),ylim=c(ysc.min,ysc.max),type='n')  # #,yaxp=c(-2,2,4))
    }
    if (length(d2b[[k]])>0) {
      count=1; apply(V.mean[d2b[[k]],,drop=FALSE],1,function(y){lines(x.vals,y,col=count);count<<-count+1})
      count=1; apply(V.mean[d2b[[k]],,drop=FALSE]+V.sd[d2b[[k]],,drop=FALSE],1,function(y){lines(x.vals,y,lty=2,col=count);count<<-count+1})
      count=1; apply(V.mean[d2b[[k]],,drop=FALSE]-V.sd[d2b[[k]],,drop=FALSE],1,function(y){lines(x.vals,y,lty=2,col=count);count<<-count+1})
      if (!is.na(sV[1])) {count=1; apply(sV[d2b[[k]],,drop=FALSE],1,function(y){lines(x.vals,y,col=count,lwd=2);count<<-count+1})}
      
      cilk=cil.plot[d2b[[k]]]
      x.names=sapply(cilk,function(x) min(x$end,B+T-1))
      long.names=sapply(cilk,function(x) x$name)
      short.names=sapply(strsplit(long.names,','),function(x) x[1])
      for (j in 1:length(short.names)) text(x.names[j],2/3*ysc.min,short.names[j],col=j,adj=c(1,0),cex=0.8)

      if (!is.na(tau.pm.plot[1])) {
        count=1; 
        lapply(d2b[[k]],
          function(y) {
            ptp=B-1+tau.pm.plot[cil.plot[[y]]$ln]
            points(ptp,0*ptp+0.97*ysc.max,pch=16,col=count,cex=0.7);
            count<<-count+1
          }
        )
      }
    }
    text(B-T*1.5/50,ysc.min,doi.plot[k],adj=c(0,0),cex=1); abline(h=0,lty=3,col=6)
  }
}

#load example data
load('PlotDioEx.RData')

## objects except tau.pm
ls()
#[1] "B"        "cil"      "cla"      "showDOIs" "T"        "tau.pm"   "U.mean"   "U.sd"  

#B=1080

#T=1155-1080

#cil and cla you know

#showDOIs is the function above - hopefully there is no need to look inside it!

#U.mean is I think like your PL skill matrix - if there are NB=59 bishops and the length of 
#the interval is NY=T-B+1 (so includes years T and B) then U.mean is an NB x NY matrix
#for each bishop b=1,...,NB and each year y=1,...,NY entry U.mean[b,y] gives the skill measure 
#for bishop b in year y (and NA if they were not around then).

#U.sd[b,y] gives the marginal posterior standard deviation of U.mean[b,y] - same form as U.mean
##

## tau.pm
#list times are uncertain - I use posterior mean - for tau.pm use (tu+tl)/2 if no tau post
#which I think you dont have? This is the prior mean and almost exactly the same as post mean anyway

tau.pm=sapply(cla,function(x) (x$tu+x$tl)/2 )-B+1 
##


## if you want to plot the truth over the top
#if synthetic data with true U/Z then this is the true U (or Z) with the same format as U.mean and Z.mean

sU=NA 
##

## select diocese you want to plot
#doi.plot is a character vector allowing you to restrict to just plotting a subset of diocese

doi.plot.all=unique(sapply(cil,function(x) {x$diocese})) #plot them all
##

#main plot - the small dots show the times of the lists the bishop was in
showDOIs(doi.plot=doi.plot.all,V.mean=U.mean,V.sd=U.sd,cil.plot=cil,B,T,sV=sU,tau.pm.plot=tau.pm)

#example subset of diocese
doi.plot.some=c("Salisbury","Winchester","London","Lincoln") #just plot top 4 powerful ones
showDOIs(doi.plot=doi.plot.some,V.mean=U.mean,V.sd=U.sd,cil.plot=cil,B,T,sV=sU,tau.pm.plot=tau.pm)

#example true data - this example wasnt synthetic so just make one artificially for this example
sU=U.mean+0.2*rnorm(prod(dim(U.mean)))
showDOIs(doi.plot=doi.plot.all,V.mean=U.mean,V.sd=U.sd,cil.plot=cil,B,T,sV=sU,tau.pm.plot=tau.pm)


