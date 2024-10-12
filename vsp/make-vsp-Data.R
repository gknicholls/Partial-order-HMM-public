
rm(list=ls())

library(MASS)

#WAWW
note=list()
note$RUNDIR="C:/Users/nicholls.NICHOLLS2389/Documents/collab - Kate/newPO/working9"
note$VERBOSE=FALSE

#data setup
note$B=1134    #1080              #cut data to after this year or equal
note$E=1138    #1084               #cut data to before this year or equal
if (note$B>note$E) stop('B before E')
note$justbishops=FALSE
  
#model setup
note$selectlists='all'       #fraction of list time which must overlap target interval 'strict' is all, 'half' is 50% and 'any' is any 
note$maxlistlength=Inf       #what list lengths allowed? NA or Inf is everything useful. Set to say 14 to knock out a few very long lists if speed an issue
note$min.lists.per.bishop=2  #ignore references to "bishops" this everyone - set this higher to drop people who dont appear in many lists
note$removebadlists=FALSE

setwd(note$RUNDIR)
#old work functions including alot of legacy stuff need to be in RUNDIR
source(file="dating.R")

#load and form the data
#time window
  
B=note$B;E=note$E; T=E-B+1
source(file="./vsp/makealldata.R",verbose=note$VERBOSE)

#datafile=paste("./vsp/datafile-all-",B,"-",E,".RData",sep='')
#save(dla,dil,file=datafile)

# list the list lengths
sapply(dla,function(x) x$ll)
# list the number of lists each individual appears in - will be at least "note$min.lists.per.bishop"
sapply(dil,function(x) x$ll)

#visiualise lists
workL=dla
nl=length(workL)
y1=sapply(workL,function(x) x$year.1); i=order(y1); y1=y1[i]
y2=sapply(workL,function(x) x$year.2)[i]
#windows(); 
plot(0,0,xlim=c(min(y1)-1,max(y2)+1),ylim=c(0,nl+1),xlab="Dated interval (years AD)",ylab="List index")
for (j in 1:nl) {lines(c(y1[j],y2[j]),c(j,j))}
abline(v=c(1067,1087,1101,1135,1154),col=3)

#windows(12,6)
plot((y1+y2)/2,sapply(workL,function(x) x$ll)[i],type="h",xlab="Midpoint of list time range",ylab="list length")
points(jitter((y1+y2)/2),jitter(sapply(workL,function(x) x$ll)[i]),pch='.',col=2,cex=3) 
abline(v=c(1067,1087,1101,1135,1154),col=3); abline(v=seq(1050,1180,10),col=2,lty=2)
workB=dil
ll<-sapply(workB,function(x) x$ll)
#windows(); 
hist(ll,breaks=seq(1,max(ll+1),1)-0.5,main='',xlab='number lists per bishops')

workB<-workB[ll>0]
#dib<-factor(sapply(workB,function(x) x$diocese))
nb=length(workB)
fr=sapply(workB,function(x) x$by); i=order(fr); fr=fr[i]
to=sapply(workB,function(x) x$till)[i]
#windows(); 
par(mai=c(1,2,0.5,0.25));
plot(0,0,xlim=c(min(fr)-1,max(to)+1),ylim=c(0,nb+1),ylab="",xlab="Dated interval (years AD)",yaxt="n")
for (i in 1:nb) {lines(c(fr[i],to[i]),c(i,i))}#,col=dib[i])}
label.bishops=sapply(workB, function(x) x$name)
axis(2, at=1:nb,label=label.bishops,cex.axis=0.4,las=2,tick=FALSE)

#display bishop P/A in lists
workL=La
ll=sapply(workL,function(x) x$ll)
#windows(); 
hist(ll,breaks=0:max(ll+1),main='',xlab='number of bishops per list')

if (FALSE) {
  workL=workL[ll>1]
  nms=unique(unlist(sapply(workL,function(x) x$names)))
  nb=length(nms); nl=length(workL)
  bli=matrix(NA,nb,nl); 
  Dl=matrix(NA,nl,nl); ml=matrix(NA,1,nl)
  Db=matrix(NA,nb,nb); mb=matrix(NA,1,nb)
  for (i in 1:nb) {for (j in 1:nl) {bli[i,j]=any(nms[i]==workL[[j]]$names) }}
  for (i in 1:nl) {ml[i]=workL[[i]]$year.1} 
  pthl=order(ml); bli=bli[,pthl]
  for (i in 1:nb) {mb[i]=median(which(bli[i,]))} 
  pthb=order(mb); bli=bli[pthb,]
  #windows(); 
  par(mai=c(0.5,2,0.1,0.1)); image(1-t(bli),xaxt='n',yaxt='n');
  axis(1, at=0.5,label="List index",cex.axis=1,las=1,tick=FALSE)
  axis(2, at=seq(0,1,length.out=nb),label=nms[pthb],cex.axis=0.4,las=2,tick=FALSE)
}

#MLE like graph
source('modelfun.R'); source('pofun.R')
mc=legaldag2(ed=dla,NB=length(dil))
showDAG(transitive.reduction(mc),edge.arrow.size=0.2,vertex.color=NA,vertex.frame.color=1,vertex.label.cex=0.75,vertex.size=8)
is.vsp(mc)
