
#######################################
#Visualisation tools follows from makedata.R data registration
#take a look at what we have left

#do.pdf=FALSE

if (!exists('note')) {
  note=list(); note$VERBOSE=FALSE;
  note$doi.core=sort(c("Worcester","London","Winchester","Hereford",
                "Chichester","Durham","Chester","Lincoln","Salisbury","St Davids",
                "Evreux","Ely","Carlisle","Lisieux","Sees","Norwich","Bayeux","Avranches","Coutances","Rochester"),decreasing=TRUE)
#note$doi=c("Lincoln", "Durham", "Chester", "Sherborne", "Winchester", "Chichester", "Bayeux", "Lisieux","Evreux",
#           "Sees", "Avranches", "Coutances", "Exeter", "London", "Rochester", "Worcester","Salisbury", "Bath",
#           "Thetford", "Wells", "Le Mans", "Hereford", "Bangor", "Ely",
#           "St Davids", "Norwich", "Carlisle", "St Asaph", "Tusculum", "the Orkneys", "Llandaff")

  note$doi=c(note$doi.core,sort(c("Sherborne", "Exeter","Bath",
           "Thetford", "Wells", "Le Mans", "Bangor", "St Asaph", "Tusculum", "the Orkneys", "Llandaff"),decreasing=TRUE))

  note$bishopdatefile="BishopDates-25-7-22b.csv" #"BishopDates-4-11-19.csv"
  note$selectlists='any'
  note$removebadlists=TRUE
  note$min.lists.per.bishop=1
}

source(file="dating.R")
source("makedatafun.R")

#load and form the data
#time window
note$B<-B<-1050; note$E<-E<-1175; T=E-B+1 #this will simply capture all the data
D=makedata(note,B,E,T)
bla=D$cla; bil=D$cil; doi=D$doi

if (do.pdf) pdf(file='v2-lists-and-bishopsb.pdf',6,8) else windows(6,8)
par(mfrow=c(3,7),xpd=NA)
plot.dio.dates(bil,doi,packed=TRUE,note=note)
if (do.pdf) dev.off()

V=getX(doi,bil,B=B,E=E)
Dcp=(V$tio<=0) #Dcp[d,t] is TRUE if either no bishop or new bishop in diocese d in year t

if (do.pdf) pdf(file='v2-diocese-activityB.pdf',10,6) else windows(10,6)
#windows(10,6); 
par(mai=c(1,1.25,1,0.5),xpd=TRUE); 
image(t(Dcp),xaxt='n',yaxt='n',col=gray.colors(2, start = 0.4, end = 0.9, gamma = 2.2, alpha=1, rev = FALSE));
xt=seq(1,length(colnames(V$drk)),4)
axis(1, at=seq(0,1,length.out=length(xt)),label=colnames(V$drk)[xt],cex.axis=0.9,las=2)
drop.col=1
y.cols=rep(drop.col,nd<-length(row.names(V$drk)))
y.cols[match(note$doi.core, row.names(V$drk))]<-1
axis(2, at=seq(0,1,length.out=nd),labels=FALSE)
text(rep(0,nd),seq(0,1,length.out=nd),row.names(V$drk),cex=0.8,col=y.cols,pos=2,offset=1)
axis(4, at=seq(0,1,length.out=nd),labels=FALSE)
#count number of lists witnessed by each diocess
ND=length(doi); dc=rep(0,ND); NL=length(bla); for (j in 1:NL) {do=bla[[j]]$do; dc[do]<-dc[do]+1}; dio.list.counts=dc
text(rep(1,nd),seq(0,1,length.out=nd),dio.list.counts,cex=0.8,col=y.cols,pos=4,offset=1)
#boi=V$boi
if (do.pdf) dev.off()

if (do.pdf) pdf(file='v2-list-time-length2.pdf',10,5) else windows(10,6)
workL=bla
y1=sapply(workL,function(x) x$year.1); i=order(y1); y1=y1[i]
y2=sapply(workL,function(x) x$year.2)[i]
plot((y1+y2)/2,sapply(workL,function(x) x$ll)[i],
     type="h",xlab="Midpoint of list time range",ylab="list length",lty=1,lwd=1)
points(jitter((y1+y2)/2,factor=1.5),jitter(sapply(workL,function(x) x$ll)[i]),pch='x',col=1,cex=0.8) 
abline(v=c(1067,1087,1101,1135,1154),col=1,lty=2,lwd=1); #abline(v=seq(1050,1170,10),col=1,lty=3)
text(c(1068,1085,1102,1133,1155),rep(12.5,5),c('William I','William II','Henry I','Stephen','Henry II'),
     srt=90,pos=1)#c(1,2,1,2,2),offset=5)#c(1,10,3,4,5))
if (do.pdf) dev.off()

#
if (do.pdf) pdf(file='v2-list-per-bishp-per-list.pdf',10,4) else windows(10,4); 
par(mfrow=c(1,2))
workB=bil
llb<-sapply(workB,function(x) x$ll)
hist(llb,breaks=seq(0,max(llb+2),8),main='',xlab='number of lists per bishops')

#display bishop P/A in lists
lll=sapply(workL,function(x) x$ll)
hist(lll,breaks=0:max(lll+1),main='',xlab='number of bishops per list',ylab='')
if (do.pdf) dev.off()

#visiualise list intervals and bishop intervals
{if (do.pdf) pdf(file='v2-list-bishop-intervals.pdf',10,6) else windows(10,6)
par(mfrow=c(1,2),oma=c(0,0,0,5))

workB=bil
workB<-workB[llb>0]
#dib<-factor(sapply(workB,function(x) x$diocese))
nb=length(workB)
fr=sapply(workB,function(x) x$from); i=order(fr); fr=fr[i]
to=sapply(workB,function(x) x$to)[i]
#windows(); 
par(mai=c(1,2,0.5,0),xpd=TRUE);
plot(0,0,xlim=c(min(fr)-1,max(to)+1),ylim=c(0,nb+1),ylab="",xlab="Dated interval (years AD)",yaxt="n",axes=FALSE)
axis(1,col=1)
for (i in 1:nb) {lines(c(fr[i],to[i]),c(i,i))}#,col=dib[i])}
label.bishops=sapply(workB, function(x) x$name)
text(fr,1:nb,label.bishops,cex=0.4,pos=2)
#axis(2, at=1:nb,label=label.bishops,cex.axis=0.4,las=2,tick=FALSE)
par(xpd=FALSE);
abline(v=seq(1060,1190,20),col=2,lty=3) #c(1067,1087,1101,1135,1154)

workL=bla
nl=length(workL)
par(mai=c(1,0.5,0.5,1));
plot(0,0,xlim=c(min(y1)-1,max(y2)+1),ylim=c(0,nl+1),ylab='',xlab="Dated interval (years AD)",yaxt='n',axes=FALSE)
axis(4,col=NA,col.ticks=1); mtext("List index", 4, line = 2)
axis(1,col=1)
for (j in 1:nl) {lines(c(y1[j],y2[j]),c(j,j))}
abline(v=seq(1060,1190,10),col=2,lty=2)
if (do.pdf) dev.off()
}
##

##
windows()
workL=workL[lll>1]
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
par(mai=c(0.5,2,0.1,0.1)); image(1-t(bli),xaxt='n',yaxt='n');
axis(1, at=0.5,label="List index",cex.axis=1,las=1,tick=FALSE)
axis(2, at=seq(0,1,length.out=nb),label=nms[pthb],cex.axis=0.4,las=2,tick=FALSE)



##visualise relations between diocese, not very interesting
#workB=cil
#bid=sapply(workB,function(x) x$diocese)
#dn=unique(bid)
#nd=length(dn)
#nb=length(workB)
#Mm=R=C=P=M=matrix(0,nd,nd,dimnames=list(c(),dn))
#for (i in 1:nd) {
#  bidi=which(sapply(workB,function(x) x$diocese)==dn[i])
#  lidi=unlist(sapply(workB[bidi],function(x) x$lid))
#  for (j in 1:nd) {
#    bidj=which(sapply(workB,function(x) x$diocese)==dn[j])
#    lidj=unlist(sapply(workB[bidj],function(x) x$lid))
#    M[i,j]=length(intersect(lidi,lidj))/length(union(lidi,lidj))
#    C[i,j]=length(intersect(lidi,lidj))
#  }
#}
#for (i in 1:nd) {
#  for (j in 1:nd) {
#    Mm[i,j]=1-sum(xor(M[i,-c(i,j)]>0,M[j,-c(i,j)]>0))/sum( ((M[i,-c(i,j)]>0)|(M[j,-c(i,j)]>0)))
#  }
#}
#M<-fix(M); C<-fix(C);Mm<-fix(Mm)
#pic(M,labels.graph=dn,labels.image=dn)
#pic(Mm,labels.graph=dn,labels.image=dn)

if (FALSE) {
  #attempt to cluster bishops - not very successful
  library(PairViz)
  
  pic<-function(V,labels.graph=lab1,labels.image=lab2) {
    #plot cooccurence matrix/similarity matrix as graph and image with sorted columns
    
    g1 <- graph_from_adjacency_matrix(V,weighted=TRUE,mode="undirected",add.colnames=TRUE)
    windows(); plot(g1,vertex.size=4,vertex.label=labels.graph,
                    vertex.label.family="sans",vertex.label.cex=0.6)
    
    if (max(V)<=1) {
      Mz=0.999*V; Mz[V==0]<-min(V[V>0]); d=as.dist(Md<--log(Mz))
    } else {
      Mz=V; Mz[V==0]<-0.001; d=as.dist(Md<-1/(Mz)^(1/4))
    } 
    pth=order_tsp(d, method = "nearest", cycle=FALSE,improve=TRUE,path_dir = path_cor)
    windows(); par(mai=c(1,2,1,0)); image(Md[pth,pth],xaxt='n',yaxt='n');
    axis(2, at=seq(0,1,length.out=length(labels.image)),label=labels.image[pth],cex.axis=0.4,las=2)
    
  }
  
  #visiualise relations between bishops
  workB=bil
  ll<-sapply(workB,function(x) x$ll)
  workB<-workB[ll>1]
  nb=length(workB)
  Mm=R=C=P=M=matrix(0,nb,nb,dimnames=list(c(),sapply(workB, function(x) x$diocese)))
  for (i in 1:nb) {
    for (j in 1:nb) {
      M[i,j]=length(intersect(workB[[i]]$lid,workB[[j]]$lid))/length(union(workB[[i]]$lid,workB[[j]]$lid))
      P[i,j]=max(0,1+min(workB[[i]]$to,workB[[j]]$to)-max(workB[[i]]$from,workB[[j]]$from))
      C[i,j]=length(intersect(workB[[i]]$lid,workB[[j]]$lid))
      R[i,j]=C[i,j]/P[i,j]  
    }
  }
  for (i in 1:nb) {
    for (j in 1:nb) {
      #Mm[i,j]=sum((M[i,-c(i,j)]-M[j,-c(i,j)])^2)
      Mm[i,j]=1-sum(xor(M[i,-c(i,j)]>0,M[j,-c(i,j)]>0))/sum( ((M[i,-c(i,j)]>0)|(M[j,-c(i,j)]>0)))
    }
  }
  fix<-function(x) {x[is.na(x)]<-0; diag(x)<-0; return(x)}; M<-fix(M); P<-fix(P); C<-fix(C); R<-fix(R); Mm<-fix(Mm)
  lab2=sapply(workB, function(x) paste(x$name,x$to))
  lab1=sapply(workB, function(x) paste(abbreviate(x$diocese,3),x$to))
  pic(V=M,labels.graph=lab1,labels.image=lab2)
  pic(V=R,labels.graph=lab1,labels.image=lab2) #v sim to M
  pic(V=Mm,labels.graph=lab1,labels.image=lab2)
  
} #attempt to cluster bishops - not very successful
