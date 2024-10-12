
#
# estimate bayes factors for bucket orders and VSP orders
#

library(MASS)
library(mvtnorm)
library(mnem)    #needed for transitive.X - ed 5-4-22
library(igraph) 
library(Rgraphviz)
library(graph) 
library(coda)
library(Hmisc)
library(pbapply); pboptions(type = "timer", style = 3, txt.width=100, char = "=")

rm(list=ls())

#repeatable
set.seed(102)


#where are we working and what is output file to analyse
#code.dir="C:/Users/nicholls/OneDrive - Nexus365/Documents/collab - kate/newPO/working9b/"
codedir="C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/collab - kate/newPO/working9b"


#load functions
setwd(code.dir)
source(file="pofun.R")
source(file="modelfun.R")
source(file="outputfun.R")

out.dir=dir()[grep('Full',dir())]

Nfile=length(out.dir)
Av=matrix(NA,Nfile,3)
row.names(Av)<-out.dir

var=c("rho","theta","p")
mai.v=list(c(0.6,0.5,0.1,0.2),c(0.6,0.4,0.1,0.2),c(0.6,0.4,0.1,0.2))
yl=list(c(0,4),c(0,3),c(0,3.6))
pdf('v2-Constant-rho-theta-p-intervals.pdf',6,2)
par(mfrow=c(1,3))
for (v in 1:3) {
  for (count in 1:Nfile) {
    
    #load outputfile (RData)
    filename=dir(out.dir[count])[2]; 
    outfile=paste(out.dir[count],'/',filename,sep='')
    output=my.load(outfile) 
    
    #make sure prior model is the same as the prior used in the posterior simulations
    note=output$note
    B=output$B;E=output$E; T=E-B+1
    DB=output$DB; NB=output$NB; 
    rank=output$rank; active=output$active
    
    NL=output$NL
    burn=1:50
    
    ########
    
    P=output$P; done=!is.na(P[,1])
    vs=P[done,var[v]]
    vs<-vs[-burn]
    
    if (count==1) {
      par(mai=mai.v[[v]])
      if (v==1)
      plot(density(vs,from=0,to=1,bw=0.1),main='',cex.lab=1.25,xlab=expression(rho),ylab='',ylim=yl[[v]],col=1)#col=1+(NL<20))
      if (v==2)
      plot(density(vs,from=0,to=1,bw=0.1),main='',cex.lab=1.25,xlab=expression(theta),ylab='',ylim=yl[[v]],col=1)#col=1+(NL<20))
      if (v==3)
        plot(density(vs,from=0,to=1,bw=0.1),main='',cex.lab=1.25,xlab='P',ylab='',ylim=yl[[v]],col=1)#col=1+(NL<20))
    } else {
      lines(density(vs,from=0,to=1,bw=0.1),col=1)#col=1+(NL<20))
    }
    Av[count,v]=mean(vs)
  }
}
dev.off()

par(mfrow=c(1,1))
plot(Av[,1],type='l',ylim=c(0,1))
lines(Av[,2],col=2)
lines(Av[,3],col=3)
