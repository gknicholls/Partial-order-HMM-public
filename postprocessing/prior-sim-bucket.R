
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
code.dir="C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/collab - kate/newPO/working9b"
out.dir=dir(code.dir)[grep('Full',dir())]

#load functions
setwd(code.dir)
source(file="pofun.R")
source(file="modelfun.R")
source(file="outputfun.R")

Nfile=length(out.dir)
Br=matrix(NA,Nfile,10)
colnames(Br)<-rep(c('Prior','Posterior','Bayes Factor','ESS','BF std err'),2)

for (count in 1:Nfile) {
  
  #load outputfile (RData)
  filename=dir(out.dir[count])[2]; outfile=paste(out.dir[count],'/',filename,sep='')
  output=my.load(outfile) 
  
  #make sure prior model is the same as the prior used in the posterior simulations
  note=output$note
  B=output$B;E=output$E; T=E-B+1
  DB=output$DB; NB=output$NB; 
  rank=output$rank; active=output$active
  
  NF=output$NF
  burn=1:50
  
  ########
  #buckets
  
  J=10000 #make 10000 for production plot runtime is like 30 mins
  vsp.i=bi=matrix(NA,J,T)
  colnames(bi)<-B-1+1:T
  PO=vector('list',J)
  for (j in 1:J) {
    rho=rRprior(fac=1/6)
    theta=rTprior()
    U=rUprior(active,NF,T,theta,rho)
    if (note$DOBETA) {
      beta=rBprior(DB=DB,SigB=1,WARN=FALSE) #unsorted beta's so "unconstrained"
      if (note$constrainbeta) beta<-sort(beta)
    } else {
      beta=output$state$beta #if beta wasnt varying it will be all zeros - but to be safe just set it to the final (constant) value
    }
    #sort for constrained, *0 for no betas #TODO get this from output
    Z=ComputeZ(years=1:T,bishops=1:NB,U,rank,beta)
    PO[[j]]=ZtoPO(Z,active,B,T,display.Z=FALSE)
    bi[j,]=sapply(PO[[j]],is.bucket)
    vsp.i[j,]=sapply(PO[[j]],is.vsp)
  }
  
  ####
  #VSP
  
  P=output$P; done=!is.na(P[,1])
  PO=output$PO[done]; 
  b1=pblapply(PO, function(x) sapply(x,is.bucket))
  b1m=matrix(unlist(b1),sum(done),T,byrow=TRUE)
  prob.all.bucket.post=mean(apply(b1m[-burn,],1,all))
  prob.all.bucket.prior=mean(apply(bi,1,all))
  BF1=prob.all.bucket.post/prob.all.bucket.prior
  print(sprintf(paste(B,'-',E," bucket order BF is %g ; bucket posterior %g ; bucket prior %g"),BF1,prob.all.bucket.post,prob.all.bucket.prior))
  ESS1=effectiveSize(0+apply(b1m[-burn,],1,all))
  BF1.e=sqrt(prob.all.bucket.post*(1-prob.all.bucket.post)/ESS1)/prob.all.bucket.prior
  Br[count,1:5]=c(prob.all.bucket.prior,prob.all.bucket.post,BF1,ESS1,BF1.e)
  #wb=which(apply(b1m,1,all))
  #showDAGs(B,T,PO[[wb[10]]])
  
  vsp.l<-pblapply(PO, function(x) sapply(x,is.vsp))
  vsp.m=matrix(unlist(vsp.l),length(PO),T,byrow=TRUE)
  prob.all.vsp.prior=mean(apply(vsp.i,1,all))
  prob.all.vsp.post=mean(apply(vsp.m[-burn,],1,all))
  BF2=prob.all.vsp.post/prob.all.vsp.prior
  ESS2=effectiveSize(0+apply(vsp.m[-burn,],1,all))
  BF2.e=sqrt(prob.all.vsp.post*(1-prob.all.vsp.post)/ESS2)/prob.all.vsp.prior
  print(sprintf(paste(B,'-',E," VSP BF is %g ; VSP posterior %g ; VSP prior %g"),BF2,prob.all.vsp.post,prob.all.vsp.prior))
  Br[count,6:10]=c(prob.all.vsp.prior,prob.all.vsp.post,BF2,ESS2,BF2.e)
  
  if (FALSE) {
    prob.vsp.post=apply(vsp.m[-burn,],2,mean)
    prob.vsp.prior=apply(vsp.i,2,mean)
    plot(B:E,prob.vsp.post,ylim=c(0,1),type='l',ylab='marginal post prob',xlab='year',main=paste(c(note$B,"-",note$E,"/ NF =",NF,note$model,"/ constrainbeta =",note$constrainbeta),collapse=' '))
    lines(B:E,prob.vsp.prior,lty=2)
    prob.bkt.post=apply(b1m[-burn,],2,mean)
    prob.bkt.prior=apply(bi,2,mean)
    lines(B:E,prob.bkt.post,col=2); 
    lines(B:E,prob.bkt.prior,col=2,lty=2)
    legend("topleft",inset=c(0.5,0.475),legend=c(paste('vsp post / BF =',round(BF2,2),collapse=' '),'vsp prior',paste('bucket post / BF =',round(BF1,2),collapse=' '),'bucket prior'),lty=c(1,2,1,2),col=c(1,1,2,2))
  }
  
}

Period=sub('-dir','',sub('Full','',out.dir))
for (i in 1:Nfile) {
  print(paste(Period[i],' & ',paste(sprintf(' %g &',signif(Br[i,],2)),collapse=''),' ','\\',sep=''))
}

#load(file = "Br-vsp-bucket.RData")

pdf('v2-vsp-bucket-BF.pdf',6,4)
yr=c(1082,1088,1094,1106,1112,1120,1128,1130,1133,1140,1146,1152)
errbar(yr-0.5,Br[,3],Br[,3]+2*Br[,5],Br[,3]-2*Br[,5],xlab='Year',ylab='Bayes Factor',
       cex.lab=1,cex.axis=0.8); 
errbar(yr+0.5,Br[,8],Br[,8]+2*Br[,10],Br[,8]-2*Br[,10],col=1,pch=1,lty=2,add=TRUE); 
abline(h=1,lty=2)
#axis(1,at=1:Nfile,labels=Period,padj=1,las=2)
dev.off()

#save(Br,file = "Br-vsp-bucket.RData")
#example
#vsp.ind=which(apply(!vsp.m,1,all))
#showDAGs(B,T,PO[[vsp.ind[[10]]]])
#dev.off()

#outfile="poHB9aRS1.RData"; burn=1:100; #34-38 subj p real lkdup NF=9 beta=0
#outfile="poHB30e.RData"; burn=1:50; #16-20 subj p real lkdup NF=5 (check) beta=0
#outfile="poHB11b.RData"; burn=1:200; #20-24 subj p real lkdup NF=8 beta=0
#outfile="poHB17aRS2.RData"; burn=1:65; #80-55 subj p real lkdup NF=18 constrained
#outfile="poHB16aRS1.RData"; burn=1:50; #80-55 subj p real lkdup NF=18 unconstrained
#outfile="poHB1aRS6.RData"; burn=1:10; #80-55 subj p real lkdup NF=9 unconstrained
#outfile="poHB12a.RData"; burn=1:80; #80-84  subj p real lkdup NF=8 beta=0

#16-20
#sub p / NF=5 / unconstrained beta / prior 0.22 
#sub p / NF=5 / beta=0 / prior 0.16 / post 0.035 / BF=0.21 
#sub p / NF=9 / unconstrained beta / 0.23
#sub p / NF=9 / beta=0 / prior 0.15 / post 0.023 / BF=0.17