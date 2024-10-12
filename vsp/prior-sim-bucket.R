
#
# estimate bayes factors for bucket orders and VSP orders - lost file rescued from HB
#

library(MASS)
library(mvtnorm)
library(mnem)    #needed for transitive.X - ed 5-4-22
library(igraph) 
library(Rgraphviz)
library(graph) 
library(coda)
library(pbapply); pboptions(type = "timer", style = 3, txt.width=100, char = "=")

rm(list=ls())

#repeatable
set.seed(102)


#where are we working and what is output file to analyse
code.dir="C:/Users/nicholls/OneDrive - Nexus365/Documents/collab - kate/newPO/working5/"
out.dir=code.dir

#outfile="poHB9aRS1.RData"; burn=1:100; #34-38 subj p real lkdup NF=9 beta=0
#outfile="poHB30e.RData"; burn=1:50; #16-20 subj p real lkdup NF=5 (check) beta=0
#outfile="poHB11b.RData"; burn=1:200; #20-24 subj p real lkdup NF=8 beta=0
#outfile="poHB17aRS2.RData"; burn=1:65; #80-55 subj p real lkdup NF=18 constrained
#outfile="poHB16aRS1.RData"; burn=1:50; #80-55 subj p real lkdup NF=18 unconstrained
#outfile="poHB1aRS6.RData"; burn=1:10; #80-55 subj p real lkdup NF=9 unconstrained
#outfile="poHB12a.RData"; burn=1:80; #80-84  subj p real lkdup NF=8 beta=0

#load functions
setwd(code.dir)
source(file="pofun.R")
source(file="modelfun.R")
source(file="outputfun.R")

#load outputfile (RData)
setwd(out.dir)
output=my.load(outfile) 

#make sure prior model is the same as the prior used in the posterior simulations
note=output$note
B=output$B;E=output$E; T=E-B+1
DB=output$DB; NB=output$NB; 
rank=output$rank; active=output$active

NF=output$NF

J=1000 #make 10000 for production plot runtime is like 30 mins
vsp.i=bi=matrix(NA,J,T)
colnames(bi)<-B-1+1:T
PO=vector('list',J)
for (j in 1:J) {
  rho=rRprior(fac=note$PhPar$rfac)
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
#16-20
#sub p / NF=5 / unconstrained beta / prior 0.22 
#sub p / NF=5 / beta=0 / prior 0.16 / post 0.035 / BF=0.21 
#sub p / NF=9 / unconstrained beta / 0.23
#sub p / NF=9 / beta=0 / prior 0.15 / post 0.023 / BF=0.17


P=output$P; done=!is.na(P[,1])
PO=output$PO[done]; 
b1=pblapply(PO, function(x) sapply(x,is.bucket))
b1m=matrix(unlist(b1),sum(done),T,byrow=TRUE)
prob.all.bucket.post=mean(apply(b1m[-burn,],1,all))
prob.all.bucket.prior=mean(apply(bi,1,all))
BF1=prob.all.bucket.post/prob.all.bucket.prior
sprintf("the bayes factor for bucket orders is %g with bucket posterior %g and bucket prior %g",BF1,prob.all.bucket.post,prob.all.bucket.prior)
#wb=which(apply(b1m,1,all))
#showDAGs(B,T,PO[[wb[10]]])

vsp.l<-pblapply(PO, function(x) sapply(x,is.vsp))
vsp.m=matrix(unlist(vsp.l),length(PO),T,byrow=TRUE)
prob.all.vsp.prior=mean(apply(vsp.i,1,all))
prob.all.vsp.post=mean(apply(vsp.m[-burn,],1,all))
BF2=prob.all.vsp.post/prob.all.vsp.prior
sprintf("the bayes factor for VSP orders is %g with VSP posterior %g and VSP prior %g",BF2,prob.all.vsp.post,prob.all.vsp.prior)

prob.vsp.post=apply(vsp.m[-burn,],2,mean)
prob.vsp.prior=apply(vsp.i,2,mean)
plot(B:E,prob.vsp.post,ylim=c(0,1),type='l',ylab='marginal post prob',xlab='year',main=paste(c(note$B,"-",note$E,"/ NF =",NF,note$model,"/ constrainbeta =",note$constrainbeta),collapse=' '))
lines(B:E,prob.vsp.prior,lty=2)
prob.bkt.post=apply(b1m[-burn,],2,mean)
prob.bkt.prior=apply(bi,2,mean)
lines(B:E,prob.bkt.post,col=2); 
lines(B:E,prob.bkt.prior,col=2,lty=2)
legend("topleft",inset=c(0.5,0.475),legend=c(paste('vsp post / BF =',round(BF2,2),collapse=' '),'vsp prior',paste('bucket post / BF =',round(BF1,2),collapse=' '),'bucket prior'),lty=c(1,2,1,2),col=c(1,1,2,2))

#example
#vsp.ind=which(apply(!vsp.m,1,all))
#showDAGs(B,T,PO[[vsp.ind[[10]]]])
#dev.off()
