
#
# DEPTH DISTRIBUTIONS - PRIOR
#


library(MASS)
library(mvtnorm)
library(mnem)    #needed for transitive.X - ed 5-4-22
library(igraph) 
library(Rgraphviz)
library(graph) 
library(coda)

rm(list=ls())
show.pdf=FALSE

note=list()
note$RUNDIR="C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/"
  #"C:/Users/nicholls/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM"

note$VERBOSE=FALSE

#data setup
note$B=1080                  #cut data to after this year or equal
note$E=1155                  #cut data to before this year or equal
if (note$B>note$E) stop('B before E')

note$doi.core=sort(c("Worcester","London","Winchester","Hereford",
                     "Chichester","Durham","Chester","Lincoln","Salisbury","St Davids",
                     "Evreux","Ely","Carlisle","Lisieux","Sees","Norwich","Bayeux","Avranches","Coutances","Rochester"),decreasing=TRUE)
note$doi=c(note$doi.core,sort(c("Sherborne", "Exeter","Bath",
                        "Thetford", "Wells", "Le Mans", "Bangor", "St Asaph","the Orkneys", "Llandaff"),decreasing=TRUE))
#note$doi=c(note$doi.core,sort(c("Exeter","Bath",
#                                "Thetford","Bangor","Llandaff"),decreasing=TRUE))
note$bishopdatefile="BishopDates-25-7-22b.csv" # "BishopDates-4-11-19.csv" #
note$selectlists='half'       #fraction of list time which must overlap target interval 'strict' is all, 'half' is 50% and 'any' is any 
note$maxlistlength=Inf     #what list lengths allowed? NA or Inf is everything useful. Set to say 14 to knock out a few very long lists if speed an issue
note$min.lists.per.bishop=2


note$DOSYNTH=FALSE           #real or synthetic data? None here

#model setup
note$NF=2                   #number of features in U, Z so U is NB x NF x T - at least MA/2 gives all possible PO's - set to NA to get floor(MA/2)
note$constrainbeta=FALSE      #if TRUE then beta is contrained to be decreasing
#note$model='lkdup'           #'bidir', 'lkddown' or 'lkdup' or 'lkdnat' or 'prior' or 'lkmallow' at the moment - 'prior' gives the joint prior of beta,theta,rho,U 
note$PhPar=list(model=note$model,
                p=list(a=1,b=9),      #prior parameters for p(queue jumping), q(bidir), p(mallows)
                q=list(a=1,b=1),      #p is beta prior parameters c(1,9) is default in most runs - called "subjective"
                p.m=list(a=0,b=10))   #p.m is p mallows - uniform(a,b) for mallows penalty param

#for prior done need start
note$STARTSTATE='none'

#run drivers
note$RANDSEED=102

# where are we working?
setwd(note$RUNDIR)

#old work functions including alot of legacy stuff need to be in RUNDIR
source(file="dating.R")
source(file="pofun.R")
source(file = "makedatafun.R")
source(file = "makeparametersfun.R")

#Functions simulating priors and evalutaing log-prior densities
source(file="modelfun.R")

#Functions for output analysis
source(file="outputfun.R")

B=note$B;E=note$E; T=E-B+1

D=makedata(note,B,E,T)
cla=D$cla; cil=D$cil; doi=D$doi

#set up the parameters (initialise the state)
PAR=makeparameters(note,cla,cil)
state=PAR$state; NB=PAR$NB;NF=PAR$NF;NL=PAR$NL;DB=PAR$DB;MA=PAR$MA; rank=PAR$rank;active=PAR$active; PhPar=PAR$PhPar

#
MA; NF=2 #floor(MA/2)
rho.fac=1/3                 #rho prior is Beta(1,rho.fac) - this needs to go in note. 

J=1000 #make 10000 for production plot runtime is like 30 mins
d=matrix(NA,J,T)
colnames(d)<-B-1+1:T
pb<-txtProgressBar(min=0, max=J, style=3)

{
  rRprior<-function(n=1,fac=c(1,1/3,8),w=0.05,tol=1e-4) {
    r=rbeta(n,shape1=fac[1],shape2=fac[2],ncp=fac[3])
    return(r)
  }
  
  for (j in 1:J) {
    rho=rRprior() #rRprior(fac=c(1,1/6,0))  
    theta=rTprior()
    U=rUprior(active,NF,T,theta,rho)
    U<- -log(-log(pnorm((1-theta)*U))) #gumbel
    beta=rBprior(DB=DB,SigB=1,WARN=FALSE) #sort for constrained, *0 for no betas
    Z=ComputeZ(years=1:T,bishops=1:NB,U,rank,beta)
    h=ZtoPO(Z,active,B,T,display.Z=FALSE)
    d[j,]=sapply(h,dagdepth)
    setTxtProgressBar(pb, j)
  }
  
  #d<-d[!is.na(d[1,]),]
  n.active=apply(active,2,sum)
  
  #load(file='postprocessing/prior-depth-dbns-11-0.3-ds.RData')
  #load(file='postprocessing/prior-depth-dbns-22-0.3-ds.RData')
  if (FALSE) {
    if (show.pdf) {pdf(file=paste('paper/prior-depth-dbns-boxes-NF',NF,'.pdf',sep=''),width=6, height=6)} else {windows()}
    boxplot(d,ylim=c(0,DB+1),xlab='year',ylab='depth') #,main='depth dbns in each year (lines individual prior sims)',cex.main=cm)
    lines(n.active,col=2,lwd=2);
    #plot(1:T,n.active,col=2,lwd=2,ylim=c(0,DB),type='l')
    lines(rep(1,T),col=2,lwd=2);
    #apply(jitter(d),1, lines, x=jitter(1:T)) #show individual prior depth sequences
    if (show.pdf) dev.off()
  }
  
  dbns=list()
  for (t in 1:T) {dbns[[t]]=hist(d[,t],breaks=0.5:(n.active[t]+0.5),plot=FALSE)$density}
  md=max(sapply(dbns,max))
  cols=n.active-min(n.active)+1
  depths=min(n.active):max(n.active)
  
  if (TRUE) {
    #Fractional Depth Dbn
    if (show.pdf) {pdf(file=paste('paper/prior-depth-dbns-curves-NF',NF,'.pdf',sep=''),width=6, height=6)} else {windows()}
    plot(c(0,1.2),c(0,md), type='n', #main='prior depth histograms, unconstrained beta, fractional depth',
         xlab='fractional depth - d(h)/m',ylab='propn of POs',cex.main=1)
    for (t in 1:T) {lines((1:n.active[t])/n.active[t],dbns[[t]],col=cols[t])}
    #abline(h=0,lwd=2)
    legend('topright',legend=c('Unif',sapply(depths,as.character)),lty=rep(1,length(depths)),col=depths-depths[1]+1,
           cex=0.8,y.intersp=1)
    if (show.pdf) dev.off()
  }
  
  if (FALSE) {
    #Absolute Depth DBN
    #if (show.pdf) {pdf(file=paste('paper/prior-depth-dbns-curves-NF',NF,'.pdf',sep=''),width=6, height=6)} else {windows()}
    plot(c(0,1.1*MA),c(0,md), type='n',       #main='prior depth histograms, unconstrained beta, actual depth',
         xlab='depth',ylab='propn of POs')#,cex.main=cm)
    for (t in 1:T) {lines(1:n.active[t],dbns[[t]],col=cols[t])}
    #abline(h=0,lwd=2)
    legend('topright',legend=c('Unif',sapply(depths,as.character)),
           lty=c(2,rep(1,length(depths))),col=c(1,depths-depths[1]+1),cex=0.5,y.intersp=0.5)
  }
  
  #save.image(file = paste('postprocessing/prior-depth-dbns-',NF,'-',rho.fac,'ds.RData',sep=''))
}

if (FALSE) {
  load("postprocessing/uniformPO.RData")
  Df=table(Du)/length(Du)
  Dv=sapply(names(Df),as.numeric)
  Df<-c(0,Df,0)
  Dv<-c(Dv[1]-1,Dv,Dv[length(Dv)]+1)
  lines(Dv/18,Df,lty=2,lwd=2)
}

if (show.pdf) dev.off()

##########

###
#posterior depth dbn

run.name="T-M2-down-sub-cn-os" # "T-M-down-sub-cn-os" # 
output=my.load("T-M2-down-sub-cn-os-dir/T-RS5-M2-down-sub-cn-os.RData")
#output=my.load("T-M-down-sub-cn-os-dir/T-RS6-M-down-sub-cn-os.RData")
PO=output$PO
P=output$P
T=output$T
active=output$active
DB=output$DB
n.active=apply(active,2,sum)

Si=which(!is.na(P[,1]))
burn=100
Si=Si[-(1:burn)]
Ns=length(Si)

d1=lapply(PO[Si], function(x) sapply(x,dagdepth))
d1m=matrix(unlist(d1),Ns,T,byrow=TRUE)
colnames(d1m)<-output$B-1+(1:T)

if (show.pdf) {pdf(file=paste('paper/post-depth-dbns-boxes-',run.name,'.pdf',sep=''),width=6, height=6)} else {windows()}

boxplot(d1m,ylim=c(0,DB+1),xlab='year',ylab='depth')#,main='depth dbns in each year (lines individual prior sims)',cex.main=cm)
lines(n.active,col=2,lwd=2)
lines(rep(1,T),col=2,lwd=2)
if (show.pdf) dev.off()
############

##########

###
#posterior depth dbn

run.name='poHB22aRS4' #'poHB34aRS2' #poHB33aRS2'
output=my.load("C:/Users/nicholls.NICHOLLS2389/Documents/collab - Kate/newPO/working9/PO-results/working5/synth/poHB22aRS4.RData")
#output=my.load("poHB17aRS4.RData")
PO=output$PO
P=output$P
T=output$T
B=output$B
E=output$E
active=output$active
DB=output$DB
n.active=apply(active,2,sum)

Si=which(!is.na(P[,1]))
burn=10
Si=Si[-(1:burn)]
Ns=length(Si)

d1=lapply(PO[Si], function(x) sapply(x,dagdepth))
d1m=matrix(unlist(d1),Ns,T,byrow=TRUE)
colnames(d1m)<-B-1+(1:T)

dt=sapply(output$Sstate$h, dagdepth)

if (show.pdf) {pdf(file=paste('post-depth-dbns-boxes-',run.name,'.pdf',sep=''),width=6, height=6)} else {windows()}

boxplot(d1m,ylim=c(0,DB+1),xlab='year',ylab='depth')#,main='depth dbns in each year (lines individual prior sims)',cex.main=cm)
lines(n.active,col=3,lwd=2)
lines(rep(1,T),col=3,lwd=2)
lines(dt,col=2,lwd=2)

if (show.pdf) dev.off()
############

x=rRprior(n=10000)
f<-function(ab){return(-sum(dbeta(x,shape1=ab[1],shape2=ab[2],ncp=ab[3],log=TRUE)))}
optim(par=c(1,1,0),fn=f)
x=seq(0.0001,0.5,length.out=100); y=dbeta(x,shape1=1,shape2=0.4,ncp=128); plot(x,y)
