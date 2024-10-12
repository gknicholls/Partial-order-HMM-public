
#Testing

rm(list=ls())
work.dir="C:/Users/nicholls.NICHOLLS2389/Documents/collab - Kate/newPO/working8/PL"
setwd(work.dir)

load("datafile-1134-1138.RData")

#everyone plays
ap=unlist(lapply(rla,function(x) x$o))
actors=sort(unique(unlist(ap)))
all(actors==1:max(ap))

#all levels of experience are seen
ep=unlist(lapply(rla,function(x) x$e))
experience=sort(unique(unlist(ep)))
all(experience==1:max(ep))

N=max(ap)
L=length(rla)
R=max(ep)

##########################

obs.dat=rla

##########################

source("plfun.R")


##############################################################################
#Sample analysis

library(coda)

##########################

#check the prior lambda~N(0,1) beta~N(0,1) seek exp(x)/(exp(x)+exp(y)) reasonable uniform [0,1]
x=rnorm(10000); y=rnorm(10000); hist(exp(x)/(exp(x)+exp(y)),breaks=seq(0,1,length.out=100))

#actually for the joint model it is exp(l+b) etc so the variance is 2
x=rnorm(10000,sd=sqrt(2)); y=rnorm(10000,sd=sqrt(2)); hist(exp(x)/(exp(x)+exp(y)),breaks=seq(0,1,length.out=100))

#a bit peaked toward 0,1 but OK. Could reduce the variance a bit but this will do


##########################
#model with skill and seniority

model2=list(
  rprior=my.rprior2,
  dprior=my.dprior2,
  prior.param=list(N=N,lmu=0,lsig=1,R=R,bmu=0,bsig=1), #list(N=N,lmu=0,lsig=1/sqrt(2),R=R,bmu=0,bsig=1/sqrt(2)), 
  #think about lsig/bsig - only real prior hyperparameter - fixed
  robs=my.robs2,
  dobs=my.dobs2,
  parnames=c(paste('lambda',1:N,sep=''),paste('beta',1:R,sep=''))
)

run.pars=list(
  init.state=model2$rprior(model2$prior.param),     #random start drawn from prior
  proposal=my.q2,
  prop.pars=list(sda=0.1,sd1=1,sdab=0.1,sd1b=1),
  samples=1000,
  sample.interval=100
)

system.time({Y2<-my.mcmc(run.pars,model2,obs.dat)}) #about 3 minutes with 1000 samples/100 sweeps per sample

Lambda2<-matrix(NA,length(Y2),N); Beta2<-matrix(NA,length(Y2),R)
colnames(Lambda2)<-paste('lambda',1:N,sep=''); colnames(Beta2)<-paste('beta',1:R,sep='')
for (i in 1:length(Y2)) {Lambda2[i,]=Y2[[i]]$lambda; Beta2[i,]=Y2[[i]]$beta}
X2<-as.mcmc(cbind(Lambda2,Beta2))

lambda.ind=grep('lambda',colnames(X2)); beta.ind=grep('beta',colnames(X2))
#Ml=diag(N)-matrix(1/N,N,N); Mb=diag(R)-matrix(1/R,R,R)
#X2[,lambda.ind]=X2[,lambda.ind]%*%Ml; X2[,beta.ind]=X2[,beta.ind]%*%Mb

#basic output analysis
effectiveSize(X2); 
#windows(); 
#par(mfrow=c(1,2))
boxplot(as.data.frame(X2)); 
worst=which.min(effectiveSize(X2))
plot(X2[,worst],trace = TRUE, density = FALSE, smooth = FALSE, auto.layout=FALSE, type='l')
my.acf(X2,30)

#####################################################################################################
# no skill in model

model3=list(
  rprior=my.rprior3,
  dprior=my.dprior3,
  prior.param=list(R=R,bmu=0,bsig=1), #think about lsig/bsig - only real prior hyperparameter - fixed
  robs=my.robs3,
  dobs=my.dobs3,
  parnames=paste('beta',1:R,sep='')
)

run.pars=list(
  init.state=model3$rprior(model3$prior.param),
  proposal=my.q3,
  prop.pars=list(sdab=0.1,sd1b=1),
  samples=1000,
  sample.interval=100
)

Y3=my.mcmc(run.pars,model3,obs.dat)

X3<-matrix(NA,length(Y3),R); colnames(X3)<-paste('beta',1:R,sep='')
for (i in 1:length(Y3)) {X3[i,]=Y3[[i]]$beta}
#X3=X3%*%Mb

X3<-as.mcmc(X3)

#basic output analysis
effectiveSize(X3); #effectiveSize(P) #P bigger due to non-identifiability of lambda<-labda+c
#windows(); 
#par(mfrow=c(1,2))
boxplot(as.data.frame(X3));
worst=which.min(effectiveSize(X3))
plot(X3[,worst],trace = TRUE, density = FALSE, smooth = FALSE, auto.layout=FALSE, type='l')
my.acf(X3,30)

###############################

#Bayes factor B_{3,2} is p(y|model 3)/p(y|model 2)
Y3p<-Y3; 
for (i in 1:length(Y3)) Y3p[[i]]$lambda=(model2$rprior(model2$prior.param))$lambda
 
prior2.on.3=sapply(Y3p,model2$dprior,model2$prior.param)
lkd2.on.3=sapply(Y3p,function(x) {(model2$dobs(x,obs.dat))$ll})
prior2.on.2=sapply(Y2,model2$dprior,model2$prior.param)
lkd2.on.2=sapply(Y2,function(x) {(model2$dobs(x,obs.dat))$ll})
m2lp<-function(state,ppar) {sum(dnorm(state$lambda,mean=ppar$lmu,sd=ppar$lsig,log=TRUE))}
prior3.on.2=sapply(Y2,model3$dprior,model3$prior.param)+sapply(Y2,m2lp,model2$prior.param)
lkd3.on.2=sapply(Y2,function(x) {(model3$dobs(x,obs.dat))$ll})
prior3.on.3=sapply(Y3p,model3$dprior,model3$prior.param)+sapply(Y3p,m2lp,model2$prior.param)
lkd3.on.3=sapply(Y3p,function(x) {(model3$dobs(x,obs.dat))$ll})
h2=0#(prior2.on.2+lkd2.on.2+prior3.on.2+lkd3.on.2)/2
h3=0#(prior2.on.3+lkd2.on.3+prior3.on.3+lkd3.on.3)/2
B32=mean(exp(prior3.on.2+lkd3.on.2-h2))/mean(exp(prior2.on.3+lkd2.on.3-h3)); B32

#not convinced above is a reliable estimate - seems a bit big - not sure if bug or just unstable

#cheap BF test if first n betas in model3 are decreasing 
#BF=post(first x betas dec|y)/prior(first x betas dec)
sapply(2:10,function(x) {factorial(x)*mean(apply(X3,1,function(y) {all(diff(y)[1:x]<0)}))})

############################################################################################
#some stuff I didnt need doing analysis with the beta's dropped from the model

if (FALSE) { 
  ################################
  # no seniority in model
  
  model1=list(
    rprior=my.rprior,
    dprior=my.dprior,
    prior.param=list(N=N,lmu=0,lsig=1), #think about lsig/bsig - only real prior hyperparameter - fixed
    robs=my.robs,
    dobs=my.dobs,
    parnames=paste('lambda',1:N,sep='')
  )
  
  run.pars=list(
    init.state=model1$rprior(model1$prior.param),
    proposal=my.q,
    prop.pars=list(sda=0.1,sd1=1),
    samples=10000,
    sample.interval=10
  )
  
  Y1=my.mcmc(run.pars,model1,obs.dat)

  X1<-matrix(NA,length(Y1),N); colnames(X1)<-paste('lambda',1:N,sep='')
  for (i in 1:length(Y1)) {X1[i,]=Y1[[i]]$lambda}
  #X1=X1%*%Ml
  
  X1<-as.mcmc(X1)
  
  #basic output analysis
  effectiveSize(X1); #effectiveSize(P) #P bigger due to non-identifiability of lambda<-labda+c
  #windows(); 
  #par(mfrow=c(1,2))
  boxplot(as.data.frame(X1));  
  worst=which.min(effectiveSize(X1))
  plot(X1[,worst],trace = TRUE, density = FALSE, smooth = FALSE, auto.layout=FALSE, type='l')
  my.acf(X1,30)
  
  ###############################
}