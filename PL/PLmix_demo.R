
rm(list=ls())

library(MASS)

#WAWW
note=list()
note$RUNDIR="C:/Users/jlee111/OneDrive - The University of Auckland/Documents/PO-Working5"
note$VERBOSE=FALSE

#data setup
note$B=1134                  #cut data to after this year or equal
note$E=1138                  #cut data to before this year or equal
if (note$B>note$E) stop('B before E')

#model setup
note$selectlists='half'       #fraction of list time which must overlap target interval 'strict' is all, 'half' is 50% and 'any' is any 
note$maxlistlength=Inf     #what list lengths allowed? NA or Inf is everything useful. Set to say 14 to knock out a few very long lists if speed an issue

setwd(note$RUNDIR)
#old work functions including alot of legacy stuff need to be in RUNDIR
source(file="dating.R")

#load and form the data
#time window

B=note$B;E=note$E; T=E-B+1
source(file="makedata.R",verbose=note$VERBOSE)

rank=ComputeRanks(cil,B,E)

rla=vector('list',length(cla))
for (i in 1:length(cla)) {a=cla[[i]]; t=round((a$tl+a$tu)/2)-B+1; rla[[i]]$o=a$o; rla[[i]]$e=rank[a$o,t]}

#datafile=paste("./PL/datafile-",B,"-",E,".RData",sep='')
#save(rla,file=datafile)

#Present lists as a matrix, list.mx
#number of row of list.mx = number of lists
#number of column of list.mx = number of bishops
len <- length(rla)
list.unique <- NULL #get a list of bishop numbers
for(i in 1:len){list.unique<-unique(c(list.unique,cla[[i]]$o)) }
list.mx <- matrix(0,len,length(list.unique)); 
for(i in 1:len){list.mx[i,1:length(cla[[i]]$o)]=cla[[i]]$o }

library('PLMIX')

#######################
Gv <- c(1:10) #number of mixture components
Max.p <- list()
Max.W <- list()
Gibbs.Dev <- list()
for(i in Gv){
  MAP <- mapPLMIX_multistart(pi_inv=list.mx, K=K, G=Gv[i],n_start=2, n_iter=400*2)
  GIBBS <- gibbsPLMIX(pi_inv=list.mx, K=K, G=Gv[i], n_iter=T, n_burn=10)
  Max.p[[i]] <- MAP$mod$P_map
  Max.W[[i]] <- MAP$mod$W_map
  Gibbs.Dev[[i]] <- GIBBS$deviance
}

## Select the optimal number of components
SELECT <- selectPLMIX(pi_inv=list.mx, seq_G=Gv,MAPestP=Max.p,MAPestW=Max.W,deviance=Gibbs.Dev)
#########################

K=ncol(list.mx); #column number
G=8 #no of mixture components 
Kn <- 10 #no of folders
T = 1e4 # number of iterations, MCMC
ii <- rep(c(1:Kn),ceiling(len/Kn)); ii=ii[1:len] #folder memberships
log.like.folder <- matrix(0,T,len)
for(i in 1:Kn){ 
  GIBBS <- gibbsPLMIX(pi_inv=list.mx[ii!=i,], K=K, G=G, n_iter=T, n_burn=10)
  for(j in 1:nrow(GIBBS$W)){ for(jt in which(ii==i)){
    test.data<-as.matrix(list.mx[jt,],nrow=1)
    log.like.folder[j,jt] <- 
      loglikPLMIX(p=matrix(GIBBS$P[j,],nrow=G, ncol=K), ref_order=matrix(sort(list.unique), nrow=G, ncol=K, byrow=TRUE),weights=GIBBS$W[j,], pi_inv=test.data)
  }}
}
#ELPD - k-folder cv
sum(log(colMeans(exp(log.like.folder))))


