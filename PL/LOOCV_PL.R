rm(list=ls())

library('PLMIX')

folder.name = 'LOOCV-1118-1122'
setwd('C:/Users/jlee111/Documents/working10')
load(paste(folder.name,'-dir/','drop-',1,'-',folder.name,'.RData',sep=''))

source(file="dating.R")
source(file="makedatafun.R")
source(file="makesynthdatafun.R")
source(file="makeparametersfun.R")
source(file="pofun.R")
T=note$E-note$B+1
D=makedata(note,note$B,note$E,T)

fulllist = D$cla #full list

len <- length(fulllist)
list.unique <- NULL #get a list of bishop numbers
for(i in 1:len){list.unique<-unique(c(list.unique,fulllist[[i]]$o)) }
list.mx <- matrix(0,len,length(list.unique)); 
for(i in 1:len){list.mx[i,1:length(fulllist[[i]]$o)]=fulllist[[i]]$o }

T=1e4; Tbutn.in=1e3
K=ncol(list.mx)

### number of mix components ##
Gv <- c(1:10) #number of mixture components
Max.p <- list()
Max.W <- list()
Gibbs.Dev <- list()
for(i in Gv){
  MAP <- mapPLMIX_multistart(pi_inv=list.mx, K=K, G=Gv[i],n_start=2, n_iter=400*2)
  GIBBS <- gibbsPLMIX(pi_inv=list.mx, K=K, G=Gv[i], n_iter=T+Tbutn.in, n_burn=Tbutn.in)
  Max.p[[i]] <- MAP$mod$P_map
  Max.W[[i]] <- MAP$mod$W_map
  Gibbs.Dev[[i]] <- GIBBS$deviance
}

## Select the optimal number of components
SELECT <- selectPLMIX(pi_inv=list.mx, seq_G=Gv,MAPestP=Max.p,MAPestW=Max.W,deviance=Gibbs.Dev)

G = which.min(SELECT$criteria[,1])

log.like <- rep(0,nrow(list.mx))

for(i in 1:length(log.like)){
  load(paste(folder.name,'-dir/','drop-',i,'-',folder.name,'.RData',sep=''))
  
  len <- length(D$cla)
  list.fold <- matrix(0,len,K); 
  for(j in 1:len){list.fold[j,1:length(D$cla[[j]]$o)]=D$cla[[j]]$o }
  K=ncol(list.fold)
  
  test.data<-matrix(list.mx[i,],nrow=1)
  GIBBS <- gibbsPLMIX(pi_inv=list.fold, K=K, G=G, n_iter=T+Tbutn.in, n_burn=Tbutn.in)
  for(j in 1:nrow(GIBBS$W)){ 
    log.like[i] <- log.like[i]+
      loglikPLMIX(p=matrix(GIBBS$P[j,],nrow=G, ncol=K), ref_order=matrix(c(1:K), nrow=G, ncol=K, byrow=TRUE),weights=GIBBS$W[j,], pi_inv=test.data)
  }
}
log.like=log.like/T
save(log.like,paste(folder.name,'-loglikePL.RData',sep=''))






