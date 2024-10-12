library(coda)
library(mnem)
source('pofun.R')

n<-50
m<-matrix(0,n,n)
rownames(m)<-colnames(m)<-1:n
mc <- my.transitive.closure(m)
mr <- transitive.reduction(mc)
wold<-sum(mc-mr)

S<-n^2
T<-S*100
POu<-vector('list',T/S)

DRAW<-TRUE
pb<-txtProgressBar(min=0, max=T/S, style=3)
if (DRAW) windows()

for (t in 1:T) {
  v<-sample(1:n,2)
  mp<-m
  mp[v[1],v[2]]<-1-mp[v[1],v[2]]
  
  if (mc[v[1],v[2]]!=mr[v[1],v[2]]) {
    m<-mp
  } else {
    mpc <- my.transitive.closure(mp)
    if (all(mpc+t(mpc)<2)) {
      mpr <- transitive.reduction(mpc)
      wnew<-sum(mpc-mpr)
      if (runif(1)<2^(wold-wnew)) {
        wold<-wnew
        mc<-mpc
        mr<-mpr
        m<-mp
      } 
    }
  }
  
  if (t%%S==0){
    #save the marginal process on t-reductions
    POu[[t/S]]<-mc 
    if (DRAW) {
      showDAG(mr)
    }
    setTxtProgressBar(pb, t/S)
    flush.console()
  }
}

if (DRAW) {
  N=10; NS=T/S; step=floor(NS/N); 
  windows()
  showDAGs(B=1,T=N,PO=POu[seq(step,Ns,step)])
  showDAG(mr,edge.arrow.size=0.6,vertex.color=1,vertex.frame.color=1,vertex.label.cex=0.1,vertex.size=5)
}

Du=sapply(POu,dagdepth)
effectiveSize(Du)

if (DRAW) windows()
hist(Du,breaks=seq(0.5,n+0.5,1))

if (FALSE) {
  #save.image(file = "postprocessing/uniformPO50.RData")
  load(file = "postprocessing/uniformPO50.RData")
  pdf(file='paper/uniformpo50.pdf',6,6)
  showDAG(mr,edge.arrow.size=0.4,edge.arrow.style=1,edge.color='black',vertex.label=NA,vertex.color=0,vertex.frame.width=2,vertex.frame.color='black',vertex.label.cex=0.1,vertex.size=5)
  dev.off()
}
if (FALSE) {
#########
#debugging check - take n=4 and check 219 PO's uniformly distributed
Su=sapply(POu,paste,collapse='')
nu=length(uSu<-unique(Su))

#check stringining works as intended
rPo=lapply(Su, function(x) matrix(sapply(strsplit(x,split=''),as.numeric),n,n)) #,dimnames=list(1:n,1:n)))
identical(rPo,POu)
#get unique PO's back
#ruPo=lapply(uSu, function(x) matrix(sapply(strsplit(x,split=''),as.numeric),n,n,dimnames=list(1:3,1:3)))

#check uniform
Pof<-table(Su)/(Ns<-length(Su))
names(Pof)<-1:nu
hist(Pof*nu,floor(sqrt(nu))); abline(v=1) #should all be about 1.
#########
}
