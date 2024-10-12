
#
# Check the VSP functions
#


rm(list=ls())

library(MASS)
library(mvtnorm)
library(mnem)    #needed for transitive.X - ed 5-4-22 for mnem
library(igraph) 
library(Rgraphviz)
library(graph) 
library(coda)
library(lecount)

output.dir="C:/Users/nicholls.NICHOLLS2389/Documents/collab - Kate/newPO/working9" #where we have the outfile
setwd(output.dir)

source('pofun.R')
source('modelfun.R')
source('vsp/vspfun.R')

###
#testing vsp functions

##########################################################################
#single random vsp tree - turn it into po draw both
N=20
actors=1:N

tree=rvsp(actors,prob.snode=0.5)
po=tree2po(tree)
par(mfrow=c(1,3),oma=c(0,0,0,0),mai=c(0,0,0,0))
showDAG(transitive.reduction(po),edge.arrow.size=3/N)
showTREE(tree,edge.arrow.size=3/N)
showTREE(tree,use.node.names=TRUE,edge.arrow.size=3/N) #for checking if we want to see the tree node labels

##########################################################################
#lists of random vsp trees with N leaves 
# - look at depth dbn 
# - calculate prior probabilities for PO derived from tree
#   check on PO prior calculation a bit painful as dont presently 
#   have a way to enumerate PO's or VSP's - so just take a large 
#   enough random sample to see them all at least once - fine for N=4, very big K for N=6
#   todo - need a version of TreeWeight for case where prob.snode is random Beta(a,b)
N=4
actors=1:N

K=5000; PO=tree=vector('list',K)
weight=d=rep(NA,K)
for (k in 1:K) {
  prob.snode=0.6 #rbeta(1,1,1) #can make random
  tree[[k]]=rvsp(actors,prob.snode)
  PO[[k]]=tree2po(tree[[k]]); 
  if (!identical(PO[[k]],my.transitive.closure(PO[[k]]))) stop('not tc')
  #d[k]=dagdepth(tree2po(tree)) #if you want to inspect depth dbn
  weight[k]=TreeWeight(tree[[k]],prob.snode)
}
PO<-lapply(PO,standardise) #sort rows and cols so names go 1->n
#hist(d,breaks=0:N+0.5,freq=FALSE)
all(sapply(PO,is.vsp)) # check tree2po(rvsp()) is a PO in the vsp class 

#get unique PO's generated - if we have all of the VSP PO's then prior probs should sum to one
#(N=2) 3, (N=3) 19, (N=4) 195, (N=5) 2791, (N=6) 51303 ...
po.as.string=sapply(PO, function(h) paste(h,collapse=''))
os=order(po.as.string)
pos=po.as.string[os]
POs=PO[os]
trees=tree[os]
weight=weight[os]
#the vector v contains the indices of the unique PO's
#the entries POs[[v[i]]]...POs[[v[i+1]-1]] are all identical
count=1; v=c(1); s=pos[1]; for (i in 2:K) {if (pos[i]!=s){s=pos[i]; v=c(v,i); count=count+1}}; 
count #if this matches the total number of PO-vsp's then sum(w) should equal one
pot=diff(c(v,K+1))
w=weight[v]
sum(w) #

plot(w,pot/K,xlab='calculated PO prob',ylab='observed PO prob',main='check PO prior - useful if count = #PO-vsp'); abline(0,1)
#plot(pot[-1]/K,w[-1]); abline(0,1) #expand origin

#if we are working with PO's that are vsp then we just want the prior prob for our PO
#on this restricted class - this is basicly pi(h) h\in V_n 
POvsp.prior(PO[[1]],prob.snode=0.6) #prior for one PO
prior.vals=sapply(POs[v],POvsp.prior,prob.snode=0.6) 
sum(prior.vals) #if POs[v] enumerates all the PO's then this should sum to one

#########################################################################
#testing the nle.vsp(tree) count matches the nle(PO) count of LE's
#and illustrating the fast LE count for VSP
par(mfrow=c(2,2),oma=c(0,2,0,0),mai=c(0,0,0.2,0)); 

N=16; actors=1:N
K=100; nle.PO=nle.v=tm.PO=tm.v=rep(NA,K)
PO=tree=vector('list',K)
for (k in 1:K) {
  prob.snode=rbeta(1,1,1)
  tree[[k]]=rvsp(actors,prob.snode)
  PO[[k]]=tree2po(tree[[k]]); 
  tm.PO[k]=system.time({nle.PO[k]=nle(PO[[k]])},gcFirst=FALSE)[1]
  tm.v[k]=system.time({nle.v[k]=nle.vsp(tree[[k]])},gcFirst=FALSE)[1]
}

all(nle.PO==nle.v) #check the counts
plot(tm.PO); points(tm.v,col=2); #the PO-count times (black) bigger than vsp count times (red)

#what does the worst tree for nle() looklike
i=which.max(tm.PO); nle.PO[i]
showTREE(tree[[i]],use.node.names=TRUE,edge.arrow.size=5/N); 
showTREE(tree[[i]],edge.arrow.size=5/N); 
showDAG(transitive.reduction(PO[[i]]),edge.arrow.size=5/N)

#look at times for the worst PO/tree
system.time({nle(PO[[i]])},gcFirst=FALSE)
system.time({print(nle.vsp(tree[[i]]))},gcFirst=FALSE)

#showing off po2tree
system.time({tree.bac=po2tree(PO[[i]])$tree; print(nle.vsp(tree.bac))},gcFirst=FALSE)


#an extreme example on 20 nodes 0 sec v. 170 sec.
#load('ExamplePtreeBigLE.RData')
#par(mfrow=c(1,2),oma=c(0,0,0,0),mai=c(0,0,0,0)); 
#showTREE(bad.tree,edge.arrow.size=5/N); showDAG(transitive.reduction(bad.PO),edge.arrow.size=5/N)
#system.time({print(nle.vsp(bad.tree))},gcFirst=FALSE)
#system.time({print(nle(bad.PO))},gcFirst=FALSE)
#showing off po2tree
#system.time({tree.bac=po2tree(bad.PO)$tree; print(nle.vsp(tree.bac))},gcFirst=FALSE)


##########################################################################
#Check po2tree
#po2tree(po)$tree converts a PO to a tree if the PO is a vsp
#it works by finding a cherry in the PO (a pair of nodes that have
#the same relns to all other nodes)

#1 simulate K vsp-trees, 2 convert to po, 3 convert back to tree, 4 convert to po
#check the po's at step 2 and 4 are identical
 
N=20; actors=1:N
K=100; 
PO.bac=tree.bac=PO=tree=vector('list',K)
for (k in 1:K) {
  prob.snode=rbeta(1,1,1)
  tree[[k]]=rvsp(actors,prob.snode)
  PO[[k]]=tree2po(tree[[k]])
  tree.bac[[k]]=po2tree(PO[[k]])$tree
  PO.bac[[k]]=tree2po(tree.bac[[k]])
}
PO<-lapply(PO,standardise) #sort the rows and cols of PO's so names increasing
PO.bac<-lapply(PO.bac,standardise)
all(sapply(1:K, function(x) identical(PO[[k]],PO.bac[[k]]))) #check stage 4 matches stage 2

#plot an example - notice how different the trees are but the PO is the same.
par(mfrow=c(2,2));
i=1
showTREE(tree[[i]],edge.arrow.size=7/N,main='1: random vsp'); 
showDAG(transitive.reduction(PO[[i]]),edge.arrow.size=7/N,main='2: PO from vsp in 1')
showTREE(tree.bac[[i]],edge.arrow.size=7/N,main='3: vsp from PO in 2'); 
showDAG(transitive.reduction(PO.bac[[i]]),edge.arrow.size=7/N,main='4: PO from vsp in 3')

##
N=20
actors=1:N

{
tree=rvsp(actors,prob.snode=0.5)
po=tree2po(tree)
par(mfrow=c(1,3),oma=c(0,0,0,0),mai=c(0,0,0,0))
showDAG(transitive.reduction(po),edge.arrow.size=3/N)
showTREE(tree,edge.arrow.size=3/N)
showTREE(tree,use.node.names=TRUE,edge.arrow.size=3/N) #for checking if we want to see the tree node labels

pk=sapply(1:100000, function(i) choose.Y1(tree)); print(table(pk)/100000)

leaf.nodes=which(sapply(tree,is.leaf))
tn.in.tree=leaf.nodes[which(sapply(leaf.nodes,function(i) is.top.bot(tree,i)))]
pk.compare=c(); for (i in tn.in.tree) pk.compare<-c(pk.compare,nle.vsp(delete(tree,i)))
names(pk.compare)<-sapply(tn.in.tree, function(i) tree[[i]]$actor)
pk.compare/sum(pk.compare)
}

choose.Y1<-function(tree) {
  r=which(sapply(tree,function(x) is.root(x)))
  cc=get.child.count(tree,r)
  n=dim(cc)[2]
  for (i in 1:n) tree[[cc[1,i]]]$nc=cc[2,i]
  g=tree[[r]]
  while (!is.leaf(g)) {
    cc=g$child
    if (g$type=='S') {
      ip=sapply(cc,function(i) tree[[i]]$order=='+'); 
      co=which(ip); 
    } else {
      nc1=tree[[cc[1]]]$nc
      nc2=tree[[cc[2]]]$nc
      if (runif(1)*(nc1+nc2)<nc1) {co=1} else {co=2}
    } 
    g<-tree[[g$child[co]]]
  }
  return(g$actor)
}
