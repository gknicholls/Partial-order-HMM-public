
#
# Check the QP functions (including the vsp-specific ones)
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

############################################################################
#QP functions
#testing QP-functions 
#QPunifLE() - random LE's from a PO using the up/down queue jumping noise model
#QP.LEProb() - likelihood calculation - for one list - in the QP model
#QP.LEProb.vsp() - likelihood calc - one list - using vsp structure

#a standard simple PO on N=6 actors
h=structure(
  c(0, 0, 0, 0, 0, 0, 
    1, 0, 0, 0, 0, 0, 
    1, 1, 0, 0, 0, 0, 
    1, 1, 0, 0, 0, 0, 
    1, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 0), .Dim = c(6L, 6L), 
  .Dimnames = list(c("6", "3", "1", "5", "4", "2"), c("6", "3", "1", "5", "4", "2")))

par(mfrow=c(2,2))
showDAG(transitive.reduction(h))

#enumerate all lists check probs sum one
lem=enum.seq(rownames(h))
p.err=0.1; q.direct=0.1
p=apply(lem,1,function(x) QP.LEProb(h,x,p.err,q.direct))
sum(p)
pn=apply(lem,1,paste,collapse='')

#sample lists from PO h using given p=p.err and q=q.err 
J=10000
le=lapply(1:J,function(x) QPunifLE(h,p.err,q.direct))
les=sapply(le,paste,collapse='')
let=table(les)

#check frequencies for simulated LE's in QP noise model match calculate lkd's
p2let.i=match(pn,names(let))
pres=which(!is.na(p2let.i))
all(pn[pres]==names(let)[p2let.i[pres]])
plot(p[pres],let[p2let.i[pres]]/J,xlab='predicted le-prob',ylab='observed le-prob',
     main='testing QPLEProb() v. QPunifLE()');abline(0,1)

###
#check QPLEProb.vsp()
tree=po2tree(h)$tree #the ame PO as above now as a tree
showTREE(tree); 
#h.bac=tree2po(tree)
#showDAG(transitive.reduction(h.bac))

#enumerate all lists check probs sum one
p=apply(lem,1,function(x) QP.LEProb.vsp(tree,x,p.err,q.direct))
sum(p)
pn=apply(lem,1,paste,collapse='')

#check frequencies for simulated LE's in QP noise model match calculate lkd's
p2let.i=match(pn,names(let))
pres=which(!is.na(p2let.i))
all(pn[pres]==names(let)[p2let.i[pres]])
plot(p[pres],let[p2let.i[pres]]/J,xlab='predicted le-prob',ylab='observed le-prob',
     main='testing QPLEProb.vsp() v. QPunifLE()');abline(0,1)

#check a bigger example
N=60; actors=1:N
prob.snode=rbeta(1,1,1)
tree=rvsp(actors,prob.snode)
PO=standardise(tree2po(tree)) 
showDAG(transitive.reduction(PO))
tree.bac=po2tree(PO)$tree
PO.bac=standardise(tree2po(tree.bac))
showDAG(transitive.reduction(PO.bac))
all(PO==PO.bac)

#calculate the lkd from the po and from the tree
p.err=0.1; q.direct=0.0
le=QPunifLE(PO,p.err,q.direct)
system.time({a<-QP.LEProb.vsp(tree,le,p.err,q.direct)}) #this number and the next should be equal
system.time({b<-QP.LEProb(PO,le,p.err,q.direct)})
abs(a-b)/abs(a)<1e-15
system.time({aa<-P.LEProb.vsp(tree,le,p.err)}) 
