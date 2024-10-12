
#
#le.expand() check - function in pofun.R lists LE's of a PO
#

rm(list=ls())

library(MASS)
library(mnem)    #needed for transitive.X - ed 5-4-22 for mnem
library(igraph) 
library(Rgraphviz)
library(graph) 


output.dir="C:/Users/nicholls.NICHOLLS2389/Documents/collab - Kate/newPO/working6" #where we have the outfile
setwd(output.dir)

source('pofun.R')
source('vsp/vspfun.R')

#########################################################################

set.seed(3)
N=20; actors=1:N
PO=tree2po(rvsp(actors,0.9))
showDAG(transitive.reduction(PO))
nles=nle(PO); nles
les=le.expand(PO); les
if (!nles==dim(les)[2]) stop('le.expand() returned wrong number of lin extns')
