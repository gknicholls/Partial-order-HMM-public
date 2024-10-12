#just OA
rm(list=ls())

library(MASS)
library(mvtnorm)
library(mnem)    #needed for transitive.X - ed 5-4-22 for mnem
library(igraph) 
library(Rgraphviz)
library(graph) 
library(coda)
library(lecount)

#set this to the place where your "fun"-files are located
#work.dir="C:/Users/nicholls/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM"
work.dir="C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM"
setwd(work.dir)

source(file="pofun.R")
source(file="modelfun.R")
source(file="outputfun.R")
 
yoi=1:76
output.dir="C:/Users/nicholls/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/dummyRUN-dir"
output=outputanalysis(out.dir=output.dir,out.file="dummyRUN.RData",burn=10,yoi=yoi,pdf.file=NA,P.samples=NA,full.analysis=TRUE)
 
