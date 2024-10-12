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
if (Sys.getenv("COMPUTERNAME")=="NICHOLLS2389") {
  work.dir="C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM"
} else {
  work.dir="C:/Users/nicholls/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM"
}
setwd(work.dir)

source(file="pofun.R")
source(file="modelfun.R")
source(file="outputfun.R")

loaddir.root=dir()[grep("-dir",dir())]
loaddir=paste(work.dir,'/',loaddir.root,sep='')

yoi=1:76
doi=sort(c("Worcester","London","Winchester","Hereford","Chichester","Durham","Chester","Lincoln",
           "Salisbury","St Davids","Evreux","Ely","Carlisle","Lisieux","Sees","Norwich","Bayeux",
           "Avranches","Coutances","Rochester"),decreasing=FALSE)
output.dir=loaddir[9]
setwd(output.dir)
output.file=dir()[grep(".RData",dir())]

output=outputanalysis(out.dir=output.dir,out.file=output.file,burn=90,doi.plot=doi,
                      yoi=yoi,pdf.file=NA,P.samples=50,full.analysis=TRUE)
 
#> loaddir.root
#[1] "H-M2-down-sub-uc-os-synth-dir" "H-M2-up-sub-uc-os-synth-dir"   "T-M-down-sub-cn-ds-dir"        "T-M-down-sub-cn-os-dir"       
#[5] "T-M-down-sub-uc-ds-dir"        "T-M-down-sub-uc-os-dir"        "T-M-up-sub-cn-os-dir"          "T-M-up-sub-uc-os-dir"         
#[9] "T-M2-down-sub-cn-ds-dir"       "T-M2-down-sub-cn-os-dir"       "T-M2-down-sub-uc-ds-dir"       "T-M2-down-sub-uc-os-dir"      
#[13] "T-M2-up-sub-cn-os-dir"         "T-M2-up-sub-uc-os-dir" 
