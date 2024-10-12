
rm(list=ls())

library(MASS)

#WAWW
note=list()
note$RUNDIR="C:/Users/nicholls.NICHOLLS2389/Documents/collab - Kate/newPO/working8"
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

datafile=paste("./PL/datafile-",B,"-",E,".RData",sep='')
save(rla,file=datafile)

