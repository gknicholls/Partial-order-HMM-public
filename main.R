
if (exists('cl') && !is.na(cl)) {stopCluster(cl); gc()}

rm(list=ls())

library(MASS)
library(mvtnorm)
library(mnem)    #needed for transitive.X - ed 5-4-22
library(igraph) 
library(Rgraphviz)
library(graph) 
library(coda)
library(lecount)

#key drivers for run - at some point we will write this to a file in a new unique directory for the run
note=list()
note$RUNDIR="C:/Users/nicholls/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM"
note$VERBOSE=FALSE

#data setup
note$B=1080                  #cut data to after this year or equal
note$E=1155                  #cut data to before this year or equal
if (note$B>note$E) stop('B before E')
note$doi.core=sort(c("Worcester","London","Winchester","Hereford",
                     "Chichester","Durham","Chester","Lincoln","Salisbury","St Davids",
                     "Evreux","Ely","Carlisle","Lisieux","Sees","Norwich","Bayeux","Avranches","Coutances","Rochester"),decreasing=TRUE)
#note$doi=c(note$doi.core,sort(c("Sherborne", "Exeter","Bath",
#                                "Thetford", "Wells", "Le Mans", "Bangor", "St Asaph","the Orkneys", "Llandaff"),decreasing=TRUE))
note$doi=c(note$doi.core,sort(c("Exeter","Bath",
                                "Thetford","Bangor","Llandaff"),decreasing=TRUE))
#doesnt include "Tusculum"
note$bishopdatefile="BishopDates-25-7-22b.csv" #"BishopDates-4-11-19.csv"
note$selectlists='half'       #fraction of list time which must overlap target interval 'strict' is all, 'half' is 50% and 'any' is any 
note$maxlistlength=Inf     #what list lengths allowed? NA or Inf is everything useful. Set to say 14 to knock out a few very long lists if speed an issue
note$min.lists.per.bishop=2

#synth data setup
note$DOSYNTH=FALSE            #real or synthetic data?
note$srep=1                  #number of repeated copies of cla like scla<-rep(cla,srep) in synth data
note$true.p=0.1                #error probability
note$true.q=0
note$true.theta=0.9          #if synth then this is true theta
note$true.rho=0.925            #if syth then this is true rho
note$true.beta=NA            #if NA then simulated (and ordered) - set to zero to remove beta
note$sNF=9                   #number of features in synth data
note$SYNTHTYPE='priorstate'
#'poststate' means truth is last state in old file, 
#'priorstate' means state is (mainly) sampled using priors
#oldsynth means it uses synthetic data from a past run
note$TRUEINPUTFILE="./testFUN-dir/testFUN.RData" #if poststate
note$OLDSYNTHINPUTFILE="./testFUN-dir/testFUN.RData" #if oldsynth

#model setup
note$NF=2                    #number of features in U, Z so U is NB x NF x T - at least MA/2 gives all possible PO's - set to NA to get floor(MA/2)
note$constrainbeta=FALSE     #if TRUE then beta is contrained to be decreasing
note$model='lkdup'           #'bidir', 'lkddown' or 'lkdup' or 'lkdnat' or 'prior' or 'lkmallow' at the moment - 'prior' gives the joint prior of beta,theta,rho,U 
note$PhPar=list(model=note$model,
                rfac=c(shape1=1,shape2=1/3,ncp=8), #prior parameters for rho
                p=list(a=1,b=9),      #prior parameters for p(queue jumping), q(bidir), p(mallows)
                q=list(a=1,b=1),      #p is beta prior parameters c(1,9) is default in most runs - called "subjective"
                p.m=list(a=0,b=10))   #p.m is p mallows - uniform(a,b) for mallows penalty param

#MCMC drivers
note$RANDSEED=101
note$NEWRUN=TRUE             #set to FALSE if resarting old run
note$DRAW=FALSE
note$savefile='T-MK2-up-sub-uc-os.RData'  #write MCMC samples to savefile
note$loadfile=NA             #if restarting set this to old savefile
note$LOADDIR=NA              #if restarting set this to old savedir
note$MCMC.SWEEPS=100000
note$SUBSAMPLE.INTERVAL=10   #get note$MCMC.SWEEPS/note$SUBSAMPLE.INTERVAL samples
note$DOWRITE=TRUE            #set to false if you dont want to save along the way - sensible for short runs only.
note$WRITE.SUBSAMPLE=100     #write the state to a file less frequently as this takes time for long runs
note$Usweeps=c(0,2)          #number of U-sweeps in doU (all b's per proposal) and doU2 (one b at a time) for each beta-sweep
note$UALLFRACUPD=c(0,1)      #fraction of two (fast,slow) U-updates to use
note$rho.reps=5              #number of rho-updates per sweep
note$theta.reps=5            #number of theta-updates per sweep
note$rhotheta.reps=5
note$rhotheta.wrap.reps=1
note$rhothetaU.wrap.reps=3
note$p.reps=5
note$q.reps=5
note$comment="T-M-up-sub-uc-os"

#if the update is switched off (debugging etc) make sure init value set to something sensible
note$STARTSTATE='ordered'    #'disordered','ordered','oldstart', 'true' - 'true' only possible if synth data 
note$STARTFILE="filename.RData"            #if oldstart, give the filename containing the run here - run will start from final state
note$init.beta.fac=1         #if synth & true start this multiplies init beta 
note$init.U.fac=1            #if synth & true start this multiplies init U - set close to zero if testing U mcmc

#init beta is all 0 and init tau is prior sample
note$init.p=ifelse(note$STARTSTATE=='disordered',0.95,0.05)             #0.05 plausible
note$init.q=ifelse(note$STARTSTATE=='disordered',0.5,0.05)              #0.05 plausible
note$init.theta=ifelse(note$STARTSTATE=='disordered',0.1,0.95)          #init theta - 0.95 plausible
note$init.rho=ifelse(note$STARTSTATE=='disordered',0.1,0.9)             #init rho - 0.9 plausible

note$DOTHETA=TRUE            #do we switch on the theta-update
note$DOBETA=TRUE             #do we switch on the beta-update
note$DORHO=TRUE              #do we switch on the rho-update
note$DORHOTHETA=TRUE         #combined rho-theta update
note$DOU=TRUE                #do we switch on the U-update
note$DOP=TRUE                #P-update
note$DOQ=(note$model=='bidir')                #Q-update
note$DOTAU=TRUE              #tau-update

#parallel drivers - dont use this - it seems more efficient to use the cores to do multiple independent scalar runs.
#Also in current version it isnt actually any faster - seems to depend delicately on where functions defined.
#was working now v. slow
note$DOPAR=FALSE             #use parallel proc?
if (note$DOPAR) library(parallel)
note$nthread=2               #number of parallel threads - as a rule of thumb, seems not much gain above 4 
#note$optimise.par.blocks=FALSE
note$split.years=1118 #c(1119,1134,1137)

#######################################################

# where are we working?
setwd(note$RUNDIR)

#old work functions including alot of legacy stuff need to be in RUNDIR
source(file="dating.R")
source(file="makedatafun.R")
source(file="makesynthdatafun.R")
source(file="makeparametersfun.R")
source(file="pofun.R")

#Functions simulating priors and evalutaing log-prior densities
source(file="modelfun.R")

#Functions for output analysis
source(file="outputfun.R")

#Functions related to the MCMC
source(file="mcmcfun.R",verbose=note$VERBOSE)

#main mcmc function
source(file='mcmcPO.R')

if (note$NEWRUN) {
  temp.dir=paste('./',sub('.RData','',note$savefile),'-dir',sep='')
  count=1; while (dir.exists(temp.dir)) {temp.dir=paste('./',sub('.RData','',note$savefile),'-',letters[count],'-dir',sep=''); count=count+1}
  note$SAVEDIR=temp.dir
  dir.create(note$SAVEDIR)
} else {
  note$SAVEDIR=note$LOADDIR
}

if (note$NEWRUN) {
  this.file <- parent.frame(2)$ofile; 
  file.copy(this.file, paste(note$SAVEDIR,'/',sub('.RData','',note$savefile),'-main.R',sep=''))
} 

#if (FALSE) {
#run MCMC - for longer runs this wont complete so O will never be returned, output via file saving

O=mcmcPO(note)

#save the output if it wasnt already saved
if (!note$DOWRITE) my.save(paste(note$SAVEDIR,'/',note$savefile,sep=''),O)

output=outputanalysis(out.dir=note$SAVEDIR,out.file=note$savefile,burn=10,yoi=(note$B:note$E)-note$B+1,
                      pdf.file=NA,P.samples=NA,full.analysis=TRUE)

if (note$DOPAR) {stopCluster(cl); gc()}
#}