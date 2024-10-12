#
#output analysis - all the figures in the PO paper and quite a bit more

rm(list=ls())

library(MASS)
library(mvtnorm)
library(mnem)    #needed for transitive.X - ed 5-4-22
library(igraph) 
library(Rgraphviz)
library(graph) 
library(coda)
library(lecount) #count linear extensions of a PO

#
#setwd("C:/Users/nicholls.NICHOLLS2389/Documents/collab - Kate/newPO/working9")
software.dir="C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/"
setwd(software.dir)

source(file="dating.R")
source(file="makedatafun.R")
source(file="makesynthdatafun.R")
source(file="makeparametersfun.R")
source(file="pofun.R")
source(file="modelfun.R")
source(file="outputfun.R")
source(file='vsp/vspfun.R')

#numbers for paper- things like number bishops, lists etc
#output=my.load("../working5/poHB1aRS6.RData") #old version of paper

output.dir="C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/"
out.file="T-M2-down-sub-uc-ds-dir/T-RS5-M2-down-sub-uc-ds.RData" #a K=22 example
out.file="T-M-up-sub-cn-os-dir/T-RS6-M-up-sub-cn-os.RData"       #a K=11 example
output=my.load(paste(output.dir,out.file,sep=''))

lid=sapply(output$cla,function(x){x$id})
output$cla[lid==2364] #access some example lists for the paper appendix

(NL=output$NL) #num lists
(NB=output$NB) #num bishops
(DB=output$DB) #dimension of beta
(NF=output$NF) #K-value

#basic numbers like typical lists per bishop
a=sapply(output$cla,function(x){x$tu-x$tl+1})   #get list time-ranges
sum(a>1)
mean(a)
sd(a)
quantile(a,0.9)                                 #how much uncertainty in list times
b=sapply(output$cil,function(x){x$to-x$from+1}) #how long are bishops in post
d=sapply(output$cil,function(x){x$ll}); mean(d) #how many bishops per list
f=sapply(output$cla,function(x){x$ll}); mean(f) #how many lists per bishop
  
#rank stuff
pdf('paper/v2-seniority-rank-stuffb.pdf',10,4)
#windows(6,4)
par(mfrow=c(1,2))
#plot the changing rank of each bishop
set.seed(2)
rank=output$rank
plot(1:40,seq(1,22,length.out=40),type='n',xlab='years in post',ylab='seniority rank')
junk=lapply(1:dim(rank)[1],function(x) {a=rank[x,!is.na(rank[x,])]; lines(jitter(1:length(a),1),a+rnorm(1,0,0.2),col=sample(1:4,1))})

#count number of lists each rank appears in
cla=output$cla; B=output$B; E=output$E; T=output$T
rc=rep(0,DB); for (i in 1:NL) {L=cla[[i]]; for (j in 1:L$ll) {b=L$o[j]; yr=round(min(max( (L$tl+L$tu)/2, B),E)-B+1); r=rank[b,yr]; rc[r]=rc[r]+1;}}
barplot(rc,names=1:DB,xlab='seniority rank',ylab='frequency',col=gray.colors(1, start = 0.9, end = 0.9, gamma = 2.2))
dev.off()

#detach(output)
rm(output)

#
n=5
m=matrix(c(0,1,1,1,1,0,0,1,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0),n,n,byrow=TRUE)
colnames(m)<-rownames(m)<-1:n

# make the example 5 element PO used as a simple example
set.seed(4)
NB=5; NF=4
finished=FALSE; count=0
while (!finished) {
  rho=rRprior()
  Sig=matrix(rho,NF,NF); diag(Sig)=1;
  count=count+1
  U=mvrnorm(NB,rep(0,NF),Sig)
  beta=rnorm(NB)
  Z=U+beta
  vp<-latent2order(Z)
  mp<-order2partial(vp,NB); 
  mpc<-my.transitive.closure(mp)
  finished=(identical(m,mpc) | count==10000)
}; print(count); 
par(mfrow=c(1,2))
v=c(1,2,4,8,1)
U[2,]<-U[2,]-0.4
vp<-latent2order(Z);mp<-order2partial(vp,NB); mpc<-my.transitive.closure(mp)
showDAG(mpc)

#show the closure, reduction and suborder
{pdf('paper/v2-fivevertexdagD.pdf')
twotwo=FALSE
if (twotwo) {par(mfrow=c(2,2));par(mai=c(0,0.15,0,0.15),oma=c(0,0,0,0))} else {par(mfrow=c(1,3));par(mai=c(0,0.05,0,0.05),oma=c(0,0,0,0))}
showDAG(m,edge.arrow.size=0.6,edge.width=3,vertex.color=NA,vertex.frame.color=1,
        vertex.frame.width=2,vertex.label.cex=1.2,vertex.size=33,
        vertex.label.family="sans")
text(-0.75,-1,'Closure')
if (twotwo) {par(mai=c(0,0.15,0,0.15))} else {par(mai=c(0,0.1,0,0))}
showDAG(transitive.reduction(m),edge.arrow.size=0.6,edge.width=3,vertex.color=NA,vertex.frame.color=1,vertex.frame.width=2,vertex.label.cex=1.2,vertex.size=33,vertex.label.family="sans")
text(-0.95,-1,'Reduction')
if (twotwo) {par(mai=c(0.4,0.15,0.4,0))} else {par(mai=c(0,0.1,0,0.5))}
o=c(2,4,5)
showDAG(m[o,o],edge.arrow.size=0.6,edge.width=3,vertex.color=NA,vertex.frame.color=1,
        vertex.frame.width=2,vertex.label.cex=1.2,vertex.size=37,vertex.label.family="sans")
text(-0.0,-1.6,'Suborder')
dev.off()
}

#show the U and Z matrices parameterising this example
{pdf('paper/v2-fivevertexdagD-UZ.pdf',9,3)
par(mfrow=c(1,3),cex.axis=1.25)
plot(1:NF,U[1,],ylim=c(-3.5,3.5),type='b',lwd=2,pch="1",cex=1.5,cex.lab=1.2,
     xlab='Feature index k=1,...,K',ylab='U-value'); 
junk=lapply(2:NB,function(x) lines(U[x,],type='b',cex=1.5,lty=1,col=v[x],lwd=2,pch=as.character(x)))
plot(1:NF,Z[1,],ylim=c(-3.5,3.5),type='b',lwd=2,pch="1",cex=1.5,cex.lab=1.2,
     xlab='Feature index k=1,...,K',ylab='Z-value'); 
junk=lapply(2:NB,function(x) lines(Z[x,],type='b',cex=1.5,lty=1,col=v[x],lwd=2,pch=as.character(x)))
par(mai=c(0,0,0,1))
showDAG(transitive.reduction(m),edge.arrow.size=0.6,edge.width=3,vertex.color=NA,vertex.frame.color=1,vertex.frame.width=2,vertex.label.cex=1.2,vertex.size=33,vertex.label.family="sans")
text(-0.0,-1.6,'Partial Order')
dev.off()}

###
#collect results from main MCMC runs
#new runs
output.dir="C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/"
source(file="C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/outputfun.R")
yoi=c(55:57)

#for comp with old version/runs restrict to selected diocese
# doi=sort(c("Worcester","London","Winchester","Hereford",
#                      "Chichester","Durham","Chester","Lincoln","Salisbury","St Davids",
#                      "Evreux","Ely","Carlisle","Lisieux","Sees","Norwich","Bayeux","Avranches","Coutances","Rochester"),decreasing=FALSE)

#for betas etc in sec 6.2
output=outputanalysis(out.dir=output.dir,out.file="T-M2-down-sub-uc-ds-dir/T-RS5-M2-down-sub-uc-ds.RData",
                      burn=100,yoi=yoi,pdf.file="paper/T-RS5-M2-down-sub-uc-ds",P.samples=NA,full.analysis=TRUE,U.con=FALSE)
output=outputanalysis(out.dir=output.dir,out.file="T-M2-down-sub-uc-ds-dir/T-RS5-M2-down-sub-uc-ds.RData",
                      burn=100,yoi=yoi,pdf.file=NA,P.samples=NA,full.analysis=FALSE,U.con=FALSE)

output=outputanalysis(out.dir=output.dir,out.file="T-M2-up-sub-uc-os-dir/T-RS5-M2-up-sub-uc-os.RData",
                      burn=100,yoi=yoi,pdf.file="paper/T-RS5-M2-up-sub-uc-os",P.samples=NA,full.analysis=TRUE,U.con=FALSE)
output=outputanalysis(out.dir=output.dir,out.file="T-M2-down-sub-uc-os-dir/T-RS5-M2-down-sub-uc-os.RData",
                      burn=100,yoi=yoi,pdf.file="paper/T-RS5-M2-down-sub-uc-os",P.samples=NA,full.analysis=TRUE,U.con=FALSE)
output=outputanalysis(out.dir=output.dir,out.file="T-M2-down-sub-cn-ds-dir/T-RS5-M2-down-sub-cn-ds.RData",
                      burn=100,yoi=yoi,pdf.file="paper/T-RS5-M2-down-sub-cn-ds",P.samples=NA,full.analysis=TRUE,U.con=FALSE)

#for main results in sec 6.3
output=outputanalysis(out.dir=output.dir,out.file="T-M2-down-sub-cn-os-dir/T-RS5-M2-down-sub-cn-os.RData",
                      burn=100,yoi=yoi,pdf.file="paper/T-RS5-M2-down-sub-cn-os",P.samples=NA,full.analysis=TRUE,U.con=FALSE)
yoi=c(1:76) #show the whole 76 years of transitive reductions of consensus POs
output=outputanalysis(out.dir=output.dir,out.file="T-M2-down-sub-cn-os-dir/T-RS5-M2-down-sub-cn-os.RData",
                      burn=100,yoi=yoi,pdf.file="paper/T-RS5-M2-down-sub-cn-os-all-yoi",P.samples=NA,full.analysis=TRUE,U.con=FALSE)

#for K=22 in appendix
yoi=c(55:57)
output=outputanalysis(out.dir=output.dir,out.file="T-M-down-sub-cn-os-dir/T-RS6-M-down-sub-cn-os.RData",
                      burn=100,yoi=yoi,pdf.file="paper/T-RS6-M-down-sub-cn-os",P.samples=NA,full.analysis=TRUE,U.con=FALSE)
output=outputanalysis(out.dir=output.dir,out.file="T-M-down-sub-cn-os-dir/T-RS6-M-down-sub-cn-os.RData",
                      burn=100,yoi=yoi,pdf.file=NA,P.samples=NA,full.analysis=FALSE,U.con=FALSE)

#for K=2
output=outputanalysis(out.dir=output.dir,out.file="H-MK2-down-sub-cn-os-dir/H-MK2-down-sub-cn-os.RData",
                      burn=1,yoi=yoi,pdf.file=NA,P.samples=NA,full.analysis=TRUE,U.con=FALSE)
output=outputanalysis(out.dir=output.dir,out.file="T-MK2-up-sub-uc-os-dir/T-MK2-up-sub-uc-os.RData",
                      burn=1,yoi=yoi,pdf.file=NA,P.samples=NA,full.analysis=TRUE,U.con=FALSE)

#for synth example
output=outputanalysis(out.dir=output.dir,out.file="H-M2-down-sub-uc-os-synth-b-dir/H-RS4-M2-down-sub-uc-os-synth-b.RData",
                      burn=100,yoi=yoi,pdf.file="paper/H-RS4-M2-down-sub-uc-os-synth-b",P.samples=NA,full.analysis=TRUE,U.con=FALSE)
output=outputanalysis(out.dir=output.dir,out.file="H-M2-down-sub-uc-os-synth-b-dir/H-RS4-M2-down-sub-uc-os-synth-b.RData",
                      burn=100,yoi=yoi,pdf.file=NA,P.samples=NA,full.analysis=TRUE,U.con=FALSE)

#output=outputanalysis(out.dir=output.dir,out.file="H-M2-down-sub-uc-ds-synth-c-dir/H-RS3-M2-down-sub-uc-ds-synth-c.RData",
#                      burn=100,yoi=yoi,pdf.file="paper/H-RS3-M2-down-sub-uc-ds-synth-c",P.samples=NA,full.analysis=TRUE,U.con=FALSE)


###
#VSP stuff

n=5
m=matrix(c(0,1,1,1,1,0,0,1,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0),n,n,byrow=TRUE)
colnames(m)<-rownames(m)<-1:n

pdf('paper/fivevertexTREE.pdf',5,5)
g=po2tree(m)
showTREE(g$tree,edge.arrow.size=0.8,vertex.color=NA,vertex.frame.color=1,vertex.frame.width=2,vertex.label.cex=1.2,vertex.size=25,vertex.label.family="sans")
dev.off()

#pdf('paper/forbiddensubgraph.pdf',5,5)
pdf('paper/intro_example_PO.pdf',5,5)
b<-matrix(c(0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0),4,4,byrow=TRUE)
row.names(b)<-colnames(b)<-c("54","57","47","58")
bg<-graph_from_adjacency_matrix(b,mode ="directed")
showDAG(b,edge.arrow.size=1.5,edge.width=3,vertex.color=NA,vertex.frame.color=1,
        vertex.frame.width=5,vertex.label.cex=1.6,vertex.size=50,vertex.label.family="sans")
#showDAG(b,edge.arrow.size=0.6,vertex.color=NA,vertex.frame.color=1,
#        vertex.frame.width=2,vertex.label.cex=1.2,vertex.size=37,vertex.label.family="sans")
dev.off()


###
# look at the data set and see how big it is and make some nice plots
#data setup

#this is the main setup we actually used
note=list()
note$B=1080                  #cut data to after this year or equal
note$E=1155                  #cut data to before this year or equal
if (note$B>note$E) stop('B before E')
note$justbishops=TRUE

#model setup
note$selectlists='half'       #fraction of list time which must overlap target interval 'strict' is all, 'half' is 50% and 'any' is any 
note$maxlistlength=Inf       #what list lengths allowed? NA or Inf is everything useful. Set to say 14 to knock out a few very long lists if speed an issue
note$removebadlists=TRUE
note$VERBOSE=FALSE

#load and form the data
#time window

B=note$B;E=note$E; T=E-B+1

#ordinatation and death dates for each bishop
note$bishopdatefile= "BishopDates-25-7-22b.csv" # "BishopDates-4-11-19.csv" #

#can select just bishops in a subset of diocese.
note$doi=c("Lincoln", "Durham", "Chester", "Sherborne", "Winchester", "Chichester", "Bayeux", "Lisieux","Evreux",
           "Sees", "Avranches", "Coutances", "Exeter", "London", "Rochester", "Worcester","Salisbury", "Bath",
           "Thetford", "Wells", "Le Mans", "Hereford", "Bangor", "Ely",
           "St Davids", "Norwich", "Carlisle", "St Asaph", "the Orkneys", "Llandaff")

# Put dioceses in the following order if you want the exact same bishop numbering rule as in the main new runs
# note$doi.core=sort(c("Worcester","London","Winchester","Hereford",
#                      "Chichester","Durham","Chester","Lincoln","Salisbury","St Davids",
#                      "Evreux","Ely","Carlisle","Lisieux","Sees","Norwich","Bayeux","Avranches","Coutances","Rochester"),decreasing=TRUE)
# note$doi=c(note$doi.core,sort(c("Exeter","Bath",
#                                 "Thetford","Bangor","Llandaff"),decreasing=TRUE))


note$min.lists.per.bishop=2

D=makedata(note,B,E,T) #D is the data we actually analysed in sections 6.2 and 6.3
lid=sapply(D$cla,function(x){x$id})
D$cla[lid==2364] #example list in paper - need doi in right order to get same numbering as MCMC runs etc


#check how much was lost when we require 2 lists per bishop above
#keep all the setup the same but change the min.lists.per.bishop now to one

note$min.lists.per.bishop=1 #how big is the data if we keep all the bishops
D3=makedata(note,B,E,T) #D2 is the data if require at least one list per bishop

lid3=sapply(D3$cla,function(x){x$id})
dl=setdiff(lid3,lid)
D3$cla[which(lid3==dl[1])]
lmi=match(lid,lid3)
ll3=sapply(D3$cla[lmi],function(x) {x$ll})
ll=sapply(D$cla,function(x) {x$ll})
ll[ll3-ll>0]
D$cla[[90]]

#
# Now ALL the data (bishops that is, not lay people) - compared to D3 main thing here is extend time range so dont trim ends 
note$B<-B<-1050; note$E<-E<-1175; T=E-B+1
note$doi=c("Lincoln", "Durham", "Chester", "Sherborne", "Winchester", "Chichester", "Bayeux", "Lisieux","Evreux",
           "Sees", "Avranches", "Coutances", "Exeter", "London", "Rochester", "Worcester","Salisbury", "Bath",
           "Thetford", "Wells", "Le Mans", "Hereford", "Bangor", "Ely",
           "St Davids", "Norwich", "Carlisle", "St Asaph", "Tusculum", "the Orkneys", "Llandaff")
#this is all the dioceses - to check see the file BishopDates-25-7-22b.csv which has all the bishops
note$min.lists.per.bishop=1 #same as setting to 0 as no bishop in zero lists

D2=makedata(note,B,E,T) #all the data excluding bishops in no lists
lid2=sapply(D2$cla,function(x){x$id})
D2$cla[lid2==2364]

di=sapply(D2$cla,function(x){any(x$do==29)}) #Tusculum is dio 29

#what fraction is dimension reduction from dropping inactive bishops
K=11; B=1080; E=1155
#[1080-1155] MLPB=1 dimU=1408
(totUbefore=sum(sapply(D3$cil, function(x){min(E,x$end)-max(B,x$begin)+1})))
#[1080-1155] MLPB=1 dimU=1408
(totUafter=sum(sapply(D$cil,function(x){min(E,x$end)-max(B,x$begin)+1}))) 
(totUbefore-totUafter)/totUbefore

#how do list lengths change when we remove bishops
a=sapply(D3$cla,function(x) matrix(c(x$id,x$ll),2,1))
b=sapply(D$cla,function(x) matrix(c(x$id,x$ll),2,1))
dim(a)[2]-dim(b)[2] #the number of lists drops by 3
m=match(b[1,],a[1,])
all(b[1,]==a[1,m])
plot(jitter(b[2,]),jitter(a[2,m]))
id=which(b[2,]-a[2,m]<0)
b[,id] #show the "after" lists lengths
a[,m][,id] #lengths before thinning
for (i in 1:length(id)) { #where in the lists were the omitted bishops
  print(D3$cla[[m[id[i]]]]$w)
  print(D$cla[[id[i]]]$w)
}
sum(b[2,]<a[2,m])

a=sapply(D2$cil,function(x) matrix(c(x$di,x$ll),2,1))
b=sapply(D2$cil,function(x) x$diocese)
m=matrix(0,1,length(note$doi))
for (i in 1:dim(a)[2]) {
  m[a[1,i]]=m[a[1,i]]+a[2,i]
}
om=order(m[1,],decreasing = TRUE)
dio.list.counts<-m[1,om,drop=FALSE]
colnames(dio.list.counts)<-D2$doi[om]
barplot(dio.list.counts,las=2) #in how many lists does each diocese appear

#
#do main EDA plots
note=list(); note$VERBOSE=FALSE;
#for fig in paper
note$doi=D2$doi[om] #make sure they are ordered by the number of lists witness events per diocese

note$bishopdatefile="BishopDates-25-7-22b.csv" # "BishopDates-4-11-19.csv"#
note$selectlists='any'
note$removebadlists=TRUE
note$min.lists.per.bishop=0


do.pdf=FALSE #TRUE
source("viewdata.R") #make all the EDA graphs in the paper

#
#compare two runs to see how they agree
#

#this was to check if adding more bishops made much diff to the ones already there
setwd("C:/Users/nicholls.NICHOLLS2389/Documents/collab - Kate/newPO/working9")
output1=my.load("../working5/poHB2aRS6.RData")
setwd("C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/")
output2=my.load("T-M2-down-sub-cn-os-dir/T-RS5-M2-down-sub-cn-os.RData")

#this checks K=2 against K=11 for a sec 6.2 style analysis (unconstrained beta)
setwd("C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/")
output1=my.load("T-M2-down-sub-uc-ds-dir/T-RS5-M2-down-sub-uc-ds.RData") #K=11
output2=my.load("T-MK2-up-sub-uc-os-dir/T-MK2-up-sub-uc-os.RData")       #K=2

#this checks K=11 against K=22 or $K=2$ for the 6.3 analysis (constrained beta)
setwd("C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/")
output1=my.load("T-M2-down-sub-cn-os-dir/T-RS5-M2-down-sub-cn-os.RData") #K=11
output2=my.load("T-M-down-sub-cn-os-dir/T-RS6-M-down-sub-cn-os.RData") #K=22
output2=my.load("H-MK2-down-sub-cn-os-dir/H-MK2-down-sub-cn-os.RData") #K=2

#this checks two MCMC runs on the same target (the 6.3 analysis) with ordered nd disordered starts
setwd("C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/")
output1=my.load("T-M2-down-sub-cn-os-dir/T-RS5-M2-down-sub-cn-os.RData") #ordered
output2=my.load("T-M2-down-sub-cn-ds-dir/T-RS5-M2-down-sub-cn-ds.RData") #disordered

bn1=sapply(output1$cil,function(x){x$name.id})
bn2=sapply(output2$cil,function(x){x$name.id})

#which bishops are in common 
shd=intersect(bn1,bn2) #shd becomes the standardised bishop set and rder
bns1=match(shd,bn1)
bns2=match(shd,bn2)


ns=length(shd);ns
new.names=as.character(1:ns)

renumber<-function(h,bns,new.names) {
  #take a single PO indidence matrix and rewrite row/col names so a bishop called bns[i] becomes new.names[i]
  rnh=row.names(h);
  h.to.sh=match(rnh,as.character(bns))
  row.names(h)<-colnames(h)<-new.names[h.to.sh]
  k=!is.na(h.to.sh)
  h<-h[k,k]
  o=order(row.names(h))
  h<-h[o,o]
  return(h)
}

PO1=lapply(output1$PO, function(x) {lapply(x,renumber,bns1,new.names)})
PO2=lapply(output2$PO, function(x) {lapply(x,renumber,bns2,new.names)})

POav<-function(PO,yoi,burn=100){
  pav=lapply(PO[[1]],function(x){0*x})
  n.samples=min(which(sapply(PO,identical,list())))-1
  for (i in burn:n.samples) {
    for (t in yoi) {pav[[t]]=pav[[t]]+PO[[i]][[t]]}
  }
  for (t in yoi) {pav[[t]]=pav[[t]]/(n.samples-burn)}
  return(pav)
}

#calculate the median absolute difference in post mean edge probs & percent agreeing edges
pav1=POav(PO1,yoi=1:76)
pav2=POav(PO2,yoi=1:76)

#percent agreeing edges
Ho=lapply(pav1,function(x){my.transitive.closure(x>0.5)})
pav=pav2
dfn=0.5
Scoring=lapply(1:output1$T, 
               function(t) { 
                 Hoz=Ho[[t]]; diag(Hoz)<-1
                 ge<-my.transitive.closure(pav[[t]]>=dfn)
                 lt<-!ge
                 an=sum( (Hoz==0 & t(Hoz)==0 ) )/2 #num missing relns in true
                 ap=sum( (Ho[[t]]==1 ) )         #num of relns in true
                 fn=sum( (Ho[[t]]==1) & (lt & t(lt)) ) #reln in true not in est
                 fp=sum( (Hoz==0 & t(Hoz)==0) & (ge | t(ge)) )/2 #no reln in true but yes in est
                 tp=sum( (Ho[[t]]==1) & ge) #reln in both - right way round
                 tn=sum( (Hoz==0 & t(Hoz)==0) & (lt & t(lt)) )/2 #no reln in both
                 wr=sum( (Ho[[t]]==1) & t(ge) ) #reln in both but wrong way round
                 n.ht=dim(Ho[[t]])[1]
                 if ( 2*(an+ap)!=(n.ht*(n.ht-1))) stop('ap+an issues')
                 if ( (wr+fn+tp)!= ap) stop('wwr+fn+tp issues')
                 if ( (fp+tn)!= an) stop('fp+tn issues')
                 #ap<-max(ap,1); an<-max(an,1)
                 return(list(fn=fn,fp=fp,tp=tp,tn=tn,wr=wr,ap=ap,an=an))
               })

pdf.file=NA
if (is.na(pdf.file)) {windows()} else {pdf(file=paste(pdf.file,'-false-pos-neg.pdf',sep=''),6,6)} 
dates=1:output1$T+output1$B-1
Fn=sapply(Scoring,function(x) x$fn)
Fp=sapply(Scoring,function(x) x$fp)
Tp=sapply(Scoring,function(x) x$tp)
Tn=sapply(Scoring,function(x) x$tn)
Wr=sapply(Scoring,function(x) x$wr)
y.max=max(c(Fn,Fp,Tp,Tn,Wr))
#y.min=min(c(Fn,Fp,Tp,Tn,Wr))
plot(dates,Fn,col=1,type='l',ylim=c(0,y.max),xlab='calendar year',ylab='positive and negative counts')
lines(dates,Fp,col=2)
lines(dates,Tp,col=3)
lines(dates,Tn,col=4)
lines(dates,Wr,col=5)
legend('topleft',
       legend = c("false -ve order","false +ve order","true +ve order",
                  "true -ve order","order reversal"),
       lty=c(1,1,1,1,1),col=c(1,2,3,4,5),cex=0.8)
plot(dates,ae<-100*(Tp+Tn)/(Tn+Fp+Tp+Fn+Wr),type='l',ylab='% agreeing edges')
#aeK2=ae
#aeK22=ae
quantile(ae,0.5)

if (exists("aeK2") & exists("aeK22")) {
  plot(dates,aeK2,type='l',col=2,ylab='% agreeing edges')
  lines(dates,aeK22,col=1,lty=2)
  legend('bottomleft',col=c(2,1),lty=c(1,2),legend=c("% relations in K=2 also in K=11","% relations in K=2 also in K=22"))
}

#median absolute difference in estimates
adf=abs(unlist(pav1)-unlist(pav2))
quantile(adf[adf>0],0.5) #a bit rough as some 0's will be real zeros not self-relns - over-estimate
hist(adf[adf>0],100,freq=FALSE,main='',xlab='absolute difference')
T=output1$T; df=rep(NA,T)
for (t in 1:T) {
  df[t]=mean( (pav1[[t]]>0.6 & pav2[[t]]<0.4) | (pav2[[t]]>0.6 & pav1[[t]]<0.4) )
}
plot(dates,df)
max(adf)

#number of lin indep rlns
ab<-sapply(PO2[[1]],nrow)
sum(ab*ab-ab)
2*sum(choose(apply(output1$active,2,sum),2)) #sanity check
2*sum(choose(apply(output2$active,2,sum),2)) 

#dimension of U
sum(!is.na(output1$Uout[[1]])) #K=11 is 13453=11 x 1223 and K=22 is 26906
sum(apply(output1$active,2,sum))*output1$NF #this and next should match
sum(sapply(output1$cil, function(x){min(output1$E,x$end)-max(output1$B,x$begin)+1}))*output1$NF #[1080-1155] MLPB=1

sum(!is.na(output2$Uout[[1]])) #K=2 is 2446 etc

#how precise are the MCMC estimates of probabilities
n.samples=min(which(sapply(PO1,identical,list())))-1
n.rls=length(unlist(PO1[[1]]))
smp=matrix(NA,n.samples,n.rls)
for (k in 1:n.samples) {
  smp[k,]=unlist(PO1[[k]])
}
mcmc.sum=summary(as.mcmc(smp[,1:10000]))$statistics[,c(1,4)]
mcmc.sum<-mcmc.sum[mcmc.sum[,1]>0,]
plot(re<-mcmc.sum[,2]/mcmc.sum[,1]) #showing typical relative errs
abline(h=quantile(re,0.5),col=3)
quantile(re,0.5)
plot(mcmc.sum[,1],mcmc.sum[,2]) #the std.err depends on the magnitude of the estimate
#when the estimate is 0.03 what is the typical std.err?
i03<-which(mcmc.sum[,1]<0.031 & mcmc.sum[,1]>0.029)
mean(re[i03])*0.03
mean(mcmc.sum[i03,2])



