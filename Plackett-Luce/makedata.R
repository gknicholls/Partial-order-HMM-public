
########################################################################################
#Kevin, "cla" and "cil" and V$who, V$tio and V$drk are probably the main thinks you need

#Sorting the dates/dioceses/lists for consistency 

rm(list=ls())

library(MASS)
#library(nem)
#library(igraph) #TODO 3/8/10 masks stuff - is this OK?
#library(Rgraphviz); library(graph) #is this needed?
setwd("C:/Users/nicholls/Documents/teaching - college/Kevin")
source(file="dating.R")

##################################################################################
#Get the data
#this is the version of makejkdata which outputs diocese data in qnames
detach(out)
out<-makejkdata(on=0,off=2000,takebishops=TRUE,include.nodates=TRUE,include.singletons=FALSE,remove.badbishops=TRUE)
attach(out) #bring la, qnames and pnames into the environment

##################################################################################
#remove some lists that seem problematic
bad.lists=c(30,677,2469,2627) #actually 5 is fine it just has NA dates, could easilly be fixed
lids=unlist(lapply(la,function(x){x$id}))
bli=which(is.element(lids,bad.lists))
la<-la[-bli]
#one might also look at the Authenticity Rating la[[i]]$ac
#and remove lists with low AR's

##################################################################################
#form lists bil & pil - these lists reorganise the data to give info bishop-by-bishop
DioDates=Get.DioDates(la,qnames)
bil=DioDates$ProcessedBishops #these bishop/diocese/dates are consistent
pil=DioDates$OriginalBishops
dioceses=unlist(lapply(bil,function(x){x$diocese}))
La=DioDates$ProcessedLists    #these lists only contain consistent bishops
  
qnames=DioDates$Qnames
all(qnames$diocese==dioceses)#sanity check
dio.names<-unique(dioceses)

#filename="Bishop_Dat_Visual.pdf"
#plot.dio.dates(bil,dio.names,file=filename)
plot.dio.dates(bil,dio.names) #or pil to see the unprocessed data

#sanity check that the node.name of a bishop is the same as its index in bil
for (j in 1:length(la)) 
  if (!all(unlist(lapply( bil[la[[j]]$o], function(x){x$name}))==la[[j]]$names)) 
    stop("oh bother")
unlist(lapply( bil, function(x){x$node.name}))

##################################################################################
# bil now gives dates for each bishop and their diocese, consistent for building X
# bil also contains the list info for each bishop, but X doesnt need that

#doi: Dioceses Of Interest
table(dioceses) #how many bishops in each diocese over period of interest [B,E]
dio.names #we typically pocus on a few dioceses of interest - in this example 10
doi<-sort(c("Worcester","London","Exeter","Winchester","Hereford",
            "Chichester","Durham","Lincoln","Salisbury","St Davids",
            "Evreux","Ely","Carlisle","Lisieux","Sees","Norwich"))
doi==sort(intersect(doi,dio.names)) #check the names are spelt correctly!

##################################################################################
# Make X for the DOI using bil - specify period of interest - also extracts bishops 
# of interest

#V$who[d,y]=index in bil for bishop in dio d in year y
#V$tio[d,y]=time in office for current holder of diocese d in year y
#V$drk[d,y]=rank of diocese d in year y
#V$X is X
B=1115;E=1145

#TODO - we could do something with tio so that we dont update U in (d,t) pairs where dio d is empty
V=getX(doi,bil,B=1115,E=1145)
image(V$tio) #sanity check
image(V$drk)
X=V$X
Dcp=(V$tio<=1) #Dcp[d,t] is TRUE if either no bishop or new bishop in diocese d in year t
image(Dcp)
boi=V$boi

###########################################################################
# repackage the data now we know who and when the focus is

#Extract the subset of bishops and lists in the interval of interest
tl=unlist(lapply(La,function(x) x$tl))
tu=unlist(lapply(La,function(x) x$tu))
loi=which(tl<=E & tu>=B)

#cut the lists - messes up number indexing from list to bishop and vv (solve using "id")
cla=La[loi]
cil=bil[boi]

cil.id=unlist(lapply(cil,function(x) x$name.id))
drop=c()
for (i in 1:length(cla)) {
  si=which(is.element(cla[[i]]$w,cil.id))
  cla[[i]]$w=cla[[i]]$w[si]
  cla[[i]]$names=cla[[i]]$names[si]
  cla[[i]]$o=match(cla[[i]]$w,cil.id)
  cla[[i]]$ll=length(cla[[i]]$w)
  if (cla[[i]]$ll<1) drop=c(drop,i)
}
cla=cla[-drop]

cla.id=unlist(lapply(cla,function(x) x$id))
for (i in 1:length(cil)) {
  si=which(is.element(cil[[i]]$lid,cla.id))
  cil[[i]]$lid=cil[[i]]$lid[si]
  cil[[i]]$ln=match(cil[[i]]$lid,cla.id)
  cil[[i]]$tl=cil[[i]]$tl[si]
  cil[[i]]$tu=cil[[i]]$tu[si]
  cil[[i]]$ac=cil[[i]]$ac[si]
  cil[[i]]$ll=length(cil[[i]]$lid)
  cil[[i]]$di=which(cil[[i]]$diocese==doi) #dimnames(X)[1] should be exactly same as doi
}

#added 18/04/2019 - list la becomes cla after we restrict to doi/[B,E]
#Now write in diocese version of witness list cla[[]]$o - this goes in cla[[]]$do
for (i in 1:length(cla)) {
  o=cla[[i]]$o
  cilo=cil[o]
  cla[[i]]$do=unlist(lapply(cilo,function(x) x$di))
}
#Would be good to check order in cla[[]]$do is all correct, going right back to la[[]]$w
#following checks cil & cla(o&do) still match up
#a=unlist(lapply(cil[unlist(lapply(cla,function(x) x$o))],function(x) x$diocese))
#b=doi[unlist(lapply(cla,function(x) x$do))]
#all(a==b)

plot.dio.dates(cil,doi)


cla[[3]]
