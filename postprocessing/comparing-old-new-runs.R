# note$doi=c("Lincoln", "Durham", "Chester", "Sherborne", "Winchester", "Chichester", "Bayeux", "Lisieux","Evreux",
#            "Sees", "Avranches", "Coutances", "Exeter", "London", "Rochester", "Worcester","Salisbury", "Bath",
#            "Thetford", "Wells", "Le Mans", "Hereford", "Bangor", "Ely",
#            "St Davids", "Norwich", "Carlisle", "St Asaph", "the Orkneys", "Llandaff")
note$doi.core=sort(c("Worcester","London","Winchester","Hereford",
"Chichester","Durham","Chester","Lincoln","Salisbury","St Davids",
"Evreux","Ely","Carlisle","Lisieux","Sees","Norwich","Bayeux","Avranches","Coutances","Rochester"),decreasing=TRUE)
note$doi=c(note$doi.core,sort(c("Exeter","Bath",
"Thetford","Bangor","Llandaff"),decreasing=TRUE))
note$min.lists.per.bishop=2
D=makedata(note,B,E,T)
lid=sapply(D$cla,function(x){x$id})
D$cla[lid==2364]
#
note$B<-B<-1050; note$E<-E<-1175; T=E-B+1
note$doi=c("Lincoln", "Durham", "Chester", "Sherborne", "Winchester", "Chichester", "Bayeux", "Lisieux","Evreux",
"Sees", "Avranches", "Coutances", "Exeter", "London", "Rochester", "Worcester","Salisbury", "Bath",
"Thetford", "Wells", "Le Mans", "Hereford", "Bangor", "Ely",
"St Davids", "Norwich", "Carlisle", "St Asaph", "Tusculum", "the Orkneys", "Llandaff")
note$min.lists.per.bishop=1
D2=makedata(note,B,E,T)
names(D2)
doi
output$doi
D2$doi
setdiff(D2$doi,output$doi)
D2$cla[1]
D2$doi
di=sapply(D2$cla,function(x){any(x$do==29)})
sum(di)
D2$cla[which(di)]
note
output$note$min.lists.per.bishop
###
#data setup
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
B=note$B;E=note$E; T=E-B+1
note$bishopdatefile= "BishopDates-25-7-22b.csv" # "BishopDates-4-11-19.csv" #
note$doi=c("Lincoln", "Durham", "Chester", "Sherborne", "Winchester", "Chichester", "Bayeux", "Lisieux","Evreux",
"Sees", "Avranches", "Coutances", "Exeter", "London", "Rochester", "Worcester","Salisbury", "Bath",
"Thetford", "Wells", "Le Mans", "Hereford", "Bangor", "Ely",
"St Davids", "Norwich", "Carlisle", "St Asaph", "the Orkneys", "Llandaff")
note$min.lists.per.bishop=2
note$min.lists.per.bishop=1
D=makedata(note,B,E,T)
note$doi=c("Lincoln", "Durham", "Chester", "Sherborne", "Winchester", "Chichester", "Bayeux", "Lisieux","Evreux",
"Sees", "Avranches", "Coutances", "Exeter", "London", "Rochester", "Worcester","Salisbury", "Bath",
"Thetford", "Wells", "Le Mans", "Hereford", "Bangor", "Ely",
"St Davids", "Norwich", "Carlisle", "St Asaph", "Tusculum", "the Orkneys", "Llandaff")
note$min.lists.per.bishop=2
D=makedata(note,B,E,T)
D$doi
note$doi=c("Lincoln", "Durham", "Chester", "Sherborne", "Winchester", "Chichester", "Bayeux", "Lisieux","Evreux",
"Sees", "Avranches", "Coutances", "Exeter", "London", "Rochester", "Worcester","Salisbury", "Bath",
"Thetford", "Wells", "Le Mans", "Hereford", "Bangor", "Ely",
"St Davids", "Norwich", "Carlisle", "St Asaph", "Tusculum", "the Orkneys", "Llandaff")
note$min.lists.per.bishop=1
D=makedata(note,B,E,T)
note$doi=c("Lincoln", "Durham", "Chester", "Sherborne", "Winchester", "Chichester", "Bayeux", "Lisieux","Evreux",
"Sees", "Avranches", "Coutances", "Exeter", "London", "Rochester", "Worcester","Salisbury", "Bath",
"Thetford", "Wells", "Le Mans", "Hereford", "Bangor", "Ely",
"St Davids", "Norwich", "Carlisle", "St Asaph", "the Orkneys", "Llandaff")
note$min.lists.per.bishop=1
D=makedata(note,B,E,T)
D2$cil[1]
totU=sum(sapply(D2$cil,function(x){x$end-x$begin}))
totU
totUafter=sum(sapply(D$cil,function(x){x$end-x$begin}))
totUbefore=sum(sapply(D2$cil,function(x){x$end-x$begin}))
totUafter=sum(sapply(D$cil,function(x){x$end-x$begin}))
totUbefore-totUafter
totUafter
(totUbefore-totUafter)/totUbefore
NL
3/371
names(D)
names(D2)
di=sapply(D2$cla,function(x){any(x$do==29)})
di
which(di)
lid2=sapply(D2$cla,function(x){x$id})
D2$cla[lid2==2364]
D$cla[lid==2364]
length(lid)
length(lid2)
note$doi=c("Lincoln", "Durham", "Chester", "Sherborne", "Winchester", "Chichester", "Bayeux", "Lisieux","Evreux",
"Sees", "Avranches", "Coutances", "Exeter", "London", "Rochester", "Worcester","Salisbury", "Bath",
"Thetford", "Wells", "Le Mans", "Hereford", "Bangor", "Ely",
"St Davids", "Norwich", "Carlisle", "St Asaph", "the Orkneys", "Llandaff")
note$min.lists.per.bishop=1
note$min.lists.per.bishop=1
D3=makedata(note,B,E,T)
lid3=sapply(D3$cla,function(x){x$id})
setdiff(lid3,lid)
dl=setdiff(lid3,lid)
D3$cla[dl]
D3$cla[which(lid3==dl)]
D3$cla[which(lid3==dl[1])]
D3$cla[which(lid3==dl[2])]
D3$cla[which(lid3==dl[3])]
lmi=match(lid3,lid)
lmi
lmi=match(lid,lid3)
lmi
lmi=match(lid,lid3)
ll3=sapply(D3$cla[lmi],function(x) {x$ll})
ll=sapply(D$cla,function(x) {x$ll})
ll3-ll
length(lmi)
D3$cla[[374]]
D$cla[[371]]
lid
lid3
D3$cla[[374]]
D$cla[[371]]
note$min.lists.per.bishop=2
D=makedata(note,B,E,T)
lid=sapply(D$cla,function(x){x$id})
note$min.lists.per.bishop=1
D3=makedata(note,B,E,T)
lid3=sapply(D3$cla,function(x){x$id})
dl=setdiff(lid3,lid)
D3$cla[which(lid3==dl[1])]
lid
D$cla[[371]]
D3$cla[[374]]
lmi=match(lid,lid3)
ll3=sapply(D3$cla[lmi],function(x) {x$ll})
ll=sapply(D$cla,function(x) {x$ll})
ll3-ll
which(ll3-ll>0)
which(ll3-ll>1)
D$cla[[90]]
ll[ll3-ll>0]
ll[ll3-ll>0]
(ll3-ll)[ll3-ll>0]
7/374
setwd("C:/Users/nicholls.NICHOLLS2389/Documents/collab - Kate/newPO/working9")
dir('../working5')
output1=my.load("../working5/poHB2aRS6.RData")
setwd("C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/")
output2=my.load("T-M2-down-sub-cn-os-dir/T-RS5-M2-down-sub-cn-os.RData")
output1$cil[[1]]
bn1=sapply(output1$cil,function(x){x$name.id})
bn1
bn2=sapply(output2$cil,function(x){x$name.id})
bn2
bni=match(bn1,bn2)
bni
setdiff(bn1,bn2)
setdiff(bn2,bn1)
shd=intersect(bn1,bn2)
shd
bns1=match(shd,bn1)
bns1
bns2=match(shd,bn2)
bns2
names(output1)
output1$PO[[1]]
h=output1$PO[[1]][[54]]
h
output1$cil[[1]]
nn1=sapply(output1$cil,function(x){x$node.name})
nn2=sapply(output2$cil,function(x){x$node.name})
ns=length(shd)
ns
new.names=1:ns
row.names(h)
new.names=as.character(1:ns)
new.names
rnh=row.names(h)
nn1
nn1[bns1]
nn2[bns2]
output1$cil[bns1[1]]
output2$cil[bns2[1]]
output1$cil[bns1[56]]
output2$cil[bns2[56]]
bns2[56]
bns1
bns2
strmatch(rnh,as.character(bns1))
str.match(rnh,as.character(bns1))
?match
match(rnh,as.character(bns1))
rnh=row.names(h); ho=h
row.names(h)<-colnames(h)<-new.names(match(rnh,as.character(bns1)))
ns=length(shd)
new.names=as.character(1:ns)
rnh=row.names(h); ho=h
row.names(h)<-colnames(h)<-new.names(match(rnh,as.character(bns1)))
row.names(h)<-colnames(h)<-new.names[match(rnh,as.character(bns1))]
h
ho
h=ho
h
rnh
bns1
rnh=row.names(h); h1=h2==h
rnh=row.names(h); h1=h2=h
row.names(h2)<-colnames(h2)<-new.names[match(rnh,as.character(bns2))]
h
h2
rnh
bns2
any(bns2==35)
any(bns2==46)
any(bns2==44)
match(rnh,as.character(bns2))
h.to.sh1=match(rnh,as.character(bns1))
h.to.sh2=match(rnh,as.character(bns2))
k1=!is.na(h.to.sh1)
k1
k2=!is.na(h.to.sh2)
k2
h.to.sh1=match(rnh,as.character(bns1))
h.to.sh2=match(rnh,as.character(bns2))
rnh=row.names(h); h1=h2=h
h.to.sh1=match(rnh,as.character(bns1))
h.to.sh2=match(rnh,as.character(bns2))
row.names(h1)<-colnames(h1)<-new.names[h.to.sh1]
row.names(h2)<-colnames(h2)<-new.names[h.to.sh2]
k1=!is.na(h.to.sh1)
k2=!is.na(h.to.sh2)
h1<-h1[k1,k1]
h2<-h2[k2,k2]
h
h1
h2
h=output1$PO[[1]]
h
h=output1$PO[[1]][[57]]
h
g=output2$PO[[1]][[57]]
g
rnh=row.names(h); h1=h
rng=row.names(g); g2=g
h.to.sh1=match(rnh,as.character(bns1))
g.to.sh2=match(rng,as.character(bns2))
row.names(h1)<-colnames(h1)<-new.names[h.to.sh1]
row.names(g2)<-colnames(g2)<-new.names[g.to.sh2]
k1=!is.na(h.to.sh1)
k2=!is.na(g.to.sh2)
h1<-h1[k1,k1]
g2<-g2[k2,k2]
h1
g2
?lapply
renumber<-function(h,bns,new.names) {
#take a single PO indidence matrix and rewrite row/col names so a bishop called bns[i] becomes new.names[i]
rnh=row.names(h);
h.to.sh=match(rnh,as.character(bns))
row.names(h)<-colnames(h)<-new.names[h.to.sh]
k=!is.na(h.to.sh)
h<-h[k,k]
return(h)
}
PO1=lapply(output1$PO, function(x) {lapply(x,renumber,bns1,new.names)})
PO1=lapply(output1$PO, function(x) {lapply(x,renumber,bns1,new.names)})
PO2=lapply(output2$PO, function(x) {lapply(x,renumber,bns2,new.names)})
PO1[[1000]][[44]]
PO2[[1000]][[44]]
diff(bnms1)
diff(bns1)
diff(bns2)
h
v=row.names(h)
g
g2
v=row.names(g2)
order(v)
g
brs2
bns2
order(bns2)
bns2[order(bns2)]
v
o=order(v)
g2[o,o]
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
PO1[[1000]][[44]]
PO2[[1000]][[44]]
pav1=lapply(PO1[[1]],function(x){0*x})
pav1
POav<-function(PO,yoi,burn=100){
pav=lapply(PO[[1]],function(x){0*x})
n.samples=length(PO)
for (i in burn:n.samples) {
for (t in yoi) {pav[[t]]=pav[[t]]+PO[[i]][[t]]}
}
for (t in yoi) {pav[[t]]=pav[[t]]/n.samples}
}
yoi
pav1=POav(PO1,yoi=1:76)
PO=PO1
pav=lapply(PO[[1]],function(x){0*x})
n.samples=length(PO)
yoi=1:76
burn=11
burn=100
for (i in burn:n.samples) {
for (t in yoi) {pav[[t]]=pav[[t]]+PO[[i]][[t]]}
}
i
t
PO[[2252]]
PO[[2253]]
names(output1)
PO[[2251]]
n.samples
x=sapply(PO,identical,list())
min(which(x))
POav<-function(PO,yoi,burn=100){
pav=lapply(PO[[1]],function(x){0*x})
n.samples=min(which(sapply(PO,identical,list())))-1
for (i in burn:n.samples) {
for (t in yoi) {pav[[t]]=pav[[t]]+PO[[i]][[t]]}
}
for (t in yoi) {pav[[t]]=pav[[t]]/(n.samples-burn)}
}
pav1=POav(PO1,yoi=1:76)
pav2=POav(PO2,yoi=1:76)
par(mfrow(1,2))
par(mfrow=c(1,2))
image(pav1[[44]])
pav1[[44]]
pav1
POav<-function(PO,yoi,burn=100){
pav=lapply(PO[[1]],function(x){0*x})
n.samples=min(which(sapply(PO,identical,list())))-1
for (i in burn:n.samples) {
for (t in yoi) {pav[[t]]=pav[[t]]+PO[[i]][[t]]}
}
for (t in yoi) {pav[[t]]=pav[[t]]/(n.samples-burn)}
return(pav)
}
pav1=POav(PO1,yoi=1:76)
pav1
pav2=POav(PO2,yoi=1:76)
par(mfrow=c(1,2))
image(pav1[[44]])
image(pav2[[44]])
Ht=lapply(pav1,function(x){x>0.5})
showDAGs(Ht)
showDAGs
showDAGs(1080,76,Ht)
showDAGs(1080,76,lapply(pav2,function(x){x>0.5}))
dfn=0.5
pav=pav2
Ho=lapply(pav1,function(x){x>0.5})
pav=pav2
dfn=0.5
Scoring=lapply(1:T,
function(t) {
Hoz=Ho[[t]]; diag(Hoz)<-1
an=sum( (Hoz==0 & t(Hoz)==0 ) ) #num pairs of nodes with no reln
ap=sum( (Ho[[t]]==1 ) )                   #num of nodes on the > side of a reln
fn=sum( (Ho[[t]]==1) & ( pav[[t]]<dfn & t(pav[[t]])<dfn ) ) #reln in true not in est
fp=sum( (Hoz==0 & t(Hoz)==0) & ( pav[[t]]>=dfn | t(pav[[t]])>=dfn ) ) #no reln in true but yes in est
tp=sum( (Ho[[t]]==1) & (pav[[t]]>=dfn )) #reln in both - right way round
tn=sum( (Hoz==0 & t(Hoz)==0) & ( pav[[t]]<dfn & t(pav[[t]])<dfn ) ) #no reln in both
wr=sum( (Ho[[t]]==1) & (t(pav[[t]])>=dfn) ) #reln in both but wrong way round
n.ht=dim(Ho[[t]])[1]
if ( (an+2*ap)!=(n.ht*(n.ht-1))) stop('ap+an issues')
if ( (wr+fn+tp)!= ap) stop('wwr+fn+tp issues')
if ( (fp+tn)!= an) stop('fp+tn issues')
#ap<-max(ap,1); an<-max(an,1)
return(list(fn=fn,fp=fp,tp=tp,tn=tn,wr=wr,ap=ap,an=an))
})
output1$T
dates=1:output1$T+B-1
Scoring
Fn=sapply(Scoring,function(x) x$fn)
Fp=sapply(Scoring,function(x) x$fp)
Tp=sapply(Scoring,function(x) x$tp)
Tn=sapply(Scoring,function(x) x$tn)
Wr=sapply(Scoring,function(x) x$wr)
y.max=max(c(Fn,Fp,Tp,Tn,Wr))
#y.min=min(c(Fn,Fp,Tp,Tn,Wr))
plot(dates,Fn,col=1,type='l',ylim=c(0,y.max),xlab='calendar year',ylab='positive and negative counts')
#y.min=min(c(Fn,Fp,Tp,Tn,Wr))
plot(dates,Fn,col=1,type='l',ylim=c(0,y.max),xlab='calendar year',ylab='positive and negative counts')
lines(dates,Fp,col=2)
lines(dates,Tp,col=3)
lines(dates,Tn,col=4)
lines(dates,Wr,col=5)
legend('topleft',legend = c("false -ve order","false +ve order","true +ve order","true -ve order","order reversal"),lty=c(1,1,1,1,1),col=c(1,2,3,4,5))
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
plot(Tp+Tn/(Tp+Tn+Fp+Fn+Wr))
plot((Tp+Tn)/(Tp+Tn+Fp+Fn+Wr))
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
plot((Tp+Tn)/(Tp+Tn+Fp+Fn+Wr))
plot(dates,(Tp+Tn)/(Tp+Tn+Fp+Fn+Wr))
plot(dates,(Tp+Tn)/(Tp+Tn+Fp+Fn+Wr),ylab='% agreeing edges')
T=output1$T; df=rep(0,T)
for (t in 1:T) {
df[t]=max(abs(pav1[t]-pav2[t]))
}
T
df
T=output1$T; df=rep(0,T)
for (t in 1:T) {
df[t]=max(abs(pav1[[t]]-pav2[[t]]))
}
plot(df)
which.max(df)
pav1[[28]]
pav2[[28]]
par(mfrow=c(1,2)); image(pav1[[28]])image(pav1[[28]]);
par(mfrow=c(1,2)); image(pav1[[28]]);image(pav1[[28]]);
par(mfrow=c(1,2)); image(pav1[[28]]);image(pav2[[28]]);
par(mfrow=c(1,2)); image(pav1[[28]]-pav2[[28]]);
par(mfrow=c(1,2)); pav1[[28]]-pav2[[28]]>0.5
par(mfrow=c(1,2)); pav1[[28]]-pav2[[28]]>0.7
par(mfrow=c(1,2)); pav1[[28]]-pav2[[28]]>0.2
showDAGs(list(pav1[[28]]>0.5,pav2[[28]]>0.5))
showDAGs(output1$B,output1$T,list(pav1[[28]]>0.5,pav2[[28]]>0.5))
showDAGs(1,2,list(pav1[[28]]>0.5,pav2[[28]]>0.5))
T=output1$T; unlist(pav1)-unlist(pav2)
hist(abs(unlist(pav1)-unlist(pav2)))
hist(abs(unlist(pav1)-unlist(pav2)))
hist(abs(unlist(pav1)-unlist(pav2)),100)
hist(abs(unlist(pav1)-unlist(pav2)),100,freq=FALSE)
hist(abs(unlist(pav1)-unlist(pav2)),100,freq=TRUE)
hist(abs(unlist(pav1)-unlist(pav2)),100,freq=FALSE)
plot(unlist(pav1),unlist(pav2))
plot(unlist(pav1),unlist(pav2),pch='.')
T=output1$T; df=rep(NA,T)
for (t in 1:T) {
df[t]=mean( (pav1[[t]]>0.6 & pav2[[t]]<0.4) | (pav2[[t]]>0.6 & pav1[[t]]<0.4) )
}
plot(dates,df)
hist(abs(unlist(pav1)-unlist(pav2)),100,freq=FALSE)
hist(abs(unlist(pav1)-unlist(pav2)),100,freq=FALSE,main='',xlab='absolute difference')
pav1[[1]]
pav2[[1]]
adf=abs(unlist(pav1)-unlist(pav2))
hist(adf[adf>0]),100,freq=FALSE,main='',xlab='absolute difference')
hist(adf[adf>0],100,freq=FALSE,main='',xlab='absolute difference')
mean(adf[adf>0])
T=output1$T; df=rep(NA,T)
for (t in 1:T) {
df[t]=mean( (pav1[[t]]>0.6 & pav2[[t]]<0.4) | (pav2[[t]]>0.6 & pav1[[t]]<0.4) )
}
plot(dates,df)
plot(dates,(Tp+Tn)/(Tp+Tn+Fp+Fn+Wr),ylab='% agreeing edges')
hist(adf[adf>0],100,freq=FALSE,main='',xlab='absolute difference')
mean(adf[adf>0])
quantile(adf[adf>0],0.5)
sum(adf>0)
max(adf)
min(adf)
min(adf[adf>0])
showDAGs(output1$B,output1$T,list(pav1[[28]]>0.5,pav2[[28]]>0.5))
showDAGs(1,2,list(pav1[[28]]>0.5,pav2[[28]]>0.5))
pav1[[28]]
showDAGs(1,2,list(pav1[[28]]>0.9,pav2[[28]]>0.9))
showDAGs(1,2,list(pav1[[28]]>0.5,pav2[[28]]>0.5))
quantile(adf[adf>0],0.5)
hist(adf[adf>0],100,freq=FALSE,main='',xlab='absolute difference')
hist(adf[adf>0],100,freq=FALSE,main='',xlab='absolute difference')
showDAGs(11134,3,lapply(pav2,function(x){x>0.5}))
showDAGs(11134,3,lapply(pav1,function(x){x>0.5}))
76*20
length(adf)
76*20^2
sapply(PO1[[1]],function(x){a=dim(x); a*(a-1)})
sapply(PO1[[1]],function(x){a=dim(x)[1]; a*(a-1)})
sum(sapply(PO1[[1]],function(x){a=dim(x)[1]; a*(a-1)}))
length(adf[adf>0])
quantile(adf[adf>0],0.95)
history(1000)
savehistory("C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/postprocessing/comparing-old-new-runs.R")
