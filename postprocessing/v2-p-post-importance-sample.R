
#
# QUEUE JUMPING ERR PROB p - post distribution - reweighting both subjective <-> uniform
# Importance sampling posterior p dbn (post with subj p prior) to get posterior for unif p prior
# and posterior p dbn (post with unif p prior) to get posterior for subj p prior
#
# Later on just redid runs with unif and subj p priors - though some long runs have no double
#

################################################
#importance reweighting for unif prior
output.dir="C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/"
setwd(output.dir)

output=my.load("T-M2-down-sub-uc-ds-dir/T-RS5-M2-down-sub-uc-ds.RData")
P=output$P
burn=100
p=P[-c(1:burn),"p"]
p<-p[!is.na(p)]
theta=P[-c(1:burn),"theta"]
theta<-theta[!is.na(theta)]
rho=P[-c(1:burn),"rho"]
rho<-rho[!is.na(rho)]

output=my.load("T-M2-down-sub-uc-os-dir/T-RS5-M2-down-sub-uc-os.RData")
P1=output$P
p1=P1[-c(1:burn),"p"]
p1<-p1[!is.na(p1)]
theta1=P1[-c(1:burn),"theta"]
theta1<-theta1[!is.na(theta1)]
rho1=P1[-c(1:burn),"rho"]
rho1<-rho1[!is.na(rho1)]

#unnormalised importance weights
w=1/dbeta(p,shape1=1,shape2=0.3333333,ncp=8)
#normalised
w<-w/sum(w)

# not much diff in means
sum(p*w)
mean(p)


#now sample p's according to weights should give post using unif prior?
pdf('paper/v2-poHB1aRS6-rho-theta-p.pdf',6,2)
n=length(p)
#q=p[sample(1:n,n,replace=TRUE,prob=w)]
par(mfrow=c(1,3),mai=c(0.6,0.5,0.1,0.2))
plot(density(rho1,cut=0),cex.lab=1.25,xlab=expression(rho),ylab='',main='',xlim=c(0,1),lty=2); 
lines(density(rho,cut=0)); 
x=seq(0.001,1,length.out=100); dr=dbeta(x,shape1=1,shape2=1/3,ncp=8)
lines(x,dr,col=1,lty=3)
par(mai=c(0.6,0.4,0.1,0.2))
plot(density(theta1,cut=0),cex.lab=1.25,xlab=expression(theta),ylab='',main='',xlim=c(0,1),lty=2); 
lines(density(theta,cut=0));
par(mai=c(0.6,0.4,0.1,0.2))
plot(density(p1,cut=0),cex.lab=1.25,xlab="P",ylab='',main='',lty=2,xlim=c(0,1)); 
lines(density(p,cut=0)); 
lines(density(p,weights=w,cut=0),col=1,lty=4,lwd=2)
dp=dbeta(x,1,9)
lines(x,dp,col=1,lty=3)
dev.off()

######################
setwd(local.laptop)
load("poWW9b.RData")
burn=10
pw=P[-c(1:burn),"p"]
pw<-pw[!is.na(pw)]

lines(density(pw,cut=0),col=3)
legend("topright",lty=c(1,1,1),col=c(1,2,3),legend=c("beta(1,9)","IS-U[0,1]","SIM-U[0,1]"))


##
#the same but this time reweighting a unif[0,1] prior to get beta(1,9)
setwd(local.laptop)

load("poWW11aRS1.RData")
burn=10
p=P[-c(1:burn),"p"]
p<-p[!is.na(p)]

#unnormalised importance weights
w=dbeta(p,1,9)
#normalised
w<-w/sum(w)

# not much diff in means
sum(p*w)
mean(p)

#now sample p's according to weights should give post using unif prior?
n=length(p)
#q=p[sample(1:n,n,replace=TRUE,prob=w)]
plot(density(p,cut=0),main="rewighting p~U[0,1] to p~Beta(1,9) using IS and simulation",xlab="p",ylim=c(0,7)); 
lines(density(p,weights=w,cut=0),col=4) #density does weights :)

setwd(onedrive)
load("poHB11b.RData")
burn=10
ph=P[-c(1:burn),"p"]
ph<-ph[!is.na(ph)]

lines(density(ph,cut=0),col=3)
legend("topright",lty=c(1,1,1),col=c(1,2,3),legend=c("U[0,1]","IS-Beta(1,9)","SIM-Beta(1,9)"))

