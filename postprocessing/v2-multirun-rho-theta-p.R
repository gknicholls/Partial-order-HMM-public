
################################################
#multirun rho-theta-p
output.dir="C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM/"
setwd(output.dir)

#out.file="T-M-up-sub-cn-os-dir/T-RS6-M-up-sub-cn-os.RData"

output=my.load("T-M2-down-sub-uc-ds-dir/T-RS5-M2-down-sub-uc-ds.RData")
P=output$P
#load("poHB9aRS1.RData")
burn=100
p=P[-c(1:burn),"p"]
p<-p[!is.na(p)]
theta=P[-c(1:burn),"theta"]
theta<-theta[!is.na(theta)]
rho=P[-c(1:burn),"rho"]
rho<-rho[!is.na(rho)]

output=my.load("T-M-up-sub-uc-os-dir/T-RS6-M-up-sub-uc-os.RData")
P1=output$P
#load("poHB9aRS1.RData")
p1=P1[-c(1:burn),"p"]
p1<-p1[!is.na(p1)]
theta1=P1[-c(1:burn),"theta"]
theta1<-theta1[!is.na(theta1)]
rho1=P1[-c(1:burn),"rho"]
rho1<-rho1[!is.na(rho1)]

output=my.load("T-M-down-sub-uc-os-dir/T-RS6-M-down-sub-uc-os.RData")
P2=output$P
#load("poHB9aRS1.RData")
p2=P2[-c(1:burn),"p"]
p2<-p2[!is.na(p2)]
theta2=P2[-c(1:burn),"theta"]
theta2<-theta2[!is.na(theta2)]
rho2=P2[-c(1:burn),"rho"]
rho2<-rho2[!is.na(rho2)]

#output=my.load("H-MK2-down-sub-cn-os-dir/H-MK2-down-sub-cn-os.RData")
output=my.load("T-MK2-up-sub-uc-os-dir/T-MK2-up-sub-uc-os.RData")
P3=output$P
#load("poHB9aRS1.RData")
p3=P3[-c(1:burn),"p"]
p3<-p3[!is.na(p3)]
theta3=P3[-c(1:burn),"theta"]
theta3<-theta3[!is.na(theta3)]
rho3=P3[-c(1:burn),"rho"]
rho3<-rho3[!is.na(rho3)]

pdf('paper/v3-poHB1-14-16-rho-theta-p.pdf',6,2)
n=length(p)
#q=p[sample(1:n,n,replace=TRUE,prob=w)]
par(mfrow=c(1,3),mai=c(0.6,0.5,0.1,0.2))
plot(density(rho2,cut=0),cex.lab=1.25,xlab=expression(rho),ylab='',main='',xlim=c(0,1),
     col=1,lty=4,lwd=1); 
lines(density(rho,cut=0),lty=1); 
lines(density(rho1,cut=0),lty=2); 
lines(density(rho3,cut=0,bw=0.1),lty=3); 
x=seq(0.001,1,length.out=100); dr=dbeta(x,1,0.333333,8)
lines(x,dr,col=1,lty=5)
legend('topleft',lty=c(4,1,2,3,5),legend=c("11-down","22-down","11-up","2-up","prior"))

par(mai=c(0.6,0.4,0.1,0.2))
plot(density(theta2,cut=0),cex.lab=1.25,xlab=expression(theta),ylab='',main='',xlim=c(0,1),
     lty=4,lwd=1); 
lines(density(theta,cut=0),col=1,lty=1);
lines(density(theta1,cut=0),lty=2);
lines(density(theta3,cut=0),lty=3);

par(mai=c(0.6,0.4,0.1,0.2))
plot(density(p2,cut=0),cex.lab=1.25,xlab="P",ylab='',main='',col=1,xlim=c(0,1),lty=4,lwd=1); 
lines(density(p,cut=0),lty=1); 
lines(density(p1,cut=0),lty=2); 
lines(density(p3,cut=0),lty=3); 
dp=dbeta(x,1,9)
lines(x,dp,col=1,lty=5)
dev.off()

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

