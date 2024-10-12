
#
# DEPTH DISTRIBUTIONS - POSTERIOR 
#

################################################
#compare depth dbns for 16a (80-55, NF=18) and 1aRS6 (same, NF=9)
load("T-M2-down-sub-cn-os-dir/T-RS5-M2-down-sub-cn-os.RData")
d1=lapply(PO[!is.na(P[,1])], function(x) sapply(x,dagdepth))
d1m=matrix(unlist(d1),sum(!is.na(P[,1])),T,byrow=TRUE)
colnames(d1m)<-B-1+1:T
boxplot(d1m)
load("T-M-down-sub-cn-os-dir/T-RS6-M-down-sub-cn-os.RData")
burn16=100
i16=which(!is.na(P[,1]))[-(1:burn16)]
d16=lapply(PO[i16], function(x) sapply(x,dagdepth))
d16m=matrix(unlist(d16),length(i16),T,byrow=TRUE)
colnames(d16m)<-(years<-B-1+1:T)

boxplot(d16m,at=years-0.25,boxwex=0.2)
boxplot(d1m,add=TRUE,col=2,at=years+0.25,boxwex=0.2,xaxt='n')

plot(years,apply(d16m,2,mean),type='l',ylab='mean depth')
lines(years,apply(d1m,2,mean),col=2)
legend('topleft',legend=c('NF=18','NF=9'),lty=c(1,1),col=c(1,2))

#compare posterior depth dbn with truth on synth data
output=my.load("synth/poHB22aRS2.RData")
PO=output$PO; P=output$P
d1=lapply(PO[!is.na(P[,1])], function(x) sapply(x,dagdepth))
d1m=matrix(unlist(d1),sum(!is.na(P[,1])),T,byrow=TRUE)
colnames(d1m)<-B-1+1:T
boxplot(d1m,main='posterior depth for synthetic data',xlab='year',ylab='depth'); 
POs=output$Sstate$h
ds=sapply(POs,dagdepth)
lines(ds,col=2,lwd=4)
#lines(apply(d1m,2,function(x){which.max(table(x))}),col=4,lwd=4)
lines(apply(d1m,2,mean),col=4,lwd=4)
legend('topleft',legend=c('posterior depth dbn','true depth','posterior mean'),lwd=c(1,4,4),col=c(1,2,4))

#post depth db for real data
windows()
output=my.load("poHB17a.RData")
PO=output$PO; P=output$P
d1=lapply(PO[!is.na(P[,1])], function(x) sapply(x,dagdepth))
d1m=matrix(unlist(d1),sum(!is.na(P[,1])),T,byrow=TRUE)
colnames(d1m)<-B-1+1:T
boxplot(d1m,main='posterior depth for synthetic data',xlab='year',ylab='depth'); 
lines(apply(d1m,2,mean),col=4,lwd=4)
legend('topleft',legend=c('posterior depth dbn','posterior mean'),lwd=c(1,4),col=c(1,4))

