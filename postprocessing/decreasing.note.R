#are the betas decreasing? Calculate Bayes factors for sequence lengths

source(file="outputfun.R")
burn=100 #set burnin

#output=my.load("../working5/poHB1aRS6.RData")
output=my.load("T-M2-down-sub-uc-ds-dir/T-RS5-M2-down-sub-uc-ds.RData")
P=output$P
step.count=output$step.count;
SS=output$SS;
J.done=step.count #last complete step	
(N.sample.done=J.done/SS+1)
P=P[burn:N.sample.done,]

# output=my.load("../working5/poHB1bRS6.RData")
# P1=output$P
# step.count1=output$step.count;
# SS1=output$SS;
# J.done1=step.count1 #last complete step	
# (N.sample.done1=J.done1/SS1+1)
# P1=P1[burn:N.sample.done1,]
# 
# P=rbind(P,P1)
(N.sample.done=dim(P)[1])

beta.ind=grep("beta",colnames(P))

#find length of decreasing sequence from beta1
dec.seq=apply(P[1:N.sample.done,beta.ind],1,function(x) {y=diff(x); z=min(which(y>0)); return(z)})
plot(dec.seq)
seen=1:max(dec.seq)
counts=sapply(seen,function(x) sum(x<=dec.seq)) # number of times each length or longer appears (cummulative)
names(counts)<-seen
#counts=table(dec.seq) # number of times each length appears
#seen=as.numeric(names(counts)) #which lengths were seen
(factorial(seen)*counts/N.sample.done) #Bayes factors for each length seen
p=counts/N.sample.done
(factorial(seen)*sqrt(p*(1-p)/N.sample.done))

#sanity check
if (FALSE) {
  g<-function(x) {y=diff(x); z=min(which(y>0)); return(z)}
  tst=P[burn+which.max(dec.seq)-1,beta.ind]
  plot(tst)
  g(tst)
}

#how many are decreasing as opposed to decreasing sequence
#n.dec.seq=apply(P[burn:N.sample.done,beta.ind],1,function(x) {y=diff(x); z=sum(y<0); return(z)})
#plot(n.dec.seq)
#gg<-function(x) {y=diff(x); z=sum(y<0); return(z)}
#tst=P[burn+which.max(n.dec.seq)-1,beta.ind]
#gg(tst)
#plot(tst)

