rm(list=ls())

pdf.file=NA #"./check/1kcheck.pdf"
if (Sys.getenv("COMPUTERNAME")=="NICHOLLS2389") {
  results.dir="C:/Users/nicholls.NICHOLLS2389/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM"
} else {
  results.dir="C:/Users/nicholls/OneDrive - Nexus365/Documents/GitHub/Partial-order-HMM"
}
setwd(results.dir)

source("outputfun.R")

#zoom in on a subset
select.string="T-"

drs=list.dirs(full.names=FALSE,recursive=FALSE);
select.drs=drs[grep(select.string,drs)]
loadfile.root=sub('-dir','',select.drs)

NLF=length(loadfile.root)
loadfile=vector("character",NLF)
for (i in 1:NLF) {
  files=paste(select.drs[i],'/',dir(select.drs[i]),sep="")
  most.recent=which.max(sapply(files,function(x){file.info(x)$mtime}))
  #most recent written file
  loadfile[i]<-files[most.recent]
}
wd=max(sapply(loadfile.root,nchar))
cl=c(1,1,2,2,3,4,5,5,6,6,7,8)
lt=rep(1,NLF)

#palette("default")
SS=N.sample.done=J.done=step.count=P=note=output=Sstate=vector('list',NLF)

for (k in 1:NLF) {
  
  output[[k]]=my.load(loadfile[[k]])
  
  note[[k]]=output[[k]]$note; 
  
  #PO=output$PO;
  #Uout=output$Uout;
  P[[k]]=output[[k]]$P;
  step.count[[k]]=output[[k]]$step.count;
  SS[[k]]=output[[k]]$SS;
  
  J.done[[k]]=step.count[[k]] #last complete step	
  (N.sample.done[[k]]=J.done[[k]]/SS[[k]]+1) #starts at 0
  #PO=PO[1:N.sample.done[[k]]]
  #Uout=Uout[1:N.sample.done[[k]]]
  
  P[[k]]=P[[k]][1:N.sample.done[[k]],]

  if (exists("DOSYNTH",where=output[[k]]$note) && output[[k]]$note$DOSYNTH) Sstate[[k]]=output[[k]]$Sstate
}
burn=ceiling(0.1*min(unlist(N.sample.done))*rep(1,NLF))

#show summary info about run
cat(sprintf("%s\n","Run progress"))
time.now=Sys.time()
for (k in 1:NLF) {  
  if (k==1) cat(sprintf("%28s | %8s | %10s | %10s | %10s \n","run name","samples","days run","hours ago","secs/sweep"))
  sps=round(sum(output[[k]]$st)/step.count[[k]])
  last.write=file.info(loadfile[k])$mtime
  age=round(difftime(time.now,last.write,units="hours"))
  created=file.info(loadfile[k])$ctime
  runtime=round(difftime(time.now,created,units="days"))
  cat(sprintf("%28s | %8d | %10d | %10d | %10d \n",loadfile.root[k],N.sample.done[[k]],runtime,age,sps))
}



############################################################

if (!is.na(pdf.file)) pdf(file=pdf.file)

############################################################
#plot obvious stuff

#if (is.na(pdf.file)) windows(10,10)
#print(colnames(P)) #to see what we have available to plot
par(mfcol=c(3,2))

aa=min(burn); bb=max(unlist(N.sample.done))

mx.oll=max(sapply(P,function(x) {max(x[-c(1:aa),"oll"])}))
mn.oll=min(sapply(P,function(x) {min(x[-c(1:aa),"oll"])}))
plot(burn[1]:N.sample.done[[1]],P[[1]][burn[1]:N.sample.done[[1]],"oll"],col=cl[1],lty=lt[1],type='l',xlim=c(aa,bb),ylim=c(mn.oll,mx.oll)); 
points(N.sample.done[[1]],P[[1]][N.sample.done[[1]],"oll"],pch=16,col=cl[1])
if (exists("DOSYNTH",where=note[[1]]) && note[[1]]$DOSYNTH) abline(h=Sstate[[1]]$oll.tot,col=2)
if (NLF>1) for (k in 2:NLF) {
  lines(burn[k]:N.sample.done[[k]],P[[k]][burn[k]:N.sample.done[[k]],"oll"],col=cl[k],lty=lt[k]); 
  points(N.sample.done[[k]],P[[k]][N.sample.done[[k]],"oll"],pch=1+k,col=cl[k])
  if (exists("DOSYNTH",where=note[[k]]) && note[[k]]$DOSYNTH) abline(h=Sstate[[k]]$oll.tot,col=2)
}

mx.lpU=max(sapply(P,function(x) {max(x[-c(1:aa),"lpU"])}))
mn.lpU=min(sapply(P,function(x) {min(x[-c(1:aa),"lpU"])}))
plot(burn[1]:N.sample.done[[1]],P[[1]][burn[1]:N.sample.done[[1]],"lpU"],col=cl[1],lty=lt[1],type='l',xlim=c(aa,bb),ylim=c(mn.lpU,mx.lpU)); points(N.sample.done[[1]],P[[1]][N.sample.done[[1]],"lpU"],pch=16,col=1)
if (exists("DOSYNTH",where=note[[1]]) && note[[1]]$DOSYNTH) abline(h=(Sstate[[1]]$oup)$lpU.tot,col=2)
if (NLF>1) for (k in 2:NLF) {
  lines(burn[k]:N.sample.done[[k]],P[[k]][burn[k]:N.sample.done[[k]],"lpU"],col=cl[k],lty=lt[k]); 
  points(N.sample.done[[k]],P[[k]][N.sample.done[[k]],"lpU"],pch=1+k,col=cl[k])
  if (exists("DOSYNTH",where=note[[k]]) && note[[k]]$DOSYNTH) abline(h=(Sstate[[k]]$oup)$lpU.tot,col=2)
}

mx.obp=max(sapply(P,function(x) {max(x[-c(1:aa),"obp"])}))
mn.obp=min(sapply(P,function(x) {min(x[-c(1:aa),"obp"])}))
plot(burn[1]:N.sample.done[[1]],P[[1]][burn[1]:N.sample.done[[1]],"obp"],col=cl[1],lty=lt[1],type='l',xlim=c(aa,bb),ylim=c(mn.obp,mx.obp)); points(N.sample.done[[1]],P[[1]][N.sample.done[[1]],"obp"],pch=16,col=1)
if (exists("DOSYNTH",where=note[[1]]) && note[[1]]$DOSYNTH) abline(h=Sstate[[1]]$obp,col=2)
if (NLF>1) for (k in 2:NLF) {
  lines(burn[k]:N.sample.done[[k]],P[[k]][burn[k]:N.sample.done[[k]],"obp"],col=cl[k],lty=lt[k]); 
  points(N.sample.done[[k]],P[[k]][N.sample.done[[k]],"obp"],pch=1+k,col=cl[k])
  if (exists("DOSYNTH",where=note[[k]]) && note[[k]]$DOSYNTH) abline(h=Sstate[[k]]$obp,col=2)
}

plot(density(P[[1]][burn[k]:N.sample.done[[1]],"oll"]),col=cl[1],ann=FALSE);
if (exists("DOSYNTH",where=note[[1]]) && note[[1]]$DOSYNTH) abline(v=Sstate[[1]]$oll.tot,col=2)
if (NLF>1) for (k in 2:NLF) { 
  lines(density(P[[k]][burn[k]:N.sample.done[[k]],"oll"]),col=cl[k],lty=lt[k],ann=FALSE) 
  if (exists("DOSYNTH",where=note[[k]]) && note[[k]]$DOSYNTH) abline(v=Sstate[[k]]$oll.tot,col=2)
  }
plot(density(P[[1]][burn[k]:N.sample.done[[1]],"lpU"]),col=cl[1],ann=FALSE);
if (exists("DOSYNTH",where=note[[1]]) && note[[1]]$DOSYNTH) abline(v=(Sstate[[1]]$oup)$lpU.tot,col=2)
if (NLF>1) for (k in 2:NLF) { 
  lines(density(P[[k]][burn[k]:N.sample.done[[k]],"lpU"]),col=cl[k],lty=lt[k],ann=FALSE) 
  if (exists("DOSYNTH",where=note[[k]]) && note[[k]]$DOSYNTH) abline(v=(Sstate[[k]]$oup)$lpU.tot,col=2)
  }
plot(density(P[[1]][burn[k]:N.sample.done[[1]],"obp"]),col=cl[1],lty=lt[1],ann=FALSE);
if (exists("DOSYNTH",where=note[[1]]) && note[[1]]$DOSYNTH) abline(v=Sstate[[1]]$obp,col=2)
if (NLF>1) for (k in 2:NLF) { 
  lines(density(P[[k]][burn[k]:N.sample.done[[k]],"obp"]),col=cl[k],lty=lt[k],ann=FALSE)
  if (exists("DOSYNTH",where=note[[k]]) && note[[k]]$DOSYNTH) abline(v=Sstate[[k]]$obp,col=2)
  }

#if (is.na(pdf.file)) windows(); 
par(mfcol=c(3,2))
plot(burn[1]:N.sample.done[[1]],P[[1]][burn[1]:N.sample.done[[1]],"rho"],col=cl[1],lty=lt[1],type='l',xlim=c(aa,bb),ylim=c(0.8,1)); points(N.sample.done[[1]],P[[1]][N.sample.done[[1]],"rho"],pch=16,col=1)
if (exists("DOSYNTH",where=note[[1]]) && note[[1]]$DOSYNTH) abline(h=Sstate[[1]]$rho,col=2)
if (NLF>1) for (k in 2:NLF) {
  lines(burn[k]:N.sample.done[[k]],P[[k]][burn[k]:N.sample.done[[k]],"rho"],col=cl[k],lty=lt[k]); 
  points(N.sample.done[[k]],P[[k]][N.sample.done[[k]],"rho"],pch=1+k,col=cl[k])
  if (exists("DOSYNTH",where=note[[k]]) && note[[k]]$DOSYNTH) abline(h=Sstate[[k]]$rho,col=2)
}
plot(burn[1]:N.sample.done[[1]],P[[1]][burn[1]:N.sample.done[[1]],"theta"],col=cl[1],lty=lt[1],type='l',xlim=c(aa,bb),ylim=c(0.8,1)); points(N.sample.done[[1]],P[[1]][N.sample.done[[1]],"theta"],pch=16,col=1)
if (exists("DOSYNTH",where=note[[1]]) && note[[1]]$DOSYNTH) abline(h=Sstate[[1]]$theta,col=2)
if (NLF>1) for (k in 2:NLF) {
  lines(burn[k]:N.sample.done[[k]],P[[k]][burn[k]:N.sample.done[[k]],"theta"],col=cl[k],lty=lt[k]); 
  points(N.sample.done[[k]],P[[k]][N.sample.done[[k]],"theta"],pch=1+k,col=cl[k])
  if (exists("DOSYNTH",where=note[[k]]) && note[[k]]$DOSYNTH) abline(h=Sstate[[k]]$theta,col=2)
}
if (exists("DOP",where=note[[1]])) {
  plot(burn[1]:N.sample.done[[1]],P[[1]][burn[1]:N.sample.done[[1]],"p"],col=cl[1],lty=lt[1],type='l',xlim=c(aa,bb),ylim=c(0,0.3)); points(N.sample.done[[1]],P[[1]][N.sample.done[[1]],"p"],pch=16,col=1)
  if (exists("DOSYNTH",where=note[[1]]) && note[[1]]$DOSYNTH) abline(h=Sstate[[1]]$p,col=2)
  if (NLF>1) for (k in 2:NLF) {
    lines(burn[k]:N.sample.done[[k]],P[[k]][burn[k]:N.sample.done[[k]],"p"],col=cl[k],lty=lt[k]); 
    points(N.sample.done[[k]],P[[k]][N.sample.done[[k]],"p"],pch=1+k,col=cl[k])
    if (exists("DOSYNTH",where=note[[k]]) && note[[k]]$DOSYNTH) abline(h=Sstate[[k]]$p,col=2)
  }
}

plot(density(P[[1]][burn[k]:N.sample.done[[1]],"rho"],from=0.7,to=1),col=cl[1],lty=lt[1],ann=FALSE);
if (exists("DOSYNTH",where=note[[1]]) && note[[1]]$DOSYNTH) abline(v=Sstate[[1]]$rho,col=2)
if (NLF>1) for (k in 2:NLF) { 
  lines(density(P[[k]][burn[k]:N.sample.done[[k]],"rho"],from=0,to=1),
                         col=cl[k],lty=lt[k],ann=FALSE) 
  if (exists("DOSYNTH",where=note[[k]]) && note[[k]]$DOSYNTH) abline(v=Sstate[[k]]$rho,col=2)
  }
plot(density(P[[1]][burn[k]:N.sample.done[[1]],"theta"],from=0.7,to=1),col=cl[1],lty=lt[1],ann=FALSE);
if (exists("DOSYNTH",where=note[[1]]) && note[[1]]$DOSYNTH) abline(v=Sstate[[1]]$theta,col=2)
if (NLF>1) for (k in 2:NLF) { 
  lines(density(P[[k]][burn[k]:N.sample.done[[k]],"theta"],from=0,to=1),
                         col=cl[k],lty=lt[k],ann=FALSE) 
  if (exists("DOSYNTH",where=note[[k]]) && note[[k]]$DOSYNTH) abline(v=Sstate[[k]]$theta,col=2)
  }
if (exists("DOP",where=note[[1]])) {
  plot(density(P[[1]][burn[k]:N.sample.done[[1]],"p"],from=0,to=0.4),col=cl[1],lty=lt[1],ann=FALSE); 
  if (exists("DOSYNTH",where=note[[1]]) && note[[1]]$DOSYNTH) abline(v=Sstate[[1]]$p,col=2)
  if (NLF>1) for (k in 2:NLF) { 
    lines(density(P[[k]][burn[k]:N.sample.done[[k]],"p"],from=0,to=1),
                           col=cl[k],lty=lt[k],ann=FALSE) 
    if (exists("DOSYNTH",where=note[[k]]) && note[[k]]$DOSYNTH) abline(v=Sstate[[k]]$p,col=2)
  }
  legend('bottomright',loadfile.root,col=cl,lty=lt,y.intersp=1.2,lwd=2,cex=0.3)
}

#beta analysis
ucb=grep('-uc-',loadfile)
NLF.uc=length(ucb)

cat(sprintf("\n\n%s\n","Decreasing effect sequences in unconstrained analyses"))
if (NLF.uc>0) {
  
  k1=ucb[1]
  SHOW_BETA=!output[[k1]]$note$constrainbeta
  
  if (SHOW_BETA) {
    centred.beta=uncentred.beta=centred.Sbeta=vector('list',NLF.uc)
    
    for (k in 1:NLF.uc) {
      kk=ucb[k]
      beta.ind=grep("beta",colnames(P[[kk]]))  
      DB=output[[kk]]$DB
      uncentred.beta[[k]]=P[[kk]][,beta.ind]   	#need to compute Z below
      M=matrix(-1/DB,DB,DB)+diag(DB)
      centred.beta[[k]]=uncentred.beta[[k]]%*%M #issues if DB varies across runs
      if (exists("DOSYNTH",where=note[[kk]]) && note[[kk]]$DOSYNTH) centred.Sbeta[[k]]=Sstate[[kk]]$beta%*%M
    }
    #if (is.na(pdf.file)) windows(); 
    par(mfcol=c(1,1)) 
    boxplot(centred.beta[[1]][burn[k1]:N.sample.done[[k1]],],col=1) 
    for (k in 1:NLF.uc) {
      kk=ucb[k]
      boxplot(centred.beta[[k]][burn[kk]:N.sample.done[[kk]],],col=k,add=TRUE)
      #find length of decreasing sequence from beta1
      dec.seq=apply(P[[kk]][burn[kk]:N.sample.done[[kk]],beta.ind],1,function(x) {y=diff(x); z=min(which(y>0)); return(z)})
      seen=1:max(dec.seq)
      counts=sapply(seen,function(x) sum(x<=dec.seq)) # number of times each length or longer appears (cummulative)
      names(counts)<-seen
      cat(sprintf("%20s: %s\n",loadfile.root[kk],paste(sprintf("(%s, %d)",names(counts),counts),collapse = ' ')))
      #(factorial(seen)*counts/length(burn[[k]]:N.sample.done)) #Bayes factors for each length seen
    }
    for (k in 1:NLF.uc) {
      kk=ucb[k]
      if (exists("DOSYNTH",where=note[[kk]]) && note[[kk]]$DOSYNTH) 
        points(1:DB,centred.Sbeta[[kk]],pch=16,col=k+NLF.uc)
    }
  } else {
    warning('file name convention is that -uc- indicates unconstrained analysis')
  }
}

if (!is.na(pdf.file)) dev.off()

if (FALSE) {
  rm(output)
  gc()
}