
  get.t.blocks<-function(n.blocks,T,splits=NA) {
    #for log.lkd it would be OK to go down to one time per block but not the U-update
    #might use different blocking
    if (2*n.blocks>T) stop('blocking for parallel - too many blocks not enough times - in get.t.blocks')
    if (!is.na(splits[1]) && length(splits)!=(n.blocks-1)) stop('blocking for parallel - wrong number of split points input')
    if (any(diff(splits)==1)) stop('blocking for parallel - splits in adjacent years')
    t.blocks=vector('list',n.blocks+1)
    if (is.na(splits[1])) {splits=floor(seq(1,T,length.out=n.blocks+1))} else {splits=c(1,splits,T)}
    t.blocks[[1]]=1:(splits[2]-1)
    if (n.blocks>2) {
      for (k in 2:(n.blocks-1)) {
        t.blocks[[k]]=(splits[k]+1):(splits[k+1]-1)
      }
    }
    t.blocks[[n.blocks]]=(splits[n.blocks]+1):T
    t.blocks[[n.blocks+1]]=splits[2:n.blocks]
    return(list(n.blocks=n.blocks,t.blocks=t.blocks))
  }

  doU<-function(update.times,state,active,rank,B,E,T,NF,NB,DB,cla,cil,oll,oll.tot,oup,note) {
    #update all bishops at once in each time slice

    accept=matrix(0,1,DB+7) #YUCK - this +7 is the number of moves - beta (ie DB) and then theta,rho,U,tau,p we have in the MCMC 
    propose=matrix(0,1,DB+7)
    
    Sig=matrix(state$rho,NF,NF); diag(Sig)=1;
  
    #conditional mean factor and var for U[b,,t]|U[b,,t-1] for t the first active year of b
    mean.fac.start=state$theta
    var.start=(1-state$theta^2)*Sig

    #conditional mean factor and var for U[b,,t]|U[b,,t+/-1] for t between first and last active years
    mean.fac.mid=state$theta/(1+state$theta^2)
    var.mid=Sig*(1-state$theta^2)/(1+state$theta^2)

    #conditional mean factor and var for U[b,,t]|U[b,,t-1] for t the last active year of b
    mean.fac.end=state$theta
    var.end=(1-state$theta^2)*Sig
    
    for (sweeps in 1:note$Usweeps[1]) {
    for (t in update.times) {

      bishops.active=which(active[,t]) #should match as.numeric(colnames(state$h[[t]]))
      n.bishops.active=length(bishops.active) #should match the dim of state$h[[t]]

      U=state$U #save typing
      new.U=U
      new.Z=state$Z
      new.h=state$h
      propose[DB+3]=propose[DB+3]+1

      #some years (1136) are very sticky and we cant update all bishops at once
      #n.bishops.update=sample(1:n.bishops.active,1) 
      #bishops.active.update=sort(sample(bishops.active,n.bishops.update,replace=FALSE))
      #for (b in bishops.active.update) { 
      #Above issue now handled using doU2() update - one b at a time

      for (b in bishops.active) { 
        years.active=which(active[b,]) #a bit unnecessary to recompute this all the time - could go in cil?
        n.years.active=length(years.active)

        #check only one of these cases ever gets hit
        count=0
        if (n.years.active==1) {
          new.U[b,,t]=mvrnorm(1,rep(0,NF),Sig) #prior conditional t=first active year
          count=count+1
        }
        if (t==years.active[1] && n.years.active>1) {
          new.U[b,,t]=mvrnorm(1,mean.fac.start*U[b,,t+1],var.start) #prior conditional t=first active year
          count=count+1
        }
        if (t==years.active[n.years.active] && n.years.active>1) {
          new.U[b,,t]=mvrnorm(1,mean.fac.end*U[b,,t-1],var.end) #prior conditional t=last active year
          count=count+1
        }
        if (t>years.active[1] && t<years.active[n.years.active]) {
          new.U[b,,t]=mvrnorm(1,mean.fac.mid*(U[b,,t-1]+U[b,,t+1]),var.mid)  #prior for t any other year
          count=count+1
        }
        if (count!=1) stop('years active selection problem in doU')

      } #b
      new.Z[,,t]=ComputeZ(years=t,bishops=1:NB,new.U,rank,state$beta)[,,t] #just do this [,,t] slice
      new.h[t]=ZtoPO(new.Z,active,B,T,display.Z=FALSE,PO=new.h,years=t)[t] #just update the PO for this year t

      new.ll=oll
      new.ll.tot=oll.tot
      MHR=0

      if (length(state$y2l[[t]])>0 && note$model!='prior') { 
        if (state$p>0 || noconflict2(new.h,state$y2l,cla,years=t)$outcome) {
          #TODO - only update lists actually hit by this change - use hit lists - see mcmc-old-V2.R
          new.ll[[t]]=loglkd2(r=state$p,new.h[[t]],cla[state$y2l[[t]]],note$model,q=state$q)
          new.ll.tot=sum(unlist(new.ll)) #TODO - only need lists in this year that were hit (but then careful with update)
          MHR=new.ll.tot-oll.tot #no contribution from prior as we proposed using U[b,,t]|U[b,,t+/-1] etc ie conditional of prior so prior bits cancel
        } else {
          MHR=(-Inf) #illegal PO conflicts lists
        }
      }

      if (log(runif(1))<MHR) {
        state$U=new.U
        state$Z=new.Z
        state$h=new.h
        oll=new.ll #no need to update oup as it isnt needed when we propose using prior conditional for U
        oll.tot=new.ll.tot
        accept[DB+3]=accept[DB+3]+1
      }

    } #t
    update.times<-rev(update.times)
    } #sweeps

    return(list(state=state,accept=accept,propose=propose))

  } #doU()

  #######################################################################################################
                 
  doU2<-function(update.times,state,active,rank,B,E,T,NF,NB,DB,cla,cil,oll,oll.tot,oup,note) {
    #update bishops one at a time in each time slice

    accept=matrix(0,1,DB+7) #YUCK - this +5 is the number of moves - beta (ie DB) and then theta,rho,U,tau,p we have in the MCMC 
    propose=matrix(0,1,DB+7)
    
    Sig=matrix(state$rho,NF,NF); diag(Sig)=1;
  
    #conditional mean factor and var for U[b,,t]|U[b,,t-1] for t the first active year of b
    mean.fac.start=state$theta
    var.start=(1-state$theta^2)*Sig

    #conditional mean factor and var for U[b,,t]|U[b,,t+/-1] for t between first and last active years
    mean.fac.mid=state$theta/(1+state$theta^2)
    var.mid=Sig*(1-state$theta^2)/(1+state$theta^2)

    #conditional mean factor and var for U[b,,t]|U[b,,t-1] for t the last active year of b
    mean.fac.end=state$theta
    var.end=(1-state$theta^2)*Sig

    for (sweeps in 1:note$Usweeps[2]) {
    for (t in update.times) {

      bishops.active=which(active[,t]) #should match as.numeric(colnames(state$h[[t]]))
      #n.bishops.active=length(bishops.active) #should match the dim of state$h[[t]]
 
      for (b in bishops.active) { 
        propose[DB+3]=propose[DB+3]+1

        years.active=which(active[b,]) #a bit unnecessary to recompute this all the time - could go in cil?
        n.years.active=length(years.active)

        U=state$U #save typing
        new.U=U
        new.Z=state$Z
        new.h=state$h
 
        #check only one of these case ever gets hit
        count=0
        if (n.years.active==1) {
          new.U[b,,t]=mvrnorm(1,rep(0,NF),Sig) #prior conditional t=first active year
          count=count+1
          #qq=dmvnorm(U[b,,t],mean.fac.start*U[b,,t+1],var.start,log=TRUE)-dmvnorm(new.U[b,,t],mean.fac.start*U[b,,t+1],var.start,log=TRUE)
        }
        if (t==years.active[1] && n.years.active>1) {
          new.U[b,,t]=mvrnorm(1,mean.fac.start*U[b,,t+1],var.start) #prior conditional t=first active year
          count=count+1
          #qq=dmvnorm(U[b,,t],mean.fac.start*U[b,,t+1],var.start,log=TRUE)-dmvnorm(new.U[b,,t],mean.fac.start*U[b,,t+1],var.start,log=TRUE)
        }
        if (t==years.active[n.years.active] && n.years.active>1) {
          new.U[b,,t]=mvrnorm(1,mean.fac.end*U[b,,t-1],var.end) #prior conditional t=last active year
          count=count+1
          #qq=dmvnorm(U[b,,t],mean.fac.end*U[b,,t-1],var.end,log=TRUE)-dmvnorm(new.U[b,,t],mean.fac.end*U[b,,t-1],var.end,log=TRUE)
        }
        if (t>years.active[1] && t<years.active[n.years.active]) {
          new.U[b,,t]=mvrnorm(1,mean.fac.mid*(U[b,,t-1]+U[b,,t+1]),var.mid)  #prior for t any other year
          count=count+1
          #qq=dmvnorm(U[b,,t],mean.fac.mid*(U[b,,t-1]+U[b,,t+1]),var.mid,log=TRUE)-dmvnorm(new.U[b,,t],mean.fac.mid*(U[b,,t-1]+U[b,,t+1]),var.mid,log=TRUE)
        }
        if (count!=1) stop('years active selection problem in doU2')

        new.Z[b,,t]=ComputeZ(years=t,bishops=b,new.U,rank,state$beta)[b,,t] #just do this [b,,t] row
        new.h[[t]]=ZtoPO(new.Z,active,B,T,display.Z=FALSE,PO=new.h,years=t)[[t]] #just update the PO for this year t
      
        br=match(b,as.numeric(rownames(state$h[[t]]))) #which row/column in the PO does this bishop sit in in this year t
        change.h=(any(new.h[[t]][br,]!=state$h[[t]][br,]) || any(new.h[[t]][,br]!=state$h[[t]][,br])) #did we actually change the PO when we changed U? 
        hit.lists<-intersect(state$y2l[[t]],cil[[b]]$ln)

        if (!change.h || length(hit.lists)==0 || note$model=='prior') {
          MHR=0
          new.ll=oll
          new.ll.tot=oll.tot
        } else {
          if (state$p>0 || noconflict2(new.h,state$y2l,cla,years=t)$outcome) {

          #TODO - hit.lists could go further and require lists to contain other bishops hit by change
          #TODO - only update lists actually hit by this change - use hit lists
          
          new.ll=oll #TODO 5/1/20 - CHANGED [[t]] to [t] below - old version was bad if RHS is NULL - was only OK as protected by length(hit.lists)>0
          new.ll[[t]]=loglkd2(r=state$p,new.h[[t]],cla[state$y2l[[t]]],note$model,q=state$q)
          new.ll.tot=sum(unlist(new.ll)) #TODO - only need lists in this year that were hit (but then careful with update)
          MHR=new.ll.tot-oll.tot #no contribution from prior as we proposed using U[b,,t]|U[b,,t+/-1] etc ie conditional of prior so prior bits cancel

          } else {
            MHR=(-Inf) #illegal PO conflicts lists
          }
        } 
      
        if (log(runif(1))<MHR) {
          state$U=new.U
          state$Z=new.Z
          state$h=new.h
          oll=new.ll  #no need to update oup as it isnt needed when we propose using prior conditional for U
          oll.tot=new.ll.tot
          accept[DB+3]=accept[DB+3]+1
        }
      } # b  
    } #t
    update.times<-rev(update.times)
    } #sweeps

    return(list(state=state,accept=accept,propose=propose))

  } #doU2()

