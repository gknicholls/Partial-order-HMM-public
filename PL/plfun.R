my.mcmc<-function(mcmc.pars,model,dat) {
  #generic mcmc function
  
  T=(mcmc.pars$samples-1)*mcmc.pars$sample.interval
  state=mcmc.pars$init.state

  X=vector('list',mcmc.pars$samples)
  X[[1]]=state #make sure same params always go into same columns of X

  post=npost=list()
  post$ll=model$dobs(state,dat)$ll
  post$lp=model$dprior(state,model$prior.param)


  for (i in 1:T) {

    prop=mcmc.pars$proposal(state,mcmc.pars$prop.pars)
    nstate=prop$nstate
    npost$ll=model$dobs(nstate,dat)$ll
    npost$lp=model$dprior(nstate,model$prior.param)

    logMHR=npost$ll+npost$lp-post$ll-post$lp+prop$qq

    if (log(runif(1))<logMHR) {
    	state=nstate
      post=npost
    }
    
    if (!(i%%mcmc.pars$sample.interval)) {
      X[[i/mcmc.pars$sample.interval+1]]=state
    }
  }
  return(X) 
}

my.acf<-function(X,lag,cols=5) {
  #generic acf plotter
  
  nv=dim(X)[2]
  par(mfrow=c(ceiling(nv/cols),cols),oma=c(1,1,1,1));                   #acf plots
  for (i in 1:nv) {
    par(mai=0.2*c(1,1,1,1)); 
    plot(acf(X[,i],lag.max=lag,plot=F),type='l',ann=F,xaxp=c(0,lag,2),yaxp=c(0,1,1)); 
    text(lag/2,0.8,colnames(X)[i])
  }
}

#####################################################
#functions for model with just lambda - not used in the end

my.rprior<-function(ppar) {
  #simulate prior density 
  sim.state=list()
  sim.state$lambda=rnorm(ppar$N,mean=ppar$lmu,sd=ppar$lsig) 
  return(sim.state)
}

my.dprior<-function(pstate,ppar) {
  #evaluate log prior density given state/parameter values
  pars=pstate$pars
  lp=sum(dnorm(pstate$lambda,mean=ppar$lmu,sd=ppar$lsig,log=TRUE))
  return(lp)
}

my.dobs<-function(state,dat) {
  #evaluate log-likelihood
  nc=length(dat)
  lla=vector('list',nc)
  for (i in 1:nc) {
    lam=state$lambda[dat[[i]]$o]
    lp=lpf(lam)    
    lla[[i]]$lp=lp
    lla[[i]]$ll=sum(lp)
  }
  ll=sum(sapply(lla, function (x) x$ll))
  return(list(ll=ll,lla=lla))
}

lpf<-function(lam) {
  np=length(lam)
  lp=numeric(np)
  for (j in 1:np) {
    lp[j]=lam[j]-log(sum(exp(lam[j:np])))
  }
  return(lp)
}

pn<-function(lam) {
  elam=exp(lam)
  return(elam/sum(elam))
}

my.robs<-function(state,dat) { #need dat as we will simulate with same players in each competition
  #simulate data given pars
  nc=length(dat)
  sdat=vector('list',nc)
  for (i in 1:nc) {
    lam=state$lambda[dat[[i]]$o]
    np=length(lam)
    o=ho=oe=numeric(np)
    for (j in 1:np) {
      oj=sample(1:np,1,prob=pn(lam)) 
      lam[oj]=-Inf #dont pick this one again
      o[j]=dat[[i]]$o[oj]; ho[j]=dat[[i]]$ho[oj]; oe[j]=dat[[i]]$e[oj]
    }
    sdat[[i]]$o=o; sdat[[i]]$e=oe; #sdat[[i]]$ho=ho; 
  }
  return(sdat)
}

my.q<-function(state,qpars) {
  nstate=list()
  N=length(state$lambda)
  if (runif(1)<0.5) {
    nstate$lambda=rnorm(N,mean=state$lambda,sd=qpars$sda)
    logQQ=0;
  } else {
    i=sample(1:N,1)
    nstate$lambda=state$lambda
    nstate$lambda[i]=rnorm(1,mean=state$lambda[i],sd=qpars$sd1)
    logQQ=0;
  }
  return(list(nstate=nstate,qq=logQQ))
}

#####################################################
#functions for the model with lambda and beta

my.rprior2<-function(ppar) {
  #simulate prior density 
  sim.state=list()
  sim.state$lambda=rnorm(ppar$N,mean=ppar$lmu,sd=ppar$lsig) #XXX
  sim.state$beta=rnorm(ppar$R,mean=ppar$bmu,sd=ppar$bsig) 
  return(sim.state)
}

my.dprior2<-function(pstate,ppar) {
  #evaluate log prior density given state/parameter values
    pars=pstate$pars
    lpl=sum(dnorm(pstate$lambda,mean=ppar$lmu,sd=ppar$lsig,log=TRUE))
    lpb=sum(dnorm(pstate$beta,mean=ppar$bmu,sd=ppar$bsig,log=TRUE))
    return(lpl+lpb)
}

my.dobs2<-function(state,dat) {
  #evaluate log-likelihood
  nc=length(dat)
  lla=vector('list',nc)
  for (i in 1:nc) {
    lam=state$lambda[dat[[i]]$o]
    bet=state$beta[dat[[i]]$e]
    lp=lpf2(lam,bet)    
    lla[[i]]$lp=lp
    lla[[i]]$ll=sum(lp)
  }
  ll=sum(sapply(lla, function (x) x$ll))
  return(list(ll=ll,lla=lla))
}

lpf2<-function(lam,bet) {
  np=length(lam) #should equal length bet
  lp=numeric(np)
  for (j in 1:np) {
    lp[j]=lam[j]+bet[j]-log(sum(exp(lam[j:np]+bet[j:np])))
  }
  return(lp)
}

pn2<-function(lam,bet) {
  elam=exp(lam+bet)
  return(elam/sum(elam))
}

my.robs2<-function(state,dat) { #need dat as we will simulate with same players in each competition
  #simulate data given pars
  nc=length(dat)
  sdat=vector('list',nc)
  for (i in 1:nc) {
    lam=state$lambda[dat[[i]]$o]
    bet=state$beta[dat[[i]]$e]
    np=length(lam)
    o=ho=oe=numeric(np)
    for (j in 1:np) {
      oj=sample(1:np,1,prob=pn2(lam,bet)) 
      lam[oj]=-Inf #dont pick this one again
      o[j]=dat[[i]]$o[oj]; ho[j]=dat[[i]]$ho[oj]; oe[j]=dat[[i]]$e[oj]
    }
    sdat[[i]]$o=o; sdat[[i]]$e=oe; #sdat[[i]]$ho=ho;  
  }
  return(sdat)
}

my.q2<-function(state,qpars) {
  nstate=list()
  if (runif(1)<0.5) {#update lambda
    nstate$beta=state$beta
    N=length(state$lambda)
    if (runif(1)<0.5) {
      nstate$lambda=rnorm(N,mean=state$lambda,sd=qpars$sda)
      logQQ=0;
    } else {
      i=sample(1:N,1)
      nstate$lambda=state$lambda
      nstate$lambda[i]=rnorm(1,mean=state$lambda[i],sd=qpars$sd1)
      logQQ=0;
    } 
  } else {
    nstate$lambda=state$lambda
    R=length(state$beta)
    if (runif(1)<0.5) {
      nstate$beta=rnorm(R,mean=state$beta,sd=qpars$sdab)
      logQQ=0;
    } else {
      i=sample(1:R,1)
      nstate$beta=state$beta
      nstate$beta[i]=rnorm(1,mean=state$beta[i],sd=qpars$sd1b)
      logQQ=0;
    }
  }
  return(list(nstate=nstate,qq=logQQ))
}


#####################################################
# functions for the model with just beta

my.rprior3<-function(ppar) {
  #simulate prior density 
  sim.state=list()
  sim.state$beta=rnorm(ppar$R,mean=ppar$bmu,sd=ppar$bsig) 
  return(sim.state)
}

my.dprior3<-function(pstate,ppar) {
  #evaluate log prior density given state/parameter values
  pars=pstate$pars
  lpb=sum(dnorm(pstate$beta,mean=ppar$bmu,sd=ppar$bsig,log=TRUE))
  return(lpb)
}

my.dobs3<-function(state,dat) {
  #evaluate log-likelihood
  nc=length(dat)
  lla=vector('list',nc)
  for (i in 1:nc) {
    bet=state$beta[dat[[i]]$e]
    lp=lpf3(bet)    
    lla[[i]]$lp=lp
    lla[[i]]$ll=sum(lp)
  }
  ll=sum(sapply(lla, function (x) x$ll))
  return(list(ll=ll,lla=lla))
}

lpf3<-function(bet) {
  np=length(bet) #should equal length bet
  lp=numeric(np)
  for (j in 1:np) {
    lp[j]=bet[j]-log(sum(exp(bet[j:np])))
  }
  return(lp)
}

pn3<-function(bet) {
  elam=exp(bet)
  return(elam/sum(elam))
}

my.robs3<-function(state,dat) { #need dat as we will simulate with same players in each competition
  #simulate data given pars
  nc=length(dat)
  sdat=vector('list',nc)
  for (i in 1:nc) {
    bet=state$beta[dat[[i]]$e]
    np=length(bet)
    o=ho=oe=numeric(np)
    for (j in 1:np) {
      oj=sample(1:np,1,prob=pn3(bet)) 
      bet[oj]=-Inf #dont pick this one again
      o[j]=dat[[i]]$o[oj]; ho[j]=dat[[i]]$ho[oj]; oe[j]=dat[[i]]$e[oj]
    }
    sdat[[i]]$o=o; sdat[[i]]$e=oe; #sdat[[i]]$ho=ho;  
  }
  return(sdat)
}

my.q3<-function(state,qpars) {
  nstate=list()
  R=length(state$beta)
  if (runif(1)<0.5) {
    nstate$beta=rnorm(R,mean=state$beta,sd=qpars$sdab)
    logQQ=0;
  } else {
    i=sample(1:R,1)
    nstate$beta=state$beta
    nstate$beta[i]=rnorm(1,mean=state$beta[i],sd=qpars$sd1b)
    logQQ=0;
  }
  return(list(nstate=nstate,qq=logQQ))
}
