makesynthdata<-function(note,cla,cil,B,E,T,dBprior) {
  
  #control T,B,E in data setup
  #control bishops in data setup
  
  ########################################
  #synthetic data - length controled by repping cla note$srep times
  #this messes up the list id's in scla and scil so fix these 
  
  if (note$SYNTHTYPE=='oldsynth') {
    #TODO - this is not checked yet
    #stop('oldsynth is not checked yet')
    
    #load synth data generated in a previous run - this allows simulation and analysis with different priors (and in future obs models)
    output=my.load(note$OLDSYNTHINPUTFILE)
    scla=output$scla
    scil=output$scil
    Sstate=output$Sstate
    
    sNL=length(scla)
    sNB=length(scil)
    
    srank=ComputeRanks(scil,B,E)
    sDB=max(srank,na.rm=TRUE) 
    sactive=!is.na(srank) 
    sNF=note$sNF #actually no reason we have to use the same NF when we are loading data
    sMA=max(apply(sactive,2,sum))
    
    doi=output$doi
    if (all(doi!=note$doi)) stop('makesynthdata: loaded old synth data, doi doesnt match note$doi')
    
    if (B!=output$B | E!=output$E | note$model!=output$note$model) { #TODO should remove this test - at least last condition - allow sim/analysis models diff
      stop('setup in run in TRUEINPUTFILE doesnt match synth setup in note - makesynthdata.R')
    }
    
    rm(output)
  } else {
    
    #doi defined in makedata() is same at this so should be same doi as in calling envir
    doi=note$doi
    
    scla=vector('list',0)
    cla.id=sapply(cla,function(x) x$id)
    mci=max(cla.id)
    for (k in 1:note$srep) {
      new.cla=cla
      for (j in 1:length(cla)) new.cla[[j]]$id=mci+1+k*cla[[j]]$id #create a new id by mapping old 
      scla=c(scla,new.cla)
    }
    scil=cil
    for (i in 1:length(cil)) {
      scil[[i]]$lid=mci+1+rep(cil[[i]]$lid,note$srep)*rep(1:note$srep,rep(length(cil[[i]]$lid),note$srep)) #update scla id's in scil
      scil[[i]]$tl=rep(cil[[i]]$tl,note$srep)
      scil[[i]]$tu=rep(cil[[i]]$tu,note$srep)
      scil[[i]]$ac=rep(cil[[i]]$ac,note$srep)
      scil[[i]]$ll=length(cil[[i]]$lid)
    }
    #dont think following is needed as cla already satisfies condition? Maybe some id-numbering to check?
    #possible cla came from some unexpected source so to be safe...
    #do this multiple times as when you remove bishops and lists you may create new bishops in less than min.lists.per.bishop
    if (!exists("min.lists.per.bishop")) min.lists.per.bishop=2
    max.l=Inf
    if (exists("maxlistlength",where=note)) { #shouldnt need this now - post 28-06-22 it is in note
      if (!is.na(note$maxlistlength)) {max.l=note$maxlistlength}
    }
    
    finished=FALSE
    while(!finished) { 
      restricted=restrict(La=scla,bil=scil,B,E,boi=1:length(scil),doi=doi,b_min_list=min.lists.per.bishop,max_list_length=max.l,rule=note$selectlists)
      finished=(identical(scla,restricted$cla) & identical(scil,restricted$cil))
      scla=restricted$cla
      scil=restricted$cil
    }
    
    # tidy=restrict(La=scla,bil=scil,B,E,boi=1:length(scil),b_min_list=2)
    # scla=tidy$cla
    # scil=tidy$cil
    # tidy=restrict(La=scla,bil=scil,B,E,boi=1:length(scil),b_min_list=2)
    # scla=tidy$cla
    # scil=tidy$cil
    ########################################
    
    sNL=length(scla)
    sNB=length(scil)
    
    srank=ComputeRanks(scil,B,E)
    sDB=max(srank,na.rm=TRUE) 
    sactive=!is.na(srank) 
    sNF=note$sNF
    sMA=max(apply(sactive,2,sum))
    
    
    if (note$SYNTHTYPE=='poststate') {
      #generate synthetic data off a state simulated from the posterior and loaded from TRUEINPUTFILE (last state)
      
      output=my.load(note$TRUEINPUTFILE)
      
      if (B!=output$B | E!=output$E | !identical(doi,output$doi) | sNB!=output$NB | sNF!=output$NF | sDB!=output$DB | note$model!=output$note$model) {
        stop('setup in run in TRUEINPUTFILE doesnt match synth setup in note - makesynthdata.R') #TODO doesnt check sMA
      } else {
        Sstate=output$state;
        if (!exists("q", where = Sstate)) {
          Sstate$q=note$true.q
          if (note$model=='bidir') {warning('poststate for synth data doesnt have a q and model=bidir so q set to note$true.q')}
        } 
      }
      showDAGs(B,T,Sstate$h,years=1:T)
      rm(output);
      
    } else {
      #generate synthetic data off a state simulated from the prior
      
      stau<-rYprior(scla,B,E) 
      sy2l<-year2list(stau,T)
      sp=note$true.p
      sq=note$true.q
      sbeta=sort(rBprior(DB=sDB,SigB=1,WARN=FALSE),decreasing=TRUE) 
      stheta=note$true.theta
      srho=note$true.rho
      
      sU<-rUprior(sactive,sNF,T,theta=stheta,rho=srho) 
      sZ<-ComputeZ(years=1:T,bishops=1:sNB,sU,srank,sbeta)
      sh<-ZtoPO(sZ,sactive,B,T,display.Z=TRUE)
      
      Sstate=list()
      Sstate$y2l=sy2l
      Sstate$tau=stau
      Sstate$p=sp
      Sstate$q=sq
      Sstate$beta<-sbeta
      Sstate$theta<-stheta
      Sstate$rho=srho
      Sstate$U=sU
      Sstate$Z=sZ
      Sstate$h<-sh
    }
    
    #now we have the synth parameters we can simulate synth lists "o"
    for (k in 1:sNL) {
      t=Sstate$tau[k]
      mc=Sstate$h[[t]]
      ind=match(scla[[k]]$o,as.numeric(rownames(mc)))
      if (note$true.p>0) {
        if (note$model=="lkdup") {
          if (note$true.q!=0) stop('model setup lkdup but true q not equal 0 - in makesynthdata.R')
          scla[[k]]$o=PunifLE(mc[ind,ind],le=NULL,p=Sstate$p) 
        }
        if (note$model=="lkddown") {
          if (note$true.q!=1) stop('model setup lkddown but true q not equal 1 - in makesynthdata.R')
          scla[[k]]$o=QPunifLE(mc[ind,ind],p=Sstate$p,q=1)  
        }
        if (note$model=="bidir") {
          scla[[k]]$o=QPunifLE(mc[ind,ind],p=Sstate$p,q=Sstate$q) #PunifLE(mc[ind,ind],le=NULL,p=Sstate$p) 
        }
      } else {
        scla[[k]]$o=unifLE(mc[ind,ind],le=NULL)  
      }
    }
  }
  
  #######################
  
  Sstate$obp=dBprior(Sstate$beta)
  Sstate$oup=dUprior(sactive,Sstate$U,Sstate$theta,Sstate$rho,bishops=1:sNB,years=1:T)
  Sstate$orp=dRprior(Sstate$rho)
  Sstate$otp=dTprior(Sstate$theta)
  Sstate$oyp=dYprior(scla,Sstate$tau,B,E)
  Sstate$oll=log.lkd.ser(Sstate$h,scla,Sstate$y2l,years=1:T,p=Sstate$p,model=note$model,q=Sstate$q) #record the llk for each list - always "serial" here
  Sstate$oll.tot=sum(unlist(Sstate$oll))
  Sstate$log.prior<-dPrior(sactive,Sstate$U,Sstate$beta,Sstate$theta,Sstate$rho,Sstate$tau,Sstate$p,Sstate$q,scla,B,E,note$PhPar,dBprior)
  Sstate$opp=dPprior(Sstate$p,note$PhPar)
  Sstate$oqp=dQprior(Sstate$q,note$PhPar)
  
  ########################
  
  return(list(Sstate=Sstate,scla=scla,scil=scil,sNF=sNF,sNL=sNL,sNB=sNB,sDB=sDB,sMA=sMA))
}


