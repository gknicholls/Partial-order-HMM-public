
makeparameters<-function(note,cla,cil,B=note$B,E=note$E,Sstate=NA) {
  
  #######################################
  #cil contains info about bishops
  #cla contains info about lists
  #B & E are years beginning and end for windowed analysis
  
  T=E-B+1
  
  ## Setup basic dimensions, rank data and bishop activity indicator 
  #Number of Bishops
  NB=length(cil)
  #number of lists
  NL=length(cla)
  
  #number of dioceses - not used?
  #dio.uniq=unique(sapply(cil,function(x) x$diocese))
  #ND=length(dio.uniq)
  
  #rank[b,t] gives rank of bishop b in year t
  rank=ComputeRanks(cil,B,E)
  #NB x T indictor matrix for active bishops
  active=!is.na(rank) 
  MA=max(apply(active,2,sum))
  
  #plot(B:E,apply(active,2,sum),type="l",xlab="Year",ylab="Number in office")
  
  #dim of beta (largest rank seen) is less than max active due to ties
  DB=max(rank,na.rm=TRUE) 
  
  #Number of Features in the PO representation - often called K
  if (note$DOSYNTH) {
    #updated to MA july 2023 GKN
    if (is.na(note$sNF)) {NF=floor(MA/2)} else {NF=note$sNF}  
  } else {
    if (is.na(note$NF)) {NF=floor(MA/2)} else {NF=note$NF}  
  }
  
  #set prior hyper parameters
  PhPar=note$PhPar
  
  if (note$STARTSTATE=='disordered') {
    #Disordered start state - low to zero depth
    
    state=list()
    
    #assign times - these are uniform random in allowed interval - y2l[[t]] gives lists assigned to year t by tau
    state$tau=rYprior(cla,B,E)
    state$y2l=year2list(state$tau,T)
    
    state$p=note$init.p
    state$q=note$init.q
    state$beta<-rep(0,DB)
    #generate system with very low order preference - works well if DB is 
    is.good=FALSE
    while (!is.good) {
      state$U=rUprior(active,NF,T,theta=0,rho=0) #the probability for an order between any pair of bishops is 1/2^NF so mostly no-order
      state$Z=ComputeZ(years=1:T,bishops=1:NB,state$U,rank,state$beta)
      state$h<-ZtoPO(state$Z,active,B,T,display.Z=note$DRAW)
      is.good<-noconflict2(state$h,state$y2l,cla)$outcome
    }
    state$theta<-note$init.theta
    state$rho<-note$init.rho
    
  }
  
  if (note$STARTSTATE=='oldstart') {
    #start state from run file - last saved state
    
    output=my.load(note$STARTFILE)
    
    if (B!=output$B | E!=output$E | !identical(doi,output$doi) | NB!=output$NB | NF!=output$NF | NL!=output$NL | DB!=output$DB) {
      stop('starting from an old state that doesnt match in makeparameters.R')
    } else {
      state=output$state;
      if (!exists("q",where=state)) {state$q=0+(note$model=='lkddown')}
    }
    rm(output)
  }
  
  if (note$STARTSTATE=='ordered') {
    #Ordered state something like MLE but hacky
    
    state=list()
    
    #assign times - these are uniform random in allowed interval - y2l[[t]] gives lists assigned to year t by tau
    state$tau=rYprior(cla,B,E)
    state$y2l=year2list(state$tau,T)
    
    state$p=note$init.p
    state$q=note$init.q
    state$beta<-rep(0,DB)
    state$theta<-note$init.theta
    state$rho=note$init.rho  #rRprior(fac=PhPar$rfac)
    
    #U=array(0,c(NB,NF,T)) 
    U<-rUprior(active,NF,T,theta=state$theta,rho=state$rho) 
    V<-Uadjust(U,active,T,state$y2l,cla) #TODO cla argument
    state$U<-V
    
    state$Z=ComputeZ(years=1:T,bishops=1:NB,V,rank,state$beta) #make sure to init beta to zero
    state$h<-ZtoPO(state$Z,active,B,T,display.Z=note$DRAW)
    
  }
  
  if (note$STARTSTATE=='true') {
    if (!note$DOSYNTH) stop('set note$DOSYNTH=TRUE if you want to sart in the true state!')
    state=list()
    state$y2l=Sstate$y2l
    state$tau=Sstate$tau
    state$p=Sstate$p
    state$q=Sstate$q
    state$beta<-Sstate$beta*note$init.beta.fac
    state$theta<-Sstate$theta
    state$rho=Sstate$rho
    state$U<-Sstate$U*note$init.U.fac
    state$Z<-Sstate$Z
    state$h<-Sstate$h
    #state$log.prior<-Sstate$log.prior
  }
  
  if (note$STARTSTATE=='none') {
    state=list()
  }
  
  return(list(NB=NB,NF=NF,NL=NL,DB=DB,MA=MA,rank=rank,active=active,state=state,PhPar=PhPar))
  
}
