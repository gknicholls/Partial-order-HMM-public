makedata<-function(note,B,E,T) {
  
  ########################################################################################
  #updated 04/11/19
  
  #Sorting the dates/dioceses/lists for consistency 
  
  ##################################################################################
  #Get the data
  #this is the version of makejkdata which outputs diocese data in qnames
  while (exists("pnames")) detach(out) 
  if (exists("out")) rm(out)       #in the bad old days I used to attach environments - this is the legacy
  if (exists("la")) rm(la)
  if (exists("qnames")) rm(qnames)
  if (exists("pnames")) rm(pnames)   
  out<-makejkdata(on=0,off=2000,takebishops=TRUE,include.nodates=TRUE,include.singletons=FALSE,remove.badbishops=TRUE)
  attach(out) #bring la, qnames and pnames into the environment
  
  ##################################################################################
  #remove some lists that seem problematic
  bad.lists=c(30,677,2469,2501,2627) #actually 5 is fine it just has NA dates, could easilly be fixed
  lids=unlist(lapply(la,function(x){x$id}))
  bli=which(is.element(lids,bad.lists))
  la<-la[-bli]
  #one might also look at the Authenticity Rating la[[i]]$ac
  #and remove lists with low AR's
  
  ##################################################################################
  #form lists bil & pil - these lists reorganise the data to give info bishop-by-bishop
  DioDates=Get.DioDates(la,qnames,note)
  bil=DioDates$ProcessedBishops #these bishop/diocese/dates are consistent
  pil=DioDates$OriginalBishops
  dioceses=unlist(lapply(bil,function(x){x$diocese}))
  La=DioDates$ProcessedLists    #these lists only contain consistent bishops
  
  qnames=DioDates$Qnames
  if (!all(qnames$diocese==dioceses)) stop('stop 1: dio names issue')#sanity check
  dio.names<-unique(dioceses)
  
  #filename="Bishop_Dat_Visual.pdf"
  #plot.dio.dates(bil,dio.names,file=filename)
  #par(mfrow=c(4,8))
  #plot.dio.dates(bil,dio.names) #or pil to see the unprocessed data
  
  #sanity check that the node.name of a bishop is the same as its index in bil
  for (j in 1:length(la)) 
    if (!all(unlist(lapply( bil[la[[j]]$o], function(x){x$name}))==la[[j]]$names)) 
      stop("stop 2: oh bother")
  if (note$VERBOSE) unlist(lapply( bil, function(x){x$node.name}))
  
  ##################################################################################
  # bil now gives dates for each bishop and their diocese, consistent for building X
  # bil also contains the list info for each bishop, but X doesnt need that
  
  #doi: Dioceses Of Interest
  if (note$VERBOSE) {
    table(dioceses) #how many bishops in each diocese over period of interest [B,E]
    dio.names #we typically focus on a few dioceses of interest - in this example 10
  }
  
  doi<-note$doi
  
  #doi<-sort(c("Worcester","London","Winchester","Hereford",
  #            "Chichester","Durham","Chester","Lincoln","Salisbury","St Davids",
  #            "Evreux","Ely","Carlisle","Lisieux","Sees","Norwich","Bayeux","Avranches","Coutances","Rochester"))
  if (!all(sort(doi)==sort(intersect(doi,dio.names)))) stop('stop 3: dio name mispelt?')#check the names are spelt correctly!
  
  ##################################################################################
  # Make X for the DOI using bil - specify period of interest - also extracts bishops 
  # of interest
  
  #V$who[d,y]=index in bil for bishop in dio d in year y
  #V$tio[d,y]=time in office for current holder of diocese d in year y
  #V$drk[d,y]=rank of diocese d in year y
  #V$X is X
  
  #TODO - we could do something with tio so that we dont update U in (d,t) pairs where dio d is empty
  V=getX(doi,bil,B=B,E=E)
  if (note$VERBOSE) {
    round(apply(V$tio==-1,1,mean),2)
    image(V$tio) #sanity check
    image(V$drk)
  }
  X=V$X
  Dcp=(V$tio<=1) #Dcp[d,t] is TRUE if either no bishop or new bishop in diocese d in year t
  if (note$VERBOSE) image(Dcp)
  boi=V$boi
  
  ###########################################################################
  # repackage the data now we know who and when the focus is
  
  #do this multiple times as when you remove bishops and lists you may create new bishops in less than min.lists.per.bishop
  if (!exists("min.lists.per.bishop")) min.lists.per.bishop=note$min.lists.per.bishop
  max.l=Inf
  if (exists("maxlistlength",where=note)) { #shouldnt need this now - post 28-06-22 it is in note
    if (!is.na(note$maxlistlength)) {max.l=note$maxlistlength}
  }
  
  cla=La; cil=bil; boi.while=boi
  finished=FALSE
  while(!finished) { 
    print(c(length(cla),length(cil)))
    restricted=restrict(La=cla,bil=cil,B,E,boi=boi.while,doi=doi,b_min_list=min.lists.per.bishop,max_list_length=max.l,rule=note$selectlists)
    finished=(identical(cla,restricted$cla) & identical(cil,restricted$cil))
    cla=restricted$cla
    cil=restricted$cil
    boi.while=1:length(cil)
  }
  print(c(length(cla),length(cil)))
  ########################################
  
  return(list(cla=cla,cil=cil,doi=doi))
}
