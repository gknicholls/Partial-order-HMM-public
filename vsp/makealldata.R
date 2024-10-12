MakePil<-function(la,qnames) {
  
  ############
  #make pil (Person-In-List data structure)
  
  #The aim is to gather together all the information about each person
  #in a list called pil (Person-In-List). pil[[k]] contains the following fields
  # $ name.id  : int - database id for this person
  # $ node.name: int - node name for this person - ie index in la$o etc
  # $ name     : chr - string giving name
  # $ lid      : int - [1:num_lists] the database id's for the lists this person is in 
  # $ ln       : int - [1:num_lists] the la-indices for the lists this person is in 
  # $ ac       : int - [1:num_lists] authenticity rating for each list this person is in 
  # $ ll       : int - the num_lists for this pesron
  # $ by       : int - must be in post by this date (going on lists)
  # $ till     : int - must be in post till this date (going on lists)
  
  NL=length(la) #number of lists - just the ones with bishops
  NP=length(qnames$name.id) #number of distinct people
  
  scary=order(qnames$node.name)
  qnames$name.id=qnames$name.id[scary]
  qnames$name=qnames$name[scary]
  qnames$node.name=qnames$node.name[scary]
  #qnames$to=qnames$to[scary]
  #qnames$from=qnames$from[scary]
  
  #######################################################################
  #REMARK - the order of the names in qnames$name is now the same as the
  #node index in la[[k]]$o, the list of integers indexing people in order 
  #of arrival in list k. This will work for pil lists below too.
  #j=10; unlist(lapply( pil[la[[j]]$o], function(x){x$name}))==la[[j]]$names
  #######################################################################
  
  #for each person find the lists they appear in
  #pil[[k]]$ln gives the lists person k appeared in 
  pil<-lapply(1:NP, function(x) {list(
    name.id=c(),
    node.name=c(),
    name=c(),
    lid=c(),
    ln=c(),
    tl=c(),tu=c(),
    ac=c(),
    ll=NA,
    by=NA,till=NA)
  })
  #go through the lists and write the info into pil
  for (i in 1:NL) {
    for (j in 1:(la[[i]]$ll)) {
      pin=which(qnames$name.id==la[[i]]$w[j])
      if (length(pin)!=1) {stop('wierd')}
      pil[[pin]]$ln=c(pil[[pin]]$ln,i)
      pil[[pin]]$lid=c(pil[[pin]]$lid,la[[i]]$id)
      pil[[pin]]$name.id=la[[i]]$w[j]
      pil[[pin]]$node.name=qnames$node.name[pin]
      pil[[pin]]$ac=c(pil[[pin]]$ac,la[[i]]$ac)
      pil[[pin]]$name=qnames$name[pin]
      #}
    }
  }
  
  #now go through the people and pull in the info for each person
  #for each person k, pil[[k]]$tl and ...tu give the lower limits
  #and upper limits for the times of the lists person k appeared in
  for (k in 1:NP) {
    if (!is.null(pil[[k]]$name.id)) { #this happens if they dont appear in any list - which happens if we removed some bad lists
      pil[[k]]$ll=length(pil[[k]]$ln)
      pil[[k]]$tu=pil[[k]]$tl=rep(NA,pil[[k]]$ll)
      for (jj in 1:pil[[k]]$ll) {
        j=pil[[k]]$ln[jj]
        pil[[k]]$tl[jj]=la[[j]]$year.1
        pil[[k]]$tu[jj]=la[[j]]$year.2
      }
      if (length(pil[[k]]$tl[!is.na(pil[[k]]$tl)])>0) {pil[[k]]$by=min(pil[[k]]$tu,na.rm=TRUE)}
      if (length(pil[[k]]$tu[!is.na(pil[[k]]$tu)])>0) {pil[[k]]$till=max(pil[[k]]$tl,na.rm=TRUE)}
    } else {
      pil[[k]]$ll=0
    }
  }
  #now person k must begin before pil[[k]]$by and finish after pil[[k]]$till
  
  for (i in 1:NL) { 
    la[[i]]$tl=la[[i]]$year.1
    la[[i]]$tu=la[[i]]$year.2
  }
  
  return(list(pil=pil,la=la))
}


while (exists("pnames")) detach(out) 
if (exists("out")) rm(out)       #in the bad old days I used to attach environments - this is the legacy
if (exists("la")) rm(la)
if (exists("qnames")) rm(qnames)
if (exists("pnames")) rm(pnames)   

out<-makejkdata(on=0,off=2000,takebishops=note$justbishops,include.nodates=FALSE,include.singletons=TRUE,remove.badbishops=TRUE)
attach(out) #bring la, qnames and pnames into the environment
#check the names in lists and qnames match
for (i in 1:length(la)) {for (j in 1:length(la[[i]]$names)) {iq=which(qnames$name.id==la[[i]]$w[j]); if (qnames$name[iq]!=la[[i]]$names[j]) {stop('1st la-qnames mismatch')}}}
for (i in 1:length(la)) {for (j in 1:length(la[[i]]$names)) {iq=which(qnames$name.id==la[[i]]$w[j]); if (qnames$node.name[iq]!=la[[i]]$o[j]) {stop('2nd la-qnames mismatch')}}}

##################################################################################
#remove some lists that seem problematic
if (note$removebadlists) {
  bad.lists=c(30,677,2469,2627) #actually 5 is fine it just has NA dates, could easilly be fixed
  lids=unlist(lapply(la,function(x){x$id}))
  bli=which(is.element(lids,bad.lists))
  la<-la[-bli]
}
#one might also look at the Authenticity Rating la[[i]]$ac
#and remove lists with low AR's

##################################################################################
#form list pil - this list reorganises the data to give info person-by-person
PilLa=MakePil(la,qnames)
pil=PilLa$pil
La=PilLa$la

#sanity check that the node.name of a person is the same as its index in pil
for (j in 1:length(La)) 
  if (!all(unlist(lapply( pil[La[[j]]$o], function(x){x$name}))==La[[j]]$names)) 
    stop("stop 2: oh bother")
if (note$VERBOSE) unlist(lapply( pil, function(x){x$node.name}))

###########################################################################
# repackage the data now we know who and when the focus is

restrict_all<-function(La,bil,B,E,boi,b_min_list=2,max_list_length=Inf,rule="all") 
{
  
  #Extract the subset of bishops and lists in the interval of interest
  tl=unlist(lapply(La,function(x) x$tl))
  tu=unlist(lapply(La,function(x) x$tu))
  ll=unlist(lapply(La,function(x) x$ll))
  if (rule=='half') {
    fo=sapply(1:length(La), function(i) {(1+min(E,tu[i])-max(B,tl[i]))/(1+tu[i]-tl[i])})
    loi=which(fo>=0.5 & ll<=max_list_length) #XXX was > now >= as fo is rational - may make issues for oldstarts etc as NL may change
  } else {
    if (rule=='any') {
      loi=which(tl<=E & tu>=B & ll<=max_list_length)
    } else {
      if (rule=='all') {
        loi=which(tl>=B & tu<=E & ll<=max_list_length)
      } else {
        stop('makedata: rule in note$selectlists not recognised - see main.R for options')
      }
    }
  }
  #cut the lists - messes up number indexing from list to bishop and vv (solve using "id")
  cla=La[loi]
  bll=sapply(bil[boi],function(x) x$ll)
  boi=boi[bll>=b_min_list] #person must be in b_min_list lists
  cil=bil[boi]
  
  cil.id=unlist(lapply(cil,function(x) x$name.id))
  drop=c()
  for (i in 1:length(cla)) {
    si=which(is.element(cla[[i]]$w,cil.id))
    cla[[i]]$w=cla[[i]]$w[si]
    cla[[i]]$names=cla[[i]]$names[si]
    cla[[i]]$o=match(cla[[i]]$w,cil.id)
    cla[[i]]$ll=length(cla[[i]]$w)
    if (cla[[i]]$ll<2) drop=c(drop,i) #drop lists with only one person (at all!)
  }
  
  if (length(drop)>0) cla=cla[-drop]
  
  cla.id=unlist(lapply(cla,function(x) x$id))
  nc=length(cil)
  for (i in 1:nc) {
    si=which(is.element(cil[[i]]$lid,cla.id))
    #if (length(si)==0) stop('si 0')
    cil[[i]]$lid=cil[[i]]$lid[si]
    cil[[i]]$ln=match(cil[[i]]$lid,cla.id)
    cil[[i]]$tl=cil[[i]]$tl[si]
    cil[[i]]$tu=cil[[i]]$tu[si]
    cil[[i]]$ac=cil[[i]]$ac[si]
    cil[[i]]$ll=length(cil[[i]]$lid)
    if (length(cil)!=nc) stop('length change')
  }
  
  return(list(cla=cla,cil=cil))
  
}
#do this multiple times as when you remove bishops and lists you may create new bishops in less than min.lists.per.bishop
if (!exists("min.lists.per.bishop")) min.lists.per.bishop=note$min.lists.per.bishop
max.l=Inf
if (exists("maxlistlength",where=note)) { #shouldnt need this now - post 28-06-22 it is in note
  if (!is.na(note$maxlistlength)) {max.l=note$maxlistlength}
}

dla=La; dil=pil; poi.while=1:length(pil)
finished=FALSE
while(!finished) { 
  print(c(length(dla),length(dil)))
  restricted=restrict_all(La=dla,bil=dil,B,E,boi=poi.while,b_min_list=min.lists.per.bishop,max_list_length=max.l,rule=note$selectlists)
  finished=(identical(dla,restricted$cla) & identical(dil,restricted$cil))
  dla=restricted$cla
  dil=restricted$cil
  poi.while=1:length(dil)
}
print(c(length(dla),length(dil)))

########################################

for (i in 1:length(dla)) {for (j in 1:length(dla[[i]]$names)) {iq=which(qnames$name.id==dla[[i]]$w[j]); if (qnames$name[iq]!=dla[[i]]$names[j]) {stop('1st la-qnames mismatch')}}}
for (j in 1:length(dla)) 
  if (!all(unlist(lapply( dil[dla[[j]]$o], function(x){x$name}))==dla[[j]]$names)) 
    stop("stop 2: oh bother")

