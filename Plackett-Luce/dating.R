
#################################################################################
#From data to X matrix giving covariates over time. Also example Z/U process simulation

#Get.DioDates()
#Make diocese dates/lists consistent - and removes some bishops from some lists
#returns pil & bil data structures giving witness-list and date info for each bishop

#plot.dio.dates()
#plot the dating info on bishops in dioceses

#getX()
#construct the design matrix X for PO latent-variable means

#Zsim()
#simulate the U process and combine with X to get the Z-process

#showZ()
#plot a Z-matrix in a single time-slice

##################################################################################
# Reorganise data to give info for each bishop in "bil" and "pil" data structures 
# pil is basic, bil has various dating conflicts removed and is used for making X

Get.DioDates<-function(la,qnames) {
  
  ############
  #make pil (Person-In-List data structure)
  
  #The aim is to gather together all the information about each bishop
  #in a list called pil (Person-In-List). pil[[k]] contains the following fields
  # $ name.id  : int - database id for this person
  # $ node.name: int - node name for this bishop - ie index in la$o etc
  # $ name     : chr - string giving name of bishop
  # $ diocese  : chr - string giving diocese of bishop
  # $ di       : int - diocese index (filled later, after X is built)
  # $ lid      : int - [1:num_lists] the database id's for the lists this bishop is in 
  # $ ln       : int - [1:num_lists] the la-indices for the lists this bishop is in 
  # $ tl       : int - [1:num_lists] lower bounds for each list this bishop is in 
  # $ tu       : int - [1:num_lists] upper bounds for each list this bishop is in 
  # $ ac       : int - [1:num_lists] authenticity rating for each list this bishop is in 
  # $ ll       : int - the num_lists for this bishop
  # $ by       : int - must be in post by this date (going on lists)
  # $ till     : int - must be in post till this date (going on lists)
  # $ from     : int - wiki start date
  # $ to       : int - wiki end date
  # $ begin    : int - rationalisation of start date by/from
  # $ end      : int - rationalisation of end date till/to
  
  NL=length(la) #number of lists - just the ones with bishops
  NP=length(qnames$name.id) #number of distinct people
  
  #wiki data
  qnames$from=qnames$to=integer(NP)
  bdates=read.csv(file="BishopDates.csv",row.names=1)
  qinb=match(qnames$name,bdates$name)
  qnames$from=bdates$from[qinb] 
  qnames$to=bdates$to[qinb]
  if (length(qinb)!=NP) warning("some bishops returned by makejkdata not found in BishopDates.csv")
  #source(file="bishops10/bishop_dates.R") #wiki data dating bishops - deprecated use csv file
  if (!all(1:NP==sort(qnames$node.name)))  warning("node.name isnt 1:NP")
  
  scary=order(qnames$node.name)
  qnames$name.id=qnames$name.id[scary]
  qnames$name=qnames$name[scary]
  qnames$node.name=qnames$node.name[scary]
  qnames$diocese=qnames$diocese[scary]
  qnames$to=qnames$to[scary]
  qnames$from=qnames$from[scary]
  
  #######################################################################
  #REMARK - the order of the names in qnames$name is now the same as the
  #node index in la[[k]]$o, the list of integers indexing bishops in order 
  #of arrival in list k. This will work for bil and pil lists below too.
  #j=10; unlist(lapply( bil[la[[j]]$o], function(x){x$name}))==la[[j]]$names
  #######################################################################
  
  #for each person find the lists they appear in
  #pil[[k]]$ln gives the lists person k appeared in 
  pil<-lapply(1:NP, function(x) {list(
    name.id=c(),
    node.name=c(),
    name=c(),
    diocese=c(),
    di=c(),
    lid=c(),
    ln=c(),
    tl=c(),tu=c(),
    ac=c(),
    ll=NA,
    by=NA,till=NA,
    from=NA,to=NA)
  })
  #go through the lists and write the info into pil
  for (i in 1:NL) {
    for (j in 1:(la[[i]]$ll)) {
      pin=which(qnames$name.id==la[[i]]$w[j])
      #if (length(pin)!=1) {stop('wierd')} else {
      pil[[pin]]$ln=c(pil[[pin]]$ln,i)
      pil[[pin]]$lid=c(pil[[pin]]$lid,la[[i]]$id)
      pil[[pin]]$name.id=la[[i]]$w[j]
      pil[[pin]]$node.name=qnames$node.name[pin]
      pil[[pin]]$ac=c(pil[[pin]]$ac,la[[i]]$ac)
      pil[[pin]]$name=qnames$name[pin]
      pil[[pin]]$diocese=qnames$diocese[pin]
      #}
    }
  }
  
  #now go through the bishops and pull in the info for this bishop
  #for each person k, pil[[k]]$tl and ...tu give the lower limits
  #and upper limits for the times of the lists person k appeared in
  for (k in 1:NP) {
    pil[[k]]$ll=length(pil[[k]]$ln)
    pil[[k]]$tu=pil[[k]]$tl=rep(NA,pil[[k]]$ll)
    pil[[k]]$begin=pil[[k]]$from=qnames$from[k]
    pil[[k]]$end=pil[[k]]$to=qnames$to[k]
    for (jj in 1:pil[[k]]$ll) {
      j=pil[[k]]$ln[jj]
      pil[[k]]$tl[jj]=la[[j]]$year.1
      pil[[k]]$tu[jj]=la[[j]]$year.2
    }
    if (length(pil[[k]]$tl[!is.na(pil[[k]]$tl)])>0) {pil[[k]]$by=min(pil[[k]]$tu,na.rm=TRUE)}
    if (length(pil[[k]]$tu[!is.na(pil[[k]]$tu)])>0) {pil[[k]]$till=max(pil[[k]]$tl,na.rm=TRUE)}
  }
  #now person k must begin before pil[[k]]$by and finish after pil[[k]]$till
  
  
  #####################################################################
  # Make bil from pil - clean up diocese occupation times
  # the cleaned-up version of pil will be called bil
  
  
  bil=pil
  dioceses=unlist(lapply(bil,function(x){x$diocese}))
  dio.names<-unique(dioceses)
  ND=length(dio.names)
  
  #go through the unique dioceses
  for (di in 1:ND) {
    #get all the bishops in that diocese
    dio=dio.names[di]
    doi.i=which(qnames$diocese==dio)
    doi.n=length(doi.i) #number of bishops appearing in this diocese
    dil=pil[doi.i]      #pull out sublist for this diocese from pil
    tos=unlist(lapply(dil,function(x){x$to}))
    o=order(tos)
    #go though the bishops from the first to the last, ordered by bil[[]]$to
    for (i in 1:doi.n) {
      b=o[i] #b is now the index in dil of the i'th bishop in this diocese
      if (i==1) { #for the first bishop there is no earlier bishop to clash dates with
        blj=c()   #blj is a vector of the indices of bad lists for this bishop
      } else {
        a=o[i-1]
        blj=which(is.element(dil[[b]]$ln,dil[[a]]$ln)) 
        #two bishops from one diocese in same list 
        #so we will remove the list from the lists of the later bishop in the pair
      }
      for (j in 1:dil[[b]]$ll) {
        #go through the lists of current bishop (bishop b in dil[[]])
        if ( !is.na(dil[[b]]$tl[j]) && !is.na(dil[[b]]$to) && dil[[b]]$tl[j]>dil[[b]]$to) {
          blj=c(blj,j) #if the list appeared after the bishop dies, remove list from bishop
        }
        if (i>1 && !is.na(dil[[b]]$tu[j]) && !is.na(dil[[a]]$to) && dil[[b]]$tu[j]<=dil[[a]]$to) {
          blj=c(blj,j) #if the list appeared before the previous bishop dies, remove list from bishop
        }
        #COMPLETE TRUST IN WIKI DATES - if we really go for this then this fn could be simplified
        if (!is.na(dil[[b]]$tu[j]) && !is.na(dil[[b]]$from) && dil[[b]]$tu[j]<=dil[[b]]$from) {
          blj=c(blj,j) #if list appeared before bishop was in post (wiki), remove list from bishop
        }
      }
      blj=unique(blj) #list could be bad on multiple counts
      if (length(blj)>0) {
        #go through all the list-info for this bishop and clean out the bad entries
        dil[[b]]$ln=dil[[b]]$ln[-blj]
        dil[[b]]$lid=dil[[b]]$lid[-blj]
        dil[[b]]$tl=dil[[b]]$tl[-blj]
        dil[[b]]$tu=dil[[b]]$tu[-blj]
        dil[[b]]$ac=dil[[b]]$ac[-blj]
        dil[[b]]$ll=length(dil[[b]]$ln)
      }
      
      #key final step - rationalise the $from/$to info from wiki and the $by/$till
      #info from the lists - first update the $by/$till data as we have deleted soem lists
      #from the lists of lists for some bishops
      
      #the conclusion of this is the $begin/$end values which are our best estimates
      if (length(dil[[b]]$tu[!is.na(dil[[b]]$tu)])>0) {
        dil[[b]]$by=min(dil[[b]]$tu,na.rm=TRUE)
        dil[[b]]$till=max(dil[[b]]$tl,na.rm=TRUE)
        
        if (i==1) {
          dil[[b]]$begin=min(dil[[b]]$from,dil[[b]]$by) 
          #first bishop starts at earlier of $from and $by
        } else {
          #later bishops take earlier of $from and $by but this cannot precede
          #the earlier bishop's $to-value
          dil[[b]]$begin=min(dil[[b]]$from,max(dil[[b]]$by,dil[[a]]$to+1))
        }
      } else {
        #in rare cases where the bishop has no list info
        dil[[b]]$by=dil[[b]]$from
        dil[[b]]$begin=dil[[b]]$from
      }
      dil[[b]]$end=dil[[b]]$to 
      #for the end date I actually just trust the wiki date as the data almost all agreed
      #it was just the start dates (ie $begin) that needed work.
    }
    bil[doi.i]=dil
  }
  #clip the list times to respect the begin/end values
  bla=la
  for (i in 1:NL) { 
    id=bla[[i]]$id
    il=unlist(lapply(bil,function(x) is.element(id,x$lid)))
    dil=bil[il]
    survive=unlist(lapply(dil,function(x) x$node.name))
    si=which(is.element(bla[[i]]$o,survive))
    bla[[i]]$o=bla[[i]]$o[si]
    bla[[i]]$w=bla[[i]]$w[si]
    bla[[i]]$names=bla[[i]]$names[si]
    bla[[i]]$ll=length(bla[[i]]$o)
    begins=c(bla[[i]]$year.1,unlist(lapply(dil,function(x) x$begin)))
    bla[[i]]$tl=max(begins,na.rm=TRUE)
    ends=c(bla[[i]]$year.2,unlist(lapply(dil,function(x) x$end)))
    bla[[i]]$tu=min(ends,na.rm=TRUE)
    if (bla[[i]]$tu<bla[[i]]$tl) stop("problem child")
  }
  #write the clipped times back into bil - not pil as thats "raw" data
  for (i in 1:NP) {
    if (bil[[i]]$ll>0) {
      for (j in 1:bil[[i]]$ll) {
        bil[[i]]$tl[j]=bla[[bil[[i]]$ln[j]]]$tl
        bil[[i]]$tu[j]=bla[[bil[[i]]$ln[j]]]$tu
      }
    }
  }
  return(list(ProcessedBishops=bil,OriginalBishops=pil,ProcessedLists=bla,Qnames=qnames))
}


##################################################################################
# visualise data

plot.dio.dates<-function(pil,dio.names,file=NA,ac.min=0) {
  #this is a plotting function, displaying list appearances and bishop dates together
  
  #pil is the Person-In-List data structure
  #dio.names is a vector of the diocese-strings we want to plot
  
  #if file is a string the plots are written as a pdf
  #ac.min puts a plus on any lists with ac below ac.min
  
  dioceses=unlist(lapply(pil,function(x){x$diocese}))
  
  if (!is.na(file)) pdf(file,8,5)
  
  for (dio in dio.names) {
    doi.i=which(dioceses==dio);
    doi.pil=pil[doi.i]
    doi.pil=doi.pil[order(unlist(lapply(doi.pil,function(x){x$from})))]
    doi.n=length(doi.pil); doi.n
    nl=sum(unlist(lapply(doi.pil,function(x){x$ll})))
    ft=min(unlist(lapply(doi.pil,function(x){x$tl})),na.rm=TRUE); 
    lt=max(unlist(lapply(doi.pil,function(x){x$tu})),na.rm=TRUE); 
    
    ln=0;delta=4
    par(xpd=NA)
    plot(1:(nl+doi.n*(delta+2)),rep(0,nl+doi.n*(delta+2)),
         xlim=c(0,nl+doi.n*(delta+2)+1),ylim=c(ft-1,lt+1+50),
         type='n',ylab="Year",xlab="Witness list/Bishop in post",xaxt="n",main=dio)
    for (i in 1:doi.n) {
      cl=doi.pil[[i]]; 
      if (cl$ll>0) {
        for (j in 1:cl$ll) {
          ln=ln+1; 
          lines(c(ln,ln),c(cl$tl[j],cl$tu[j]),col=i)
          points(c(ln,ln),c(cl$tl[j],cl$tu[j]),col=i,pch=1,cex=1)
          if ( (!is.na(cl$end) && !is.na(cl$tl[j]) && cl$tl[j]>cl$end) || 
               (!is.na(cl$begin) && !is.na(cl$tu[j]) && cl$tu[j]<cl$begin)) {
            points(ln,(cl$tl[j]+cl$tu[j])/2,pch="x",cex=1.5,col=i)
            text(ln,(cl$tl[j]+cl$tu[j])/2,as.character(cl$lid[j]),cex=0.6,pos=4,adj=0.5,srt=270)
          }
          if (cl$ac[j]<ac.min) {
            points(ln,(cl$tl[j]+cl$tu[j])/2,pch="+",cex=1.5,col=i)
          }
        }
      }
      ln=ln+1;
      lines(c(ln,ln),c(cl$from,cl$to),col=i,lwd=3)
      points(c(ln,ln),c(cl$from,cl$to),col=i,cex=2,pch="*")
      ln=ln+1;
      lines(c(ln,ln),c(cl$begin,cl$end),col=i,lwd=3)
      points(c(ln,ln),c(cl$begin,cl$end),col=i,cex=2,pch="+")
      ln=ln+1;
      lines(c(ln,ln),c(cl$by,cl$till),col=i,lwd=3)
      #abline(h=c(cl$by,cl$till),lty=2,col=i)
      text(ln,cl$by,cl$name,adj=1,pos=4,srt=90,cex=0.6,col=1)
      #abline(h=c(cl$by,cl$till),lty=2,col=i)
      ln=ln+delta
    }
    
    print(dio)
    #unlist(lapply(doi.pil,function(x){x$by}))
    #unlist(lapply(doi.pil,function(x){x$till}))
    
  }
  if (!is.na(file)) dev.off()
}

#dioceses=unlist(lapply(pil,function(x){x$diocese}))
#dio.names<-unique(dioceses)
#plot.dio.dates(pil,dio.names)

##################################################################################
#Construct the X array in each year

getX<-function(doi,bil,B,E) {
  #construct the X array in each year
  #X[,,t] is the ND x NC (num diocese x num covariates) design matrix in the t'th year
  #t=1 is the index for year B, t=T with T=E-B+1 is index for year E
  #doi=names of Dioceses of Interest,bil=person-in-list data structure,B=start date,E=end date
  #output is X,who,tio,drk, see below for defs
  
  ND=length(doi);NP=length(bil)
  
  begins=unlist(lapply(bil,function(x){x$begin}))
  ends=unlist(lapply(bil,function(x){x$end}))
  dioceses=unlist(lapply(bil,function(x){x$diocese}))
  T=E-B+1
  
  who=matrix(NA,ND,T,dimnames=list(doi,B:E)) #who[d,y]=index in bil for bishop in dio d in year y
  #initialise Time In Office tio to -1 so if diocese is empty it ranks last
  tio=matrix(-1,ND,T,dimnames=list(doi,B:E)) #tio[d,y]=time in office for current holder of diocese d in year y
  drk=matrix(NA,ND,T,dimnames=list(doi,B:E)) #drk[d,y]=rank of diocese d in year y
  
  #if we want an effect in X for each diocese
  #covariate.names=c(paste(abbreviate(doi,4),".re",sep=""),c(paste("rank.",1:ND,sep="")))
  
  #if we want to push the diocess effect into the U-process we just have the rank effect
  covariate.names=c(paste("rank.",1:ND,sep=""))
  
  NC=length(covariate.names) #number of covariates
  X=array(dim=c(ND,NC,T),dimnames=list(doi,covariate.names,B:E))
  MZ=diag(ND)-1/ND #MZ: subtract mean of effects for identifiability - do this for rank effect 
  
  #go through the years 
  for (t in 1:T) {
    yr=B+t-1
    #go through the dioceses
    for (d in 1:ND) {
      #[d,t] is a diocese-year pair
      active=which(begins<=yr & ends>=yr & dioceses==doi[d]) #bil-index of bishops in dio d active in year t
      #the number of active bishops in dio d could be 0 or 1. 
      if (length(active)==1) {
        who[d,t]=active                    #this is the bil-index of the guy we want 
        tio[d,t]=yr-bil[[active]]$begin+1  #the record for this bishop is in bil[[active]]
      }
      #there are some conflicts in the data
      if (length(active)>1) stop("wierdness") 
      #there will be some cases where bishops start and end in the same year
      #handle this on a case-by-case basis
    }
    drk[,t]=ND+1-rank(tio[,t],ties="first")#now the tio's are know for this year, rank them
    #unoccupied dioceses have the lowest rank and will stay lowest till they are occupied
    
    #do this if you want to include an effect for diocess explicitly
    #X[,1:ND,t]=diag(ND) #the first block is just indicators for diocese
    #X[,(ND+1):(2*ND),t]=diag(ND)[drk[,1],]%*%MZ 
    
    #do this if you want to just have rank as a covariate
    X[,1:ND,t]=diag(ND)[drk[,1],]%*%MZ 
    #diag(ND)[drk[,1],]%*%MZ is a nominal covariate for each tio-rank level
    #MZ is mean-subtraction operator
    
  }
  boi=unique(who[1:prod(dim(who))]); boi=boi[!is.na(boi)]
  return(list(X=X,who=who,tio=tio,drk=drk,boi=boi))
}

makejkdata<-function(on=1122,off=1128,takebishops=TRUE,include.nodates=FALSE,include.singletons=FALSE,remove.badbishops=FALSE) {
  
  #read the data from files
  act <- read.table("act.txt", sep="\t", header = T)
  person <- read.table("person.txt", sep="\t", header = T)
  pnames <- read.table("person name.txt", sep="\t", quote = "\"", header = T, fill = T)
  
  #pull out the first name, second name and position information
  #most have just 1st and 2nd or 1st and 3rd, but see for example
  #Roger (1st), d'Abetot (2nd), sheriff of Worcester (3rd) entry 4563, id 4723
  z<-pnames[,c('name.entry.element','name.second.element','name.third.element')]
  z2<-apply(cbind(levels(z[,1])[z[,1]],levels(z[,2])[z[,2]],
                  levels(z[,3])[z[,3]]),1,paste,collapse=', ')
  z3<-sub(', ,',',', z2) 
  z4<-sub(', +$','', z3)
  z4n<-pnames$person.name.ID
  
  #a list of name-ids (not row #s!) corresponding to Bishops
  if (takebishops) {
    #select the bishops
    rw<-grep(c("^bishop"),pnames$name.third.element,ignore.case=TRUE)
    bip<-pnames$person.name.ID[rw]
    
    #standardise their dioceses
    z5<-pnames[,c('name.third.element')][rw]
    diocese<-sub("-[[:digit:]]+","",
                 sub(" [[:digit:]]+","",
                     sub(", [[:graph:]]+","",
                         sub("bishop[[:blank:]]+of ","",z5,ignore.case=TRUE))))
    diocese<-sub("St David's","St Davids",diocese)
    diocese<-sub("Chester-Coventry","Chester",diocese)
    diocese<-sub(" Stephen","",diocese)
    diocese<-sub("S[[:graph:]][[:graph:]][[:graph:]]$","Sees",diocese)
    #bip the person id's matching diocese - person bip[i] is in diocese[i]
    #diocese<-sort(unique(diocese))
    
    if (remove.badbishops) {
      
      bad.dio=c(grep("Lincoln/Chester",diocese),grep("Thetford/Norwich",diocese),
                grep("^bishop$",diocese),grep("uncertain",diocese),grep("attesting",diocese))
      diocese<-diocese[-bad.dio]
      bip<-bip[-bad.dio]
      rw<-rw[-bad.dio]
      
      #same thing - check
      #bad.pn=c(grep("Lincoln/Chester",pnames$name.third.element),grep("Thetford/Norwich",pnames$name.third.element),
      #         grep("^bishop$",pnames$name.third.element),
      #         grep("bishop, identity uncertain",pnames$name.third.element),grep("attesting",pnames$name.third.element))
      #bad.nid<-pnames[bad.pn,]$person.name.ID
      #bip<-setdiff(bip,bad.nid)
      
    }
    
    #GKN 1/1/19
    
  }
  
  #make a list of all the act id's that people appear in
  v<-unique(person$act.ID)
  la<-NULL
  count<-0
  
  for (i in v) {
    
    #find the act with id=i in the list of acts 
    a<-act[act$act.ID==i,]
    
    #if the year.2 field is empty or 0 set it equal the year.1 value
    #ie if the data is telling us it happened in a single year, make the
    #upper and lower limits equal
    if ( (is.na(a$year.2)) | (a$year.2==0) ) {a$year.2<-a$year.1}
    
    #get all the people who were witnesses in this act
    #z has columns  "person.name.ID" and "name.sequence" the latter being
    #their position in the list
    z<-person[(person$act.ID==i)&(person$name.role=='W'),c(2,4)]
    
    #Is this act of further interest? 
    #Were there any witnesses (dim...>1)
    #Are there any data on the time of the act (if year.1 is null forget it)?
    #Is the lower limit after 'on' and the upper limit before 'off'?
    if ( (dim(z)[1]>1 | include.singletons) & (!is.na(a$year.1) | include.nodates) & (is.na(a$year.1) | ((a$year.2<off) & (a$year.1>on))) ) {
      #the list order of the people in this witness list
      zs<-sort.int(z[,2],index.return=TRUE)
      #if (any(diff(zs$x)!=1)) {cat('this stinks \n'); break}
      if (takebishops) {
        #take the subset of witnesses who are bishops
        tem<-list(w=intersect(z[zs$ix,1],bip),id=i,year.1=a$year.1, year.2=a$year.2, ac=a$authenticity.code)
        tem$allnames<-unlist(lapply(z[zs$ix,1],function(x) {z4[z4n==x]}))
      } else {
        tem<-list(w=z[zs$ix,1],id=i,year.1=a$year.1, year.2=a$year.2, ac=a$authenticity.code)
      }
      tem$ll<-length(tem$w)
      tem$names<-unlist(lapply(tem$w,function(x) {z4[z4n==x]}))
      if (tem$ll>1) {
        count<-count+1
        la[[count]]<-tem
      }
    }
  }
  n.order<-count
  
  #make a visual display of the lists
  unlist(lapply(la,function(x) x$ll))
  
  #la$w has name id sorted by list location but these name id's are not packed
  #so gather together all the distinct name ids
  #that appear in our list, and give them a new, extra id
  #that just counts up to the number in our subset
  v2nid<-unique(unlist(lapply(la,function(x) x$w)))
  
  #mapping name id to vertex
  nid2v<-function(i) {
    els<-length(i)
    u<-rep(NaN,els)
    for (j in 1:els) {
      u[j]<-which(v2nid==i[j])
    }
    u
  }
  #use vertex id's in data
  for (count in 1:n.order) {la[[count]]$o<-nid2v(la[[count]]$w)}
  
  #return the selected and processed data
  if (takebishops) {
    bin=is.element(bip,v2nid); #which bishops made into the lists?
    b.name.id=bip[bin];        #thin the bishop id's
    diocese=diocese[bin]       #and matching dioceses
    rw=rw[bin]                 #the rows of pnames that ended up getting used
    return( list(la=la,
                 pnames=list(name.id=pnames[,1], name=z4),
                 qnames=list(name.id=b.name.id,name=z4[rw],node.name=nid2v(b.name.id),diocese=diocese))
    )
  } else {
    just.used=is.element(pnames[,1],v2nid)
    return( list(la=la,
                 pnames=list(name.id=pnames[,1], name=z4),
                 qnames=list(name.id=pnames[just.used,1],name=z4[just.used],node.name=nid2v(v2nid),diocese=NA))
    )
  } 
}
