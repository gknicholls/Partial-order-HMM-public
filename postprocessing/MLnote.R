#make self contained - 01-05-22
library(igraph) 
library(mnem) 

source(file="outputfun.R")
source(file="pofun.R")

#20-9-20
#source to calculate and plot "MLE" PO's from lists in a smoothing window of width ww+1

#it works with all lists in the T+1 years from B to E in the load file - can zoom in on 
#a subset of years of interest via "yoi"

#can get single years by setting ww=0 - need to make sure xplot, yplot large enough in 
#that case to show all output$T graphs

output=my.load("T-M2-down-sub-cn-ds-dir/T-RS5-M2-down-sub-cn-ds.RData") #we only need a few things from this output file, mainly cla, and nothing essential about choosing this one

##
#how to smooth
ww=2                              #smoothing banwidth - actually a 2*ww+1 years-width window - ww=2 so 5 year window seems plausible
m=2                               #shortest list we consider - for MLE we want all of them - the shortest of interest is two

##
#years of interest and plotting grid x/y boxes
#yoi=output$B:output$E             #all the years (will be clipped at end by ww entries adjacent E)     
yoi=1134:1136                    #example of zooming in on a subset - doesnt have to be contiguous years - eg yoi=c(1:5,51:60,72:76)+output$B-1
xplots=3; yplots=3               #PO's will be plotted as DAGS in an xplot x yplot array if xplot*yplot is as big as the number of plots, else 5 x 15

##
#loop through years of interest - referenced by index in B:E and clipped for years within ww of output$E so sliding window works
yioi=yoi[yoi<=(output$E-ww)]-output$B+1 
POML=vector('list',output$T-ww)   #MLE's will go in here - we access these by year index so we need the full T-ww length even if we focus on a few in yioi below
for (yy in yioi) {                #go through the year index

  yr=output$B+yy-1                #the calendar year
  i=which(sapply(output$cla,function(x) {cy=round((x$tl+x$tu)/2); (cy>=yr-ww & cy<=yr+ww & x$ll>=m)})) 
  #lists in cla with centre year of list in moving window

  if (length(i)>0) {              #there may be none - if so MLE is empty PO see end for this case

    ##
    #get the bare bishop lists and the attested bishops
    clai=output$cla[i]
    fst=lapply(1:m,function(k) {sapply(clai,function(x) x$o[k])}) #sort the lists in this window by first, second, third ... m entry etc
    j=do.call(order,fst)
    clai=(clai[j])                                                #sorting done
    o36=lapply(clai, function(x) x$o)                             #pull out the bishop-list field - the "36" ending is legacy of this originally targeting the one year 1136 
    ao36=unique(unlist(o36))                                      #who do we have in these lists - attested bishops
    
    #plot(NA,NA,xlim=c(1,length(clai)),ylim=c(0,max(l36)))        #this code just plots the bare lists as text - this is why we sorted, otherwise sorting isnt needed
    #dx=0; lapply(o36,function(x) {dx<<-dx+1; dy=0; lapply(x,function(y) {text(dx,dy<<-dy+1,y)})})

    ##
    #construct the MLE from the lists - this should be (!) the strongest partial order showing all _attested_ orders not contradicted 
    n=max(ao36)
    w<-lapply(o36,seq2dag,n)                                      #turn each list into a simple PO - n is just any number at least as big as the largest bishop label present - so the relevant labels are contained
    x<-lapply(w,my.transitive.closure)                               #ed 5-4-22 mnem - close the PO to get all the order constraints
    z<-matrix(0,n,n);                    
    colnames(z)<-rownames(z)<-1:n
    for (y in x) {z<-z+y}                                         #add together all the pairwise orders implied by all the lists - in a n x n matrix counts how many times each pair-order was attested
    rz=(1:n)[-sort(ao36)]                                         #remove any rows for bishops who were not present in any list in this window
    if (length(rz)>0) {z36<-z[-rz,-rz]} else {z36<-z}             #removed 

    az=setdiff(which(apply(output$active[,yy:(yy+ww),drop=FALSE],1,any)),as.numeric(rownames(z36))) #get a list of all the bishops who were active but unattested in lists in this window
    if ((laz<-length(az))>0) {                                                                      #if there are any such bishops...
      nz=dim(z36)[1]; z36b=matrix(0,nz+laz,nz+laz); z36b[1:nz,1:nz]=z36;                            #tack them onto the PO matrix with no order constraints
      colnames(z36b)<-rownames(z36b)<-c(rownames(z36),as.character(az))
    } else {z36b=z36}                                                                               #otherwise the PO is unchanged

    POML[[yy]]=transitive.reduction(0+(z36b & !t(z36b)))                                            #the final MLE PO shows all orders i->j attested (must have z36b[i,j]>0) but not contradicted (must have z36b[j,i]=0)
    ##

  } else {                                                       #no lists in this window - MLE is empty PO for active bishops in this window
    az=which(apply(output$active[,yy:(yy+ww),drop=FALSE],1,any));#get a list of all the bishops who were active in this window
    nza=length(az); 
    z36b=matrix(0,nza,nza)
    colnames(z36b)<-rownames(z36b)<-as.character(az)
    POML[[yy]]=z36b                                              #empty PO of all active bishops
  }
}

##
#plot the MLE's (smoothed in sliding window of ww+1 years) as directed graphs
#windows()
years=yoi
pdf(file='paper/v2-poHB2aRS6-MLEB.pdf',6,3)
if (xplots*yplots>=length(yioi)) {par(mfrow=c(xplots,yplots))} else {par(mfrow=c(5,15))}
par(mfrow=c(1,3),mai=c(0.1,0,0.2,0),oma=c(0,0,0,0)); 
yrp=output$B+1:(output$T-ww)-1
k=0; junk<-lapply(POML[yioi],function(x) {
  showDAG(x,main=paste('[',yrp[yioi[k<<-k+1]],']',sep=''),cex.main=4,edge.arrow.size=0.5/length(years),vertex.color=NA,
          vertex.frame.color='black',
          vertex.label.cex=0.5,vertex.size=15,edge.color='black',vertex.label.family="sans")} #,edge.arrow.size=.1,vertex.shape="none")}
  ) #, vertex.label.cex=0.5,
dev.off()
##
#output is potentially a large object so dont leave it lying around
rm(output)
gc()

