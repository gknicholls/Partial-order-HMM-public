# vsp MCMC

source('~/Desktop/POwTie/code/source-code/pofun.R')
source('~/Desktop/vsp/vsp.R')

library(Zseq)
library(parallel)

# probability distributions
loglikP.vsp <- function(g,Y,r){
  
  if (noconflict4(Y,PO=vsp2po(g))){
  # if conflict: return -Inf
    if (length(unique(lengths(Y)))==1){
    # if Y is all of the same lengths
      loglk=(-length(Y)*log(nle.vsp(subtree(g,Y[[1]]))))
      
    } else {
      
      loglkp = function(y,g){-log(nle.vsp(subtree(g,y)))}
      loglk=sum(sapply(Y,loglkp,g=g))
      
    }
    
  } else {loglk = -Inf}
  
  return(loglk)
}

loglikQ.vsp <- function(g,Y,r=0,model='lkddown') {
  
  ## NEED TO OPTIMISE ##
  # checked: 'lkddown' & 'lkdup'
  
  #loglikelihood r=prob for error, g is a vsp-tree
  #Y[[]] is a list, elements have a field which is an order sequence o
  
  ##example
  #Y<-list()
  #Y[[1]]<-list(o=c(1,2,3,4,5))
  #Y[[2]]<-list(o=c(1,3,2,4,5))
  #loglkd(r=0.5,g,Y,'lkdup')
  
  ##if the seq builder chooses each person at random
  #loglkd(r=1,g,Y,'lkdup')
  #-log(factorial(5))
  
  if (model=='prior') {return(0)}
  
  lkddown.fac<-function(r,g,o) {
    
    #evaluate the log-probability for the
    #placement of o[1] - the first person in the order o
    #given the PO with vsp-tree g
    
    #g,o, an order of length 1, just one way to place it
    if (length(o)==1) {return(0)}
    
    #the sub-vsp-tree for the order
    mla<-subtree(g,o)

    #first person may have been placed at random
    fac <- r/length(o)
    
    #if the first person is in a place that does
    #not violate the proposed PO (given by g) then
    #they may have been placed using the distribution
    #over linear extensions
    
    if (is.extreme.node(mla,as.character(o[1]),extreme = 'top')) {
      #the sub-vsp-tree with the first element removed
      mlb<-subtree(g,o[-1])
      fac<-fac+(1-r)*nle.vsp(mlb)/nle.vsp(mla)
    }
    #return the log-likelihood for this placement
    #plus the log-likelihood for the subsequent placements
    return(log(fac)+lkddown.fac(r,g,o[-1]))
  }
  
  lkdup.fac<-function(r,g,o) {
    
    #evaluate the log-probability for the
    #placement of o[n] - the last person in the order o
    #given the PO with vsp-tree g
    
    #g,o, an order of length 1, just one way to place it
    n = length(o)
    if (n==1) {return(0)}
    
    #the sub-vsp-tree for the order
    mla<-subtree(g,o)

    #last person may have been placed at random
    fac <- r/length(o)
    
    #if the last person is in a place that does
    #not violate the proposed PO (given by g) then
    #they may have been placed using the distribution
    #over linear extensions
    
    if (is.extreme.node(mla,as.character(o[n]),extreme = 'bottom')) {
      #the sub-vsp-tree with the first element removed
      mlb<-subtree(g,o[-n])
      fac<-fac+(1-r)*nle.vsp(mlb)/nle.vsp(mla)
    }
    #return the log-likelihood for this placement
    #plus the log-likelihood for the subsequent placements
    return(log(fac)+lkdup.fac(r,g,o[-n]))
  }

  r = 1/(1+exp(-r)) # define p
  
  if (model=='lkddown') {llkda = unlist(mclapply(Y,lkddown.fac,r=r,g=g, mc.cores = 8))}
  if (model=='lkddown') {llkda = unlist(mclapply(Y,lkdup.fac,r=r,g=g, mc.cores = 8))}
  
  return(sum(llkda))
  
}

logp.vsp <- function(g,psi){
  q = 1/(1+exp(-psi))
  # return(log(TreeWeight(g,q)))
  Pcount=sum(V(g)$type=="P")
  Scount=sum(V(g)$type=="S")

  return(Scount*log(q/2)+Pcount*log(1-q))
}

# tree operations

local_counter <- function(g,v,l){
  
  v_up = as.numeric(neighbors(g,v,mode='in'))
  v_down = setdiff(neighbors(g,v,mode='out'),l)

  edge_ls = E(g)[from(v_up),from(v_down),to(v_up)]
  edge_ls = edge_ls[!sapply(edge_ls, function(e){v==head_of(g,e)})]
  
  counter = length(edge_ls)
  
  return(list(el = edge_ls, counter = counter))

}

reattach_edge <- function(g,e,v,l){
  
  # attaches node v on the target edge e (numerical value)
  
  target_up = tail_of(g,e)
  target_l = head_of(g,e)
  target_down = setdiff(neighbors(g,target_up,mode='out'),target_l)
  g = delete.edges(g,E(g,P=c(as.numeric(target_up),as.numeric(target_l))))
  g = add.edges(g,c(as.numeric(target_up),as.numeric(v),as.numeric(v),as.numeric(target_l)))
  V(g)[v]$order = V(g)[target_l]$order
  V(g)$order[c(as.numeric(target_l), as.numeric(l))] = sample(c('+','-'),2)
  
  return(g)
}

edge_ops <- function(g,e,mode='global'){
  
  v = tail_of(g,e)
  l = head_of(g,e)
  v_up = as.numeric(neighbors(g,v,mode='in'))
  v_down = setdiff(neighbors(g,v,mode='out'),l)
  
  if(mode =='local' & local_counter(g,v,l)$counter==0){
    return(g)
  } else {
    if(mode == 'local'){
      
      local_el = local_counter(g,v,l)$el
      target_edge = local_el[sample(length(local_el),1)]
      
      target_up = as.numeric(tail_of(g,target_edge))
      target_down = as.numeric(head_of(g,target_edge))
    }
    
    # detach v 
    if(length(v_up)==0){
      g= delete.edges(g,E(g,path=c(as.numeric(v),v_down)))
    } else {
      g = delete.edges(g,E(g,P=c(v_up,as.numeric(v),as.numeric(v),v_down)))
      g = add.edges(g,c(v_up,v_down))
    }
    V(g)[v_down]$order = V(g)[v]$order
    
    # reattach v
    if(mode=='global'){
      
      choices = 2*n-1-length(subcomponent(g, as.numeric(v), mode = "out"))
      
      if(choices==1){
        g = add.edges(g,c(as.numeric(v),v_down))
        V(g)$order[c(v_down, as.numeric(l))] = sample(c('+','-'),2)
        
      } else {
        
        s = sample(choices,1)
        root = setdiff(root(g),v)
        if (s == 1){
          # replace the root
          g = add.edges(g,c(as.numeric(v),root))
          V(g)[v]$order = ""
          V(g)$order[c(root, as.numeric(l))] = sample(c('+','-'),2)
          
        } else {
          # add to random edge
          
          el = unique(unlist(incident_edges(g, subcomponent(g, root, mode = "out"), mode = "all")))
          target_edge = el[s-1] # numeric value
          
          g = reattach_edge(g,target_edge,v,l)
        }
      }
    }
    
    if(mode=='local'){
      
      g = reattach_edge(g,E(g, P = c(V(g)[target_up],V(g)[target_down])),v,l)
    }
    
    return(g)
  }
  
}

mcmc.vsp <- function(g,q,p,Y,w=0.5,n.itr=1000,n.record=2*n,chain.index,model){
  
  if(model=='p'){loglik.vsp = loglikP.vsp}
  if(model=='q'){loglik.vsp = loglikQ.vsp}
  if(model=='n'){loglik.vsp = function(g,Y,r){return(0)}}
  
  ptm = proc.time()
  N = length(Y)
  n = (length(V(g))+1)/2
  
  psi = log(q/(1-q))
  r = log(p/(1-p))
  
  g_res = vector(mode= "list", length = n.itr/n.record)
  psi_res = vector(mode="numeric", length = n.itr/n.record)
  r_res = vector(mode="numeric", length = n.itr/n.record)
  loglk_res = vector(mode="numeric", length = (n.itr/n.record))
  
  g_res[[1]]=g
  psi_res[1]=psi
  r_res[1] = r
  loglk = loglik.vsp(g,Y,r); loglk_res[1] = loglk
  
  for(i in 2:n.itr){
    # update on g
    ## update on the tree structure
    
    ### global move = 1
    
    # i = i + 1
    
    e = sample(E(g),1)
    g_temp = edge_ops(g,e,mode='global')
    loglk_temp = loglik.vsp(g = g_temp,Y,r)
    log_eta1 = loglk_temp+logp.vsp(g_temp,psi)-loglk-logp.vsp(g,psi)
    if(log_eta1 > log(runif(1))){g = g_temp; loglk=loglk_temp}
    
    ## local move = n
    for (j in 1:n){
      e = sample(E(g),1)

      v = as.numeric(tail_of(g,e))
      l = as.numeric(head_of(g,e))
      
      if(local_counter(g,V(g)[v],V(g)[l])$counter!=0){
        g_temp = edge_ops(g,e,mode='local')
        loglk_temp = loglik.vsp(g = g_temp,Y,r)
        log_eta5 = loglk_temp+logp.vsp(g_temp,psi)-loglk-logp.vsp(g,psi)+log(local_counter(g,V(g)[v],V(g)[l])$counter)-log(local_counter(g_temp,V(g_temp)[v],V(g_temp)[l])$counter)
        if(log_eta5 > log(runif(1))){g = g_temp; loglk=loglk_temp}
      }
    }
    
    ## update on S&P
    ind = sample(setdiff(1:(2*n-1),which(V(g)$type %in% "L")),1)
    
    g_temp = g
    if (V(g)$type[ind] == "P"){V(g_temp)$type[ind]="S"}
    if (V(g)$type[ind] == "S"){V(g_temp)$type[ind]="P"}
    
    loglk_temp = loglik.vsp(g = g_temp,Y,r)
    log_eta3 = loglk_temp+logp.vsp(g_temp,psi)-loglk-logp.vsp(g,psi)
    if(log_eta3 > log(runif(1))){g = g_temp; loglk=loglk_temp}
    
    # update on q (prior psi ~ norm(0,1))
    psi_temp = rnorm(1,psi,1)
    log_eta2 = logp.vsp(g,psi_temp)+dnorm(psi_temp,0,1,log=TRUE)-logp.vsp(g,psi)-dnorm(psi,0,1,log=TRUE)
    if(log_eta2 > log(runif(1))){psi = psi_temp}

    # update on r if model='q'
    if (model=='q'){
      r_temp = rnorm(1,r,1)
      loglk_temp = loglik.vsp(g = g,Y,r_temp)
      log_eta4 = loglk_temp+dnorm(r_temp,0,1,log=TRUE)-loglk-dnorm(r,0,1,log=TRUE)
      if(log_eta4 > log(runif(1))){r = r_temp}
    }
    
    if (i %% n.record == 0){
      g_res[[i/n.record]] = g
      psi_res[i/n.record] = psi
      r_res[i/n.record] = r
      loglk_res[i/n.record] = loglk
    }
    if (i %% (n.itr/10) == 0){
      print(i)
      Result = list(g=g_res,q=1/(1+exp(-psi_res)),p=1/(1+exp(-r_res)),loglk=loglk_res)
      save(Result, file=paste0(chain.index,".RData"))
    }
  }
  return(list(g=g_res,q=1/(1+exp(-psi_res)),p=1/(1+exp(-r_res)),loglk=loglk_res))
}

