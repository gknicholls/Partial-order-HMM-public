library(igraph)
library(ape)

is.vsp<-function(tc) {
  # function to detect if a transitive closure is a series-parallel partial order
  tcg<-graph_from_adjacency_matrix(tc,mode ="directed")
  b<-matrix(c(0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0),4,4,byrow=TRUE)
  bg<-graph_from_adjacency_matrix(b,mode ="directed")
  return(!subgraph_isomorphic(bg,tcg,induced=TRUE))
}

showVSP <- function(g){
  vertex_label = V(g)$type
  vertex_label[vertex_label=='L'] = V(g)$name[V(g)$type=='L']
  vertex_label = paste0(vertex_label,V(g)$order)
  plot(g, layout=layout_as_tree,vertex.label = vertex_label)
}

rvsp <- function(n, q=.5){
  # function to generate a random vsp
  # q: the probability for series (S) against parallel (P)
  # n: the number of nodes
  # return: the graph object g
  
  g = as.igraph(rtree(n, rooted = TRUE, br=NULL,tip.label = 1:n, equiprob = TRUE)) # generates binary trees from the uniform distribution over topologies 
  g = set.vertex.attribute(g, 'type', index=sort(V(g)$name), c(rep('L',n),sample(c('S','P'),n-1,replace=TRUE,prob=c(q,1-q))))
  
  dtree = dominator_tree(g, root=1)
  order_ind = rep("", 2*n-1)
  for (i in unique(dtree$dom)[-1]){
    order_ind[dtree$dom==i]=sample(c("+","-"),2)
  }
  
  g = set.vertex.attribute(g, 'order', index=V(dtree$domtree)$name, order_ind)
  showVSP(g)
  return(g)
}

vsp2el <- function(g, el){
  # function to convert a vsp into an edge list
  # el: an edge list (data frame)
  # g: a graph object to represent a vsp
  # return: a data frame
  
  top = which(sapply(sapply(V(g), function(x) neighbors(g,x, mode="in")), length) == 0)
  
  children = neighbors(g, top, mode = "out")
  
  if (V(g)[top]$type=='S'){
    children1 = subcomponent(g, children[which(children$order=='+')], mode ="out")$name
    children1 = children1[!sapply(children1,grepl, pattern="Node")]
    
    children2 = subcomponent(g, children[which(children$order=='-')], mode ="out")$name
    children2 = children2[!sapply(children2,grepl, pattern="Node")]
    
    el=rbind(el,data.frame(from=rep(children1,each=length(children2)),to=rep(children2,length(children1))))
  }
  
  if(children[which(children$order=='+')]$type != 'L'){
    el = vsp2el(subgraph(g,subcomponent(g, children[which(children$order=='+')], mode ="out")),el)
  }
  
  if(children[which(children$order=='-')]$type != 'L'){
    el = vsp2el(subgraph(g,subcomponent(g, children[which(children$order=='-')], mode ="out")),el)
  }
  
  return(el)
}

vsp2po <- function(g){
  nodes = sort(as.numeric(grep("Node", V(g)$name, invert=TRUE, value = TRUE)))
  n = length(nodes)
  el = vsp2el(g,data.frame())
  
  if(nrow(el)==0){
    tr = matrix(0,n,n)
    rownames(tr) = colnames(tr) = nodes
  } else {
    g_po = graph_from_data_frame(el, directed = TRUE, vertices = nodes)
    tr = transitive.reduction(as.matrix(as_adj(g_po)))
  }
  return(tr)
}

nle.vsp <- function(g){
  
  # doesn't require vertex orders
  # need to work together with subtree() -> requires $n.child
  
  root = root(g)
  
  if(V(g)[root]$type=='L'){return(1)}
  
  if(V(g)[root]$type!='L'){
    
    children = neighbors(g, root, mode = "out")
    
    if(V(g)[root]$type=='P'){
      
      n_children1 = V(g)[children[1]]$n.child
      n_children2 = V(g)[children[2]]$n.child
      
      nle_children1 = nle.vsp(subgraph(g,subcomponent(g, children[1], mode ="out")))
      nle_children2 = nle.vsp(subgraph(g,subcomponent(g, children[2], mode ="out")))
      
      nle = nle_children1*nle_children2*choose(n_children1+n_children2,n_children1)
    }
    
    if(V(g)[root]$type=='S'){
      
      nle_children1 = nle.vsp(subgraph(g,subcomponent(g, children[1], mode ="out")))
      nle_children2 = nle.vsp(subgraph(g,subcomponent(g, children[2], mode ="out")))
      
      nle = nle_children1*nle_children2
    }
  }
  
  return(nle)
}

form.type.clusters.new<-function(g,type,r=NA) {
  
  if (is.na(r)) r=V(g)[root(g)]
  
  if (r$type=='L') {return(list())}
  
  c1=neighbors(g,r,mode='out')[1]
  c2=neighbors(g,r,mode='out')[2]
  cl1=form.type.clusters.new(g,type,c1)
  cl2=form.type.clusters.new(g,type,c2)
  
  if (r$type==type) {
    c1.merge=c2.merge=FALSE
    if (length(cl1)>0) {
      tc1=sapply(cl1,function(x) any(x==c1))
      if (any(tc1)) {c1.merge=TRUE; li1=which(tc1)}
    }
    if (length(cl2)>0) {
      tc2=sapply(cl2,function(x) any(x==c2))
      if (any(tc2)) {c2.merge=TRUE; li2=which(tc2)}
    }
    if (!c1.merge & !c2.merge) {clust<-c(cl1,cl2,list(r))}
    if (c1.merge & !c2.merge) {cl1[[li1]]=c(cl1[[li1]],r); clust<-c(cl1,cl2)}
    if (!c1.merge & c2.merge) {cl2[[li2]]=c(cl2[[li2]],r); clust<-c(cl1,cl2)}
    if (c1.merge & c2.merge) {a=cl1[[li1]]; b=cl2[[li2]]; cl1<-cl1[-li1]; cl2<-cl2[-li2]; nc=c(a,b,r); clust<-c(cl1,cl2,list(nc))}
  } else {clust<-c(cl1,cl2)}
  return(clust)   
} 

Ntrees<-function(x) {if (x<3) {return(1)} else { return((2*x-3)*Ntrees(x-1))}} 

Catalan<-function(s) {
  return(choose(2*s,s)/(s+1))
}

TreeWeight<-function(g,q=0.5) {
  
  r=V(g)[root(g)]
  
  Pclust=form.type.clusters.new(g,'P',r)
  Sclust=form.type.clusters.new(g,'S',r)
  
  Pcount=sum(V(g)$type=="P")
  Scount=sum(V(g)$type=="S")
  
  if (length(Pclust)==0) {
    Pf=1
  } else {
    NP=1+sapply(Pclust,length)
    Pf=prod(sapply(NP,Ntrees))
    if (!(Pcount=sum(sapply(Pclust,length)))) stop('error Pcount not matching')
  }
  if (length(Sclust)==0) {
    Sf=1
  } else {
    NS=sapply(Sclust,length)
    Sf= prod(sapply(NS,Catalan)) #Pell(Scount+1) #prod(2^(NS-1))
    if (!(Scount=sum(sapply(Sclust,length)))) stop('error Scount not matching')
  }
  So=1/2^Scount
  Prob=Pf*Sf*So*q^Scount*(1-q)^Pcount/Ntrees( Pcount+Scount+1 )
  return(Prob)
}

# building block functions

root <- function(g){return(which(sapply(sapply(V(g), function(x) neighbors(g,x, mode="in")), length) == 0))}

parent <- function(g,v){return(neighbors(g,v,mode='in'))}

lca <- function(graph, nodes) {
  
  # find the lowest common ancester for nodes
  
  path = ego(graph, order=length(V(graph)), nodes=nodes, mode="in")
  
  max(Reduce(intersect, path))

  }   

is.extreme.node <- function(g,l,extreme='top'){ 
  
  if (extreme=='top') {od='-'} else {od='+'} 
  
  #test if a leaf node of tree is a top or bottom node in the PO 
  ## inputs
  ## l : character, e.g. '1'
  
  if (root(g)==V(g)[l]) return(TRUE) #tree has just one leaf which is also root
  
  if (parent(g,l)$type=='S' & V(g)[l]$order==od) return(FALSE)
  
  v = l
  
  while (length(parent(g,v))>0) {
    v=parent(g,v)
    if (V(g)[v]$order==od){
      if(parent(g,v)$type=='S'){return(FALSE)}
    }   
  }
  
  return(TRUE)
}

subtree <- function(g,y){
  
  # returns a subtree given an incomplete list
  
  node.remover <- function(g,s){
    
    if (length(s)==0) return(g)
    
    l = V(g)[as.character(s[1])]
    v = parent(g,l)
    v_up = as.numeric(neighbors(g,v,mode='in'))
    v_down = setdiff(neighbors(g,v,mode='out'),l)
    
    if(length(v_up)!=0){g = add.edges(g,c(v_up,v_down))}
    V(g)[v_down]$order = V(g)[v]$order
    g = delete_vertices(g, c(as.numeric(l),as.numeric(v)))
    
    return(node.remover(g,s[-1]))
  }
  
  n.child <- function(g,v){return(sum(subcomponent(g, v, mode ="out")$type=='L'))}
  
  n = (length(V(g))+1)/2
  s = setdiff(1:n,y)
  
  g_sub = node.remover(g,s)
  g_sub =set_vertex_attr(g_sub, 'n.child', index = V(g_sub), sapply(V(g_sub),n.child,g=g_sub))
  
  return(g_sub)
}
