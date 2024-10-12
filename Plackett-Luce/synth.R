
rm(list=ls())
set.seed(2020)

library(BradleyTerry2)
source("make.data.function.R")

#example with 24 bishops called "a" to "x"
#rank is 1:24 by letter so a is 1, x is 24 etc
#so q was appointed before u, a before b etc

n=24 #number of bishops
bishop.names=letters[1:n]
q=3 #number of levels of bishop-level covariate - diocese - need n/24 to be an integer
diocese.names=LETTERS[1:q]

#dioceses
xf=factor(rep(diocese.names,rep(n/q,q)),levels=diocese.names)
xf
X=model.matrix(~-1+xf)

#basic setup
beta=c(10,12,14); (offsets=beta-beta[1]) #effect due to dio - we only estimate offsets
del=1 #the effect size for the game-level covariate (ie rank)
reps<-1 #all play all reps times so reps*n*(n-1)/2 games

#Example 1: if no random effects all works
re=0*rnorm(n) #the random effects
synth.data<-make.data(n,X,beta,re,del,bishop.names,reps)
summary(synth.model <- BTm(player1 = winner, player2 = loser,
                           formula = ~ rank.ind + diocese[bishop] + (1|bishop), id = "bishop",
                           data = synth.data))
#fair agreement
synth.model$coefficients[1:3]
c(del,offsets[2:q])

#repeat with lots of reps
synth.data<-make.data(n,X,beta,re,del,bishop.names,reps=10)
summary(synth.model <- BTm(player1 = winner, player2 = loser,
                           formula = ~ rank.ind + diocese[bishop] + (1|bishop), id = "bishop",
                           data = synth.data))

#good agreement - try reps=1000 and it nails it
synth.model$coefficients[1:3]
c(del,offsets[2:q])

#Example 2: if random effects colinearity of bishop and diocese is a problem
re=rnorm(n) #the random effects
synth.data<-make.data(n,X,beta,re,del,bishop.names,reps=1)
summary(synth.model <- BTm(player1 = winner, player2 = loser,
                           formula = ~ rank.ind + diocese[bishop] + (1|bishop), id = "bishop",
                           data = synth.data))
#fair agreement
synth.model$coefficients[1:3]
c(del,offsets[2:q])
#but notice std errs much larger

#repeat again lots of reps
synth.data<-make.data(n,X,beta,re,del,bishop.names,reps=100)
summary(synth.model <- BTm(player1 = winner, player2 = loser,
                           formula = ~ rank.ind + diocese[bishop] + (1|bishop), id = "bishop",
                           data = synth.data))

#still not great despite huge data
#colinearity is partly overcome by model (it knows the re's are normal)
#the std err for the fixed effects is not going to zero

#ranef sizes sometimes OK
ere=attr(synth.model$coefficients,"random") #found the little rascals
plot(re,ere,xlab="TRUE",ylab="FITTED",main="random effects")
abline(0,1)

BTabilities(synth.model) #works ok for synthetic data but isnt very useful


###############
#one more experiment more realistic

n=26 #number of bishops
bishop.names=letters[1:n]
q=10 #number of levels of bishop-level covariate - diocese - need n/24 to be an integer
diocese.names=LETTERS[1:q]

#dioceses - this time different numbers of bishops per diocese
xf=factor(sample(diocese.names,n,replace=TRUE),levels=diocese.names)
xf
table(xf) #diocese F isnt used G has just one bishop so badly colinear
X=model.matrix(~-1+xf)

#basic setup
beta=1:q; (offsets=beta-beta[1]) #effect due to dio - we only estimate offsets
del=1 #the effect size for the game-level covariate (ie rank)
reps<-1 #all play all reps times so reps*n*(n-1)/2 games

#Example 3: more realistic, no ranef
synth.data<-make.data(n,X,beta,re=rep(0,n),del,bishop.names,reps=1)
summary(synth.model <- BTm(player1 = winner, player2 = loser,
                           formula = ~ rank.ind + diocese[bishop] + (1|bishop), id = "bishop",
                           data = synth.data))
#fair agreement - sometimes - if you re-run this a few times it screws up sometimes 
#depending on data realisation
synth.model$coefficients[1:q]
c(del,offsets[2:q])
plot(c(del,offsets[2:q]),synth.model$coefficients[1:q],xlab="TRUE",ylab="FITTED",main="fixed effects")
abline(0,1)

#Example 4: more realistic and now with ranef
re<-rnorm(n)
synth.data<-make.data(n,X,beta,re,del,bishop.names,reps=1)
summary(synth.model <- BTm(player1 = winner, player2 = loser,
                           formula = ~ rank.ind + diocese[bishop] + (1|bishop), id = "bishop",
                           data = synth.data))
#fair agreement - mostly - if you repeat this experiment it sometimes gets some coefs very bad
synth.model$coefficients[1:q]
c(del,offsets[2:q])
plot(c(del,offsets[2:q]),synth.model$coefficients[1:q],xlab="TRUE",ylab="FITTED",main="fixed effects")
abline(0,1)

#ranef sizes pretty rough
ere=attr(synth.model$coefficients,"random") #found the little rascals
plot(re,ere,xlab="TRUE",ylab="FITTED",main="random effects")
abline(0,1)
