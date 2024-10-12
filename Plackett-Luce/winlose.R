
#################################################################
# Week 1

# Start from "cla" and "cil"
# "cla" and "cil" and V$who, V$tio and V$drk are probably the main things you need

# "cla" - exploratory:
# lists of witness lists within time period 1115-1145 AD
# each with arguments:
# $w - label assigned to witnesses??
# $id - id of list
# $year.1 - earliest year dated, time lower bound
# $year.2 - latest year dated, time upper bound
# $ac - accuracy on a scale of 0-5 (unreliable-reliable)
# $names - names of BISHOPS appear on the list
# $o - order of $names in the list, in the form of $node.name

# "cil" - exploratory:
# lists of bishops with their personal info
# each with arguments:
# $name.id - id under Dr Johnson's classification, I assume?
# $node.name - Prof Nicholls's relabelled name id?
# $name - name of the bishop
# $diocese - diocese of the bishop
# etc.

# "V" - exploratory:
# $who - who's ($node.name) at which diocese between 1115-1145 AD
# $tio - classified by diocese, each bishop at post had been a bishop for $tio years
# $drk - rank of $tio by diocese

# Goal: (Re)Process data to fit in the Bradley-Terry Model

##############################################################################################
# Week 2-3: Do the work! Or what else do you want?! Think straight, you idiot!

# formula = ~ at.home + rank + (1|bishop) + D1 [bishop] + D2 [bishop] + ...
# BTm(outcome = 1, player1, player2, formula = NULL, id = "..", 
#   separate.ability = NULL, refcat = NULL, family = "binomial",
#   data = NULL, weights = NULL, subset = NULL, na.action = NULL,
#   start = NULL, etastart = NULL, mustart = NULL, offset = NULL,
#   br = FALSE, model = TRUE, x = FALSE, contrasts = NULL, ...)

# We will have:
# B1 (data.frame) - winner bishop
# B2 (data.frame) - loser bishop

##############################################################################################

# start with a for loop on cla and get two lists of node names for winner and loser bishops

# aid function: count how many pairs there are - this cuz I'm stupid :'(

count_o_pairs <- function(cla) {
  n <- 0
  for (i in 1:length(cla)) {
    l <- length(cla[[i]]$o)
    if (l>1) {
      n <- n + l*(l-1)/2
    }
  }
  return(n)
}

count_o_pairs(cla)

get_winner_loser <- function(cla) {
  winner <- numeric(916)
  loser <- numeric(916)
  meeting.yr1 <- numeric(916)
  meeting.yr2 <- numeric(916)
  p <- 1
  for (i in 1:length(cla)) {
    l <- length(cla[[i]]$o)
    if (l>1) {
      for (j in 1:l) {
        k <- j+1
        while (k<l+1) {
          winner[p] <- cla[[i]]$o[j]
          loser[p] <- cla[[i]]$o[k]
          meeting.yr1[p] <- cla[[i]]$year.1
          meeting.yr2[p] <- cla[[i]]$year.2
          k <- k+1
          p <- p+1
        }
      }
    }
  }
  return(list(winner, loser, meeting.yr1, meeting.yr2))
}

winner.node.name <- get_winner_loser(cla) [[1]]
loser.node.name <- get_winner_loser(cla) [[2]]
meeting.year.1 <- get_winner_loser(cla) [[3]]
meeting.year.2 <- get_winner_loser(cla) [[4]]

meeting.year <- ceiling(0.5*(meeting.year.1+meeting.year.2))

head(meeting.year)

# get the names of winner and loser bishops
get_name <- function(x) {
  bishop.name <- character(length = length(x))
  for (i in 1:length(x)) {
    node.name <- x[i]
    bishop.name[i] <- cil[[node.name]]$name
  }
  return(bishop.name)
}

winner.name <- get_name(winner.node.name)
loser.name <- get_name(loser.node.name)

head(winner.name)
head(loser.name)

# get the diocese of winner and loser bishops
get_diocese <- function(x) {
  bishop.diocese <- character(length = length(x))
  for (i in 1:length(x)) {
    node.name <- x[i]
    bishop.diocese[i] <- cil[[node.name]]$diocese
  }
  return(bishop.diocese)
}

winner.diocese <- get_diocese(winner.node.name)
loser.diocese <- get_diocese(loser.node.name)

head(winner.diocese)
head(loser.diocese)

# get the diocese indices of winner and loser bishops
get_di <- function(x) {
  bishop.di <- numeric(length = length(x))
  for (i in 1:length(x)) {
    node.name <- x[i]
    bishop.di[i] <- cil[[node.name]]$di
  }
  return(bishop.di)
}

winner.di <- get_di(winner.node.name)
loser.di <- get_di(loser.node.name)

head(winner.di)
head(loser.di)

# get the rank as an indicator function for winner and loser bishops
get_rank_ind <- function(win.di, lose.di, meeting.yr) {
  win.rank.ind <- numeric(length = length(win.di))
  lose.rank.ind <- numeric(length = length(lose.di))
  for (i in 1:length(win.di)) {
    rwin <- win.di[i]
    rlose <- lose.di[i]
    cwin <- meeting.yr[i]
    if (cwin<1115) 
      cwin <- 1
    else
      cwin <- cwin-1114
    win.rank <- V$drk[rwin, cwin]
    lose.rank <- V$drk[rlose, cwin]
    df <- win.rank-lose.rank
    if (df<0) {
      win.rank.ind[i] <- 1
      lose.rank.ind[i] <- 0
    }
    if (df>0) {
      win.rank.ind[i] <- 0
      lose.rank.ind[i] <- 1
    }
  }
  return(list(win.rank.ind, lose.rank.ind))
}

winner.rank.ind <- get_rank_ind(winner.di, loser.di, meeting.year) [[1]]
loser.rank.ind <- get_rank_ind(winner.di, loser.di, meeting.year) [[2]]

head(winner.rank.ind)
head(loser.rank.ind)

#######################################################################
# Now, despite at.home, we have for each winner and loser bishop:
# node.name
# name
# diocese
# di
# rank.ind (very important!)

# And:
# meeting.year - note: I remain questioning on the validity/credibility of the determining process for this
# NEED DISCUSSION! 
# meeting.year.1
# meeting.year.2

# apply data.frame() to get data frames for winner and loser bishops
# Did the chameleon study involve an at.home variable??
#######################################################################

Winner <- data.frame(node.name=winner.node.name, name=winner.name, 
                     di=winner.di, diocese=winner.diocese, 
                     rank.ind=winner.rank.ind)
Loser <- data.frame(node.name=loser.node.name, name=loser.name, 
                    di=loser.di, diocese=loser.diocese, 
                    rank.ind=loser.rank.ind)

# Now that we have two data frames, we try to fit a Bradley-Terry model as in the Chameleon case
# Moment of revelation :o
# didn't really work, we have to further frame X into a form more similar to chameleons
# need a $predictor data.frame - looks like a dictionary
# we'll try to get it from cil

get_pred <- function(cil) {
  rname <- character(length = length(cil))
  dioc <- character(length = length(cil))
  for (i in 1:length(cil)) {
    rname[i] <- cil[[i]]$name
    dioc[i] <- cil[[i]]$diocese
  }
  return(list(rname, dioc))
}

pred.rnames <- get_pred(cil) [[1]]
pred.diocese <- get_pred(cil) [[2]]

Predictors <- data.frame(diocese2 = pred.diocese, row.names = pred.rnames)
dioc <- Predictors$diocese

# new.diocese <- model.matrix(~ -1 + dioc)
# Fred <- gsub("dioc", "", colnames(new.diocese))
# Fred <- paste(Fred, "[name]", sep = "")
# form.dio <- paste("~ rank.ind + (1|name) + ", paste(Fred, 
#                                               collapse ="+"), collapse = "")

X <- list(winner=Winner, loser=Loser, predictors=Predictors)

a=levels(X$winner$name)
b=levels(X$loser$name)
new.a=c(a,setdiff(b,a))

new.b=c(b,setdiff(a,b))
X$winner$name <- factor(X$winner$name, levels = new.a)

X$loser$name <- factor(X$loser$name, levels = new.a)

library(BradleyTerry2)
bishop.model <- BTm(player1 = winner, player2 = loser,
                    formula = ~ rank.ind+diocese2[name],#as.formula(form.dio), 
                    id="name", data = X)
summary(bishop.model)

bishop.model2 <- BTm(player1 = winner, player2 = loser,
                    formula = ~ (1|name),#as.formula(form.dio), 
                    id="name", data = X)
summary(bishop.model2)

anova(bishop.model,bishop.model2)

Estimate[bishop i]~diocese[bishop i]