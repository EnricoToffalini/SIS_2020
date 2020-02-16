
###############################
###############################

library(lavaan)
library(blavaan)
library(MASS)

###############################
###############################

# CREATE SIMULATED POPULATION
# (from real data on 122 people)

Sigma<-lav_matrix_lower2full(c(
 + 1,
 + .205, 1,
 + -.204, -.390, 1)
)
pop<-data.frame(mvrnorm(n=5e4,mu=rep(0,3),Sigma=Sigma,empirical=T))
colnames(pop)<-c("NEUROT","METACOGN","SLEEP")

# See the "real" model on the population

model<-'
  METACOGN ~ a*NEUROT
  SLEEP ~ b*METACOGN + NEUROT
  indirect := a*b
'
fit<-sem(model=model,data=pop)
summary(fit)

###############################
###############################

# SAMPLE N CASES

N <- 100
d <- pop[sample(1:nrow(pop),N,replace=F),]

###############################

# MAX-LIK MODEL WITH LAVAAN

model<-'
  METACOGN ~ a*NEUROT
  SLEEP ~ b*METACOGN + NEUROT
  indirect := a*b
'
fit<-sem(model=model,data=d)
summary(fit)

#####

# DEFAULT BAYESIAN WITH BLAVAAN

model<-'
  METACOGN ~ NEUROT
  SLEEP ~ METACOGN + NEUROT
'
fit<-bsem(model=model,data=d)
summary(fit)

#####

# PLAUSIBLE/DIRECTIONAL BAYESIAN WITH BLAVAAN

model<-'
  METACOGN ~ prior("normal(.20,.50)")*NEUROT
  SLEEP ~ prior("normal(-.20,.50)")*METACOGN + prior("normal(-.20,.50)")*NEUROT
'
fit<-bsem(model=model,data=d)
summary(fit)

#####

# EXPERT-INFORMED BAYESIAN WITH BLAVAAN

model<-'
  METACOGN ~ prior("normal(.20,.20)")*NEUROT
  SLEEP ~ prior("normal(-.40,.20)")*METACOGN + prior("normal(-.15,.20)")*NEUROT
'
fit<-bsem(model=model,data=d)
summary(fit)

###############################
###############################


