remove(list=ls())
library(lavaan) ; library(semPlot)


# figure1 SIS

# Lambda matrices:
# <- rbind(diag(1,2,2),diag(1,2,2),diag(1,2,2))
Loadings <-matrix(c(1,1,1,0,0,0,0,0,0,1,1,1),ncol=2)

# Phi and Psi matrices:
LatVar2 <- diag(1,2,2)
LatVar1 <- matrix(c(1,1,1,1),2,2)

# Beta matrix:
Beta <- matrix(0,2,2)
Beta[2,1] <- 1

# Theta matrices:
ManVar <- diag(1,nrow(Loadings),nrow(Loadings))

# Gamma matrix:
Gamma <- matrix(c(1,1,0,1),2,2)

# Tau matrices:
ManInts <- rep(1,6)

# Alpha and Kappa matrices:
LatInts <- rep(1,2)

# Combine model:
mod <- lisrelModel(LY=Loadings,PS=LatVar2,BE=Beta,TE=ManVar,
                   LX=Loadings,PH=LatVar1,GA=Gamma,TD=ManVar,
                   TY=TRUE,TX=ManInts,AL=LatInts,KA=LatInts)


######### prove             
semPaths(mod, as.expression=c("nodes","edges"),intercepts=FALSE,style="LISREL",mar=c(3,3,3,3),
         edge.label.cex=.9,edge.color="black",whatLabels="omit",residuals = FALSE) 
rect(-.6,-.6,.6,.6,lty=2,lwd=1.5)

# prova 2
semPaths(mod, as.expression=c("nodes","edges"),intercepts=FALSE,style="LISREL",mar=c(3,3,3,3),
         edge.color="black",whatLabels="omit",residuals = FALSE,label.cex=3) 
rect(-.6,-.6,.6,.6,lty=2,lwd=1.5)
###############

######### Figure (al limite vedi ?pdf per modificare altezza e larghezza)
pdf("figure1.pdf")
semPaths(mod, as.expression=c("nodes","edges"),intercepts=FALSE,style="LISREL",mar=c(3,3,3,3),
         edge.color="black",whatLabels="omit",residuals = FALSE,label.cex=2) 
rect(-.6,-.6,.6,.6,lty=2,lwd=1.5)
dev.off()


