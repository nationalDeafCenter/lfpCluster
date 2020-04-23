library(poLCA)
library(tidyverse)

print(load('archive2016/artifacts/lca6.RData'))
lcaOld <- lca6

load('artifacts/lca6raw.RData')
load('artifacts/lca7raw.RData')

## how would subjects in modOut be classified acc to modFitted?
## (assumes prior from modOut)
outOfSampleClassProbs <- function(modFitted,modOut){
   ## class probabilities for trad students classified acc to alt model
    modFitted$probs$other <- NULL

    stopifnot(all(names(modFitted)==names(modOut)))
    vp <- poLCA:::poLCA.vectorize(modFitted$probs)

  ## need Stand "prior"
  prior <- matrix(rep(modOut$P,modOut$N),modOut$N,byrow=TRUE)

### need matrix of responses
  y <- as.matrix(modOut$y)
  y[is.na(y)] <- 0

  poLCA:::poLCA.postClass.C(prior, vp, y)
}


oldModFitted6 <- outOfSampleClassProbs(lcaOld,lca6)
oldModFitted7 <- outOfSampleClassProbs(lcaOld,lca7)



comp <- tibble(
  new6=lca6$predclass,
  new7=lca7$predclass,
    old6=apply(oldModFitted6,1,which.max),
      old7=apply(oldModFitted7,1,which.max)
)%>%
  mutate(id=1:n())

### compare two new models:
xtabs(~new7+new6,comp)

### compare 2016 model to lca6:
tab66 <- xtabs(~old6+new6,comp)

tab76 <- xtabs(~old7+new7,comp)

tab66a <- round(sweep(tab66,1,rowSums(tab66),'/')*100)
tab66b <-round(sweep(tab66,2,colSums(tab66),'/')*100)


tab76a <- round(sweep(tab76,1,rowSums(tab76),'/')*100)
tab76b <- round(sweep(tab76,2,colSums(tab76),'/')*100)

tab76c <- matrix(nrow=6,ncol=7)
for(i in 1:6) for(j in 1:7) tab76c[i,j] <- ifelse(tab76a[i,j]>=20|tab76b[i,j]>=20,1,0)


plot(c(rep(1,6),rep(2,7)),c(1:6,1:7),pch='n')

omar <- par('mar')

par(mar=c(5.1,.1,4,.1))
plot(c(0, 1), c(0, 1), type = 'n', xaxt = 'n', yaxt = 'n')
text(c(rep(.2,6),rep(.8,7)),c(seq(6)/6,seq(7)/7),labels=c(1:6,1:7))
for(i in 1:6) for(j in 1:7) if(tab76c[i,j]==1) lines(c(.2,.8),c(i/6,j/7))


par(mfrow=c(1,2))
plot(c(0, 1), c(0, 1), ann = F, type = 'n', xaxt = 'n', yaxt = 'n')
text(c(rep(.2,6),rep(.8,7)),c(seq(0,5)/5,seq(0,6)/6),labels=c(1:6,1:7))
for(i in 1:6) for(j in 1:7) if(tab76a[i,j]>=20){
    lines(c(.2,.8),c((i-1)/5,(j-1)/6))
    text(.28,((j-1)/6-(i-1)/5)/.6*.08+(i-1)/5,tab76a[i,j])
}
axis(1,at=c(0.2,0.8),labels=c('2016 cluster','2018 cluster'))

plot(c(0, 1), c(0, 1), ann = F, type = 'n', xaxt = 'n', yaxt = 'n')
text(c(rep(.2,6),rep(.8,7)),c(seq(6)/6,seq(7)/7),labels=c(1:6,1:7))
for(i in 1:6) for(j in 1:7) if(tab76b[i,j]>=20){
    lines(c(.2,.8),c(i/6,j/7))
    text(.72,(j/7-i/6)/.6*.52+i/6,tab76b[i,j])
}
axis(1,at=c(0.2,0.8),labels=c('2016 cluster','2018 cluster'))
