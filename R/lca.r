library(tidyverse)
library(grid)
library(gridExtra)
library(poLCA)

source('~/Box Sync/rcode/plotLCA.r')
source('R/functions.r')

select <- dplyr::select

loadData <- TRUE
if(exists('sdat'))
  if(nrow(sdat)==1705) #&ncol(sdat)==153) ### correct as of 4/23/2020 may need revision
    loadData <- FALSE

if(loadData){
  sdatFile <- 'data/clusteringData1yr2018.RData'
  if(file.exists(sdatFile))  ## the prepared data is available
    if(
      file.info(sdatFile)$mtime>max(
        file.info('R/makeData.r')$mtime,
        file.info('R/readInData.r')$mtime,
        file.info('R/manipulateData.r')$mtime
      )
    ){ ## and up to date
      load(sdatFile)
  } else source('R/makeData.r')
}

lcas <- lapply(1:10,function(k) poLCA(cbind(ageCat,hincCat,married,native,blind,otherDis,hupac,attain,white,sex,inSchool)~1,nclass=k,nrep=20,na.rm=FALSE,verbose=FALSE,calc.se=FALSE,data=lcadat))
save(lcas,file='artifacts/lcasCombined.RData')


ic <- sapply(lcas,function(x)
    c(aic=x$aic,
      caic=x$bic+x$npar,
      bic=x$bic,
      sabic=-2*x$llik+x$npar*log((x$N+2)/24),
      Log.Lik=x$llik))


ic%>%
    rbind(`-2Log.Lik`=-2*ic['Log.Lik',])%>%
    as.data.frame()%>%
        `names<-`(1:10)%>%
            rownames_to_column("index")%>%
                filter(index!='Log.Lik')%>%
                    pivot_longer(-index,"nclass")%>%
                        mutate(nclass=as.numeric(nclass))%>%
                            ggplot(aes(nclass,value,color=index,group=index))+geom_line()+
                                geom_point(data=data.frame(x=apply(ic[1:4,],1,which.min),
                                               y=apply(ic[1:4,],1,min)),
                                           mapping=aes(x,y),size=2,inherit.aes=FALSE)+
                                    scale_x_continuous("# Classes",1:10,labels=1:10)

ggsave('figures/combined/IC.png')

## plot ICs
icPlot <- data.frame(nclass=rep(1:10,3),IC=c(ic[1,],ic[2,],-2*ic[3,]),what=rep(c('AIC','BIC','-2Log.Lik'),each=10))
ggplot(icPlot,aes(nclass,IC,group=what,color=what))+geom_point()+geom_line()


library(xtable)
print(xtable(ic[,1:10],digits=0),type='html',file='tab2.2.html')

plot(ic[1,],ylim=range(ic[-3,]),type='b')
lines(ic[2,],type='b')
for(i in 1:length(lcas)) lcas[[i]]$call$nclass <- i

lca5 <- update(lcas[[5]],nrep=200,nclass=5)
lca5 <- update(lca5,probs.start=poLCA.reorder(lca5$probs.start,order(lca5$P,decreasing=TRUE)),nrep=1)

save(lca5,file='artifacts/lca5rawCombined.RData')

lca6 <- poLCA(cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex,inSchool)~1,nclass=6,nrep=100,na.rm=FALSE,verbose=FALSE,calc.se=TRUE,data=lcadat)

lca6 <- update(lca6,probs.start=poLCA.reorder(lca6$probs.start,order(lca6$P,decreasing=TRUE)),nrep=1)

save(lca6,file='artifacts/lca6rawcombined.RData')

lca7 <- poLCA(cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex,inSchool)~1,nclass=7,nrep=100,na.rm=FALSE,verbose=FALSE,calc.se=TRUE,data=lcadat)

lca7 <- update(lca7,probs.start=poLCA.reorder(lca7$probs.start,order(lca7$P,decreasing=TRUE)),nrep=1)

save(lca7,file='artifacts/lca7rawcombined.RData')


## ###  put them in the order I originally sent to Carrie Lou:
## adjp <- as.vector(crossprod(lca6$posterior,sdat$pwgtp/sum(sdat$pwgtp)))
## intendedP <- c(0.2346, 0.1177, 0.0773,0.3439, 0.1146, 0.1120)
## neword <- sapply(intendedP,function(p) which.min(abs(round(adjp,4)-p)))

## attempts <- lca6$attempts
## pstart <- lca6$probs.start
## pstart <- poLCA.reorder(pstart,neword)
## lca6 <- poLCA(#formula(lca6)
##     cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex,inSchool)~1
##    ,lcadat, nclass=6,na.rm=FALSE,probs.start=pstart,verbose=FALSE,calc.se=TRUE)
## lca6$attempts <- attempts



figures(lca5)



## probabilities in a spreadsheet
openxlsx::write.xlsx(
    list(clusterProportions=propDat[,-ncol(propDat)],
         disability=printProb(fit,c('other','selfCare','blind','indepLiving','ambulatory','cognitive'),vnames=c(other='Deafdisabled',selfCare='Self Care',blind='Blind',indepLiving='Independent Living',ambulatory='Ambulatory',cognitive='Cognitive')),
         demographics=printProb(fit,c('native','white','sex'),c(native='Native Born',white='White',sex='Female')),
         family=printProb(fit,c('sex','married','hupac'),c('Female','Married','KidsAtHome')),
         education=printProb(fit,c('attain','inSchool')),
         age=printProb(fit,'ageCat','age'),
         HHincome=printProb(fit,'hincCat','')),
    row.names=TRUE,
    file='LCAprobabilities.xlsx')

## mean(rowMax(lca5$posterior)>0.8)
## lcasD <- lapply(1:10,function(k) poLCA(cbind(ageCat,hincCat,married,native,hupac,attain,white,sex)~1,nclass=k,nrep=20,na.rm=FALSE,verbose=FALSE,calc.se=FALSE,data=lcadat[!sdat$otherDiss,]))
## sapply(lcasD,function(x) c(aic=x$aic,bic=x$bic))

## odDat <- lcadat[sdat$otherDiss,]
## odDat$cognitive[odDat$selfCare==2 & odDat$blind==2 & odDat$indepLiving==2 & odDat$ambulatory==2] <- NA
## lcasOD <- lapply(1:10,function(k) poLCA(cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex)~1,nclass=k,nrep=20,na.rm=FALSE,verbose=FALSE,calc.se=FALSE,data=odDat))
## sapply(lcasOD,function(x) c(aic=x$aic,bic=x$bic))

## margProb <- function(x) as.vector(table(x))/length(x)

## probsD <- nameCat(lcasD[[3]]$probs)
## makeplot2(probsD,itemNames=itemNames[c(1:4,11:14)],P=c(lcasD[[3]]$P,1))
## ggsave('lcaDeaf.pdf',height=6.5,width=9,units='in')

## probsOD <- lcasOD[[4]]$probs
## cog <- NULL
## cogDiff <- 2-lcadat[sdat$otherDiss,'cognitive']
## numDis <- NULL
## nd <- with(lcadat[sdat$otherDiss,], (selfCare==1) + (blind==1) +(indepLiving==1) +(ambulatory==1) +(cognitive==1))
## for(cc in 1:4){
##     post <- lcasOD[[4]]$posterior[,cc]
##     p <- sum(cogDiff*post)/sum(post)
##     cog <- rbind(cog,c(p,1-p))
##     ps <- sapply(1:5,function(n) sum((nd==n)*post)/sum(post))
##     numDis <- rbind(numDis,ps)
## }

## probsOD$cognitive <- cog
## probsOD <- nameCat(probsOD)

## colnames(numDis) <- 1:5
## numDis <- rbind(numDis,margProb(nd))
## probsOD$other <- numDis
## probsOD <- probsOD[names(probsAll)]

## probsOD <- lapply(names(probsOD),function(x) rbind(probsOD[[x]],margProb(lcadat[sdat$otherDiss,x])))

## itemNames2 <- itemNames
## itemNames2[5] <- '# of\nDisabilities'
## makeplot2(probsOD, itemNames=itemNames2,P=c(lcasOD[[4]]$P,1))
## ggsave('lcaDeafDisabled.pdf',height=6.5,width=9,units='in')


