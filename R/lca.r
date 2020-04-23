library(tidyverse)
library(grid)
library(gridExtra)
library(poLCA)

select <- dplyr::select

loadData <- TRUE
if(exists('sdat'))
  if(nrow(sdat)==137934&ncol(sdat)==153) ### correct as of 4/17/2020 may need revision
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

lcas <- lapply(1:10,function(k) poLCA(cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex,inSchool)~1,nclass=k,nrep=20,na.rm=FALSE,verbose=FALSE,calc.se=FALSE,data=lcadat))
save(lcas,file='artifacts/lcas.RData')


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


## plot ICs
icPlot <- data.frame(nclass=rep(1:10,3),IC=c(ic[1,],ic[2,],-2*ic[3,]),what=rep(c('AIC','BIC','-2Log.Lik'),each=10))
ggplot(icPlot,aes(nclass,IC,group=what,color=what))+geom_point()+geom_line()


library(xtable)
print(xtable(ic[,1:10],digits=0),type='html',file='tab2.2.html')

plot(ic[1,],ylim=range(ic[-3,]),type='b')
lines(ic[2,],type='b')
for(i in 1:length(lcas)) lcas[[i]]$call$nclass <- i

#lca5 <- poLCA(cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex)~1,nclass=5,nrep=100,na.rm=FALSE,verbose=FALSE,calc.se=TRUE,data=lcadat)

lca6 <- poLCA(cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex,inSchool)~1,nclass=6,nrep=100,na.rm=FALSE,verbose=FALSE,calc.se=TRUE,data=lcadat)

lca6 <- update(lca6,probs.start=poLCA.reorder(lca6$probs.start,order(lca6$P,decreasing=TRUE)),nrep=1)

save(lca6,file='artifacts/lca6raw.RData')

lca7 <- poLCA(cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex,inSchool)~1,nclass=7,nrep=100,na.rm=FALSE,verbose=FALSE,calc.se=TRUE,data=lcadat)

lca7 <- update(lca7,probs.start=poLCA.reorder(lca7$probs.start,order(lca7$P,decreasing=TRUE)),nrep=1)

save(lca7,file='artifacts/lca7raw.RData')


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





source('~/Box Sync/rcode/plotLCA.r')

nameCat <- function(probs){
    nnn <- names(probs)
    cn <- list(
        ageCat=c('26-30','31-35','36-40'),
        sex=c('Female','Male'),
        white=c('White','POC'),
        attain=c('No HS','HS','Some Col.','Bach+'),
        hincCat=c('1st Qu','2nd Qu','3rd Qu','4th Qu'),
        married=c('Married','Unmarried'),
        native=c('Native','Immigrant'),
        selfCare=c('Diff.','No Diff'),
        blind=c('Diff.','No Diff'),
        indepLiving=c('Diff.','No Diff'),
        ambulatory=c('Diff.','No Diff'),
        cognitive=c('Diff.','No Diff'),
        other=c('Yes','No'),
        inSchool=c('Yes','No'),
        hupac=c('Little','Big','None'))
    for(nn in nnn)
        colnames(probs[[nn]]) <- cn[[nn]]
    names(probs) <- nnn
    probs
}

keepColName <- function(x,cc=1){
    cnames <- colnames(x)
    out <- x[,cc]
    attr(out,"category") <- cnames[cc]
    out
}

pdatFun <- function(fit,varbs,se,vnames=NULL){

  probs <- fit$probs
  for(nn in names(probs)) probs[[nn]] <- rbind(probs[[nn]],Overall=margProb(lcadat[[nn]]))

  probs.se <- fit$probs.se
  for(nn in names(probs.se)){
    probs.se[[nn]] <- rbind(probs.se[[nn]],Overall=0)
    dimnames(probs.se[[nn]]) <- dimnames(probs[[nn]])
  }


    ppp <- if(se) probs.se else probs
    pdat <- ppp[varbs]
    for(i in 1:length(pdat))
        if(ncol(pdat[[i]])==2){
            print(paste('For variable',varbs[i],'keeping category',colnames(pdat[[i]])[1]))
            pdat[[i]] <- pdat[[i]][,1]
        }
    pdat <- as.data.frame(pdat)
    pdat$class <- c(paste('Class', 1:(nrow(pdat)-1)),'Overall')
    pdat <- gather(pdat,'var','x',-class)
    names(pdat)[names(pdat)=='x'] <- ifelse(se,'se','probability')
    if(!is.null(vnames))
        for(i in 1:length(varbs)) pdat$var <- gsub(varbs[i],vnames[i],pdat$var)
    pdat$var <- factor(pdat$var,levels=unique(pdat$var))
    pdat
}

pdat1 <- function(fit,varbs,vnames=NULL){
    probDat <- pdatFun(fit,varbs,se=FALSE,vnames=vnames)
    seDat <- pdatFun(fit,varbs,se=TRUE,vnames=vnames)
    merge(probDat,seDat)
}

pdatTot <- function(fit,varbs,vnames=NULL,calc=NULL,catName){
    pdat <- pdat1(fit,varbs=varbs,vnames=vnames)
    pdat <- within(pdat,{
                         ebmin=rowMax(cbind(probability-2*se,0))
                         ebmax=rowMin(cbind(probability+2*se,1))
                     }
                   )
    pdat$overall <- pdat$class=='Overall'
    if(!is.null(calc)) pdat$overall[pdat$var%in%calc] <- TRUE

    pdat$category <- catName
    pdat
}

plotGroup <- function(fit, varbs,vnames=NULL,calc=NULL,catName='n/a'){

    pdat <- pdatTot(fit,varbs,vnames=vnames,calc=calc,catName=catName)

    ggplot(pdat,aes(var,probability,fill=overall))+geom_col(position='dodge')+
        geom_errorbar(aes(ymin=ebmin,ymax=ebmax),width=0)+
        facet_wrap(~class,nrow=1)+
        theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust=.5),
                  legend.position='none',text=element_text(size=12))+
        xlab(NULL)+
        scale_y_continuous(name=NULL,labels=scales::percent,limits=c(0,1))

}


figures <- function(fit){

    nclass <- length(fit$P)
    if(!is.element(paste0('figures/model',nclass,'class'),list.dirs('figures')))
        dir.create(paste0('figures/model',nclass,'class'))

    probsBig <- fit$probs
    oth <- sdat[,'otherDiss']
    nd <- NULL
    numDiss <- rowSums(lcadat[,c('selfCare','blind','indepLiving','ambulatory','cognitive')]==1)

    for(cc in 1:nclass){
        post <- fit$posterior[,cc]
        n <- sum(numDiss*post)/sum(post)
        nd <- c(nd,n)
    }
    fit <- update(fit,probs.start=poLCA.reorder(fit$probs.start,order(nd,decreasing=TRUE)),nrep=1)

    ad <- NULL
    for(cc in 1:nclass){
        post <- fit$posterior[,cc]
        p <- sum(oth*post)/sum(post)
        ad <- rbind(ad,c(p,1-p))
    }

    probsBig$other <- ad
    fit$probs$other <- ad



  fit$probs <- fit$probs[c(1:4,15,5:14)]
  fit$probs <- nameCat(fit$probs) #probsAll <- nameCat(probsAll)

  fit$probs.se$other <- matrix(0,nrow(ad),ncol(ad))
  fit$probs.se <- fit$probs.se[c(1:4,15,5:14)]


  itemNames <- c('Age','Income\nQuartile','Marital\nStatus','Immig.','Deafdisabled','Self\n-Care','Vision','Indep.\nLiving','Amb.','Cog','Kids at\nHome','Ed.\nAttain.','Race','Sex','In\nSchool')

  porig <- fit$P
  fit$P <- crossprod(fit$posterior,sdat$pwgtp/sum(sdat$pwgtp))#/nrow(sdat)

  prep <-
    sapply(1:80,
      function(x)
        crossprod(sdat[[paste0('pwgtp',x)]],fit$posterior)/sum(sdat[[paste0('pwgtp',x)]]))

  P.seOrig <- fit$P.se

  fit$P.se <- sqrt(apply(sweep(prep,1,fit$P),1,function(x) mean(x^2)*4)+
                     fit$P.se^2)

  save(fit,file=paste0('artifacts/lca',nclass,'.RData'))

  lcadat$other <- ifelse(sdat$otherDiss, 'Yes','No')
  makeplot2(fit,NULL,itemNames, calc='other', include.margin=TRUE,data=lcadat)
  ggsave(paste0('figures/model',nclass,'class/pooledCluster.png'),height=6.5,width=9,units='in')



## plot disabilities
      plotGroup(fit,varbs=c('other','selfCare','blind','indepLiving','ambulatory','cognitive'),
                vnames=c(other='Deafdisabled',selfCare='Self Care',blind='Blind',indepLiving='Independent Living',ambulatory='Ambulatory',cognitive='Cognitive'),
                calc='Deafdisabled',catName='Disabilities')
      ggsave(paste0('figures/model',nclass,'class/disability.png'),width=6.5,height=3)

      ggplot(mapping=aes(x=1:7,y=sort(nd,decreasing=TRUE)))+geom_col()+labs(title='Avg. #\nDisabilities',x='Class',y=NULL)+scale_x_continuous(breaks=1:nclass,minor_breaks=NULL)
      ggsave(paste0('figures/model',nclass,'class/numdisability.png'),width=1.5,height=3)

  ## plot demographics
  plotGroup(fit,varbs=c('native','white','sex'),
    vnames=c(native='Native Born',white='White',sex='Female'),catName='Demographics')

  ggsave(paste0('figures/model',nclass,'class/demographic.png'),width=6.5,height=3)

  ### plot family
  plotGroup(fit,varbs=c('sex','married','hupac'),catName='Family')+
    scale_x_discrete(labels=c('Female','Married','Kids at Home (<6yo)','Kids at Home (all >5yo)',
      'No Kids at Home'))
  ggsave(paste0('figures/model',nclass,'class/family.png'),width=6.5,height=3)


  #### education
  plotGroup(fit,varbs=c('attain','inSchool'),catName='Education')+
    scale_x_discrete(labels=c('<HS','HS','Some College','BA+','Enrolled'))

  ggsave(paste0('figures/model',nclass,'class/education.png'),width=6.5,height=3)

  ### age
  plotGroup(fit,'ageCat')+
    scale_x_discrete(labels=c('25-30','31-35','36-40'))+
    ggtitle('Age')

  ggsave(paste0('figures/model',nclass,'class/age.png'),width=6.5,height=3)

  ##
  plotGroup(fit,'hincCat')+scale_x_discrete(labels=colnames(probs$hincCat))+
    ggtitle('Household Income Quartile')
  ggsave(paste0('figures/model',nclass,'class/income.png'),width=6.5,height=3)



  ## plot proportions
  propDat <- data.frame(Class=factor(1:nclass,levels=nclass:1),Proportion=fit$P,SE=fit$P.se)
  propDat$label_pos <- cumsum(propDat$Proportion)-propDat$Proportion/2
  ggplot(propDat,aes(1,Proportion,fill=Class,label=Class))+geom_col()+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    geom_text(aes(1,label_pos))+coord_flip()+scale_fill_manual(values=subwayPalette,guide='none')+ylab(NULL)
  ggsave(paste0("figures/model",nclass,'class/ClusterProportions.png'),width=3,height=1)
}

printProb <- function(fit,varbs,vnames=NULL){
    ddd <- pdat1(fit,varbs,vnames)
    ests <- spread(subset(ddd,select=-se),var,probability)%>%column_to_rownames("class")
    ses <- spread(subset(ddd,select=-probability),var,se)%>%column_to_rownames("class")
    out <- matrix(nrow=nrow(ests),ncol=ncol(ests),dimnames=dimnames(ests))
    for(i in 1:nrow(ests)) for(j in 1:ncol(ests)) out[i,j] <-
        ifelse(all(ses[i,]==0)|all(ses[,j]==0),round(ests[i,j]*100,1),
               paste0(round(ests[i,j]*100,1),' (',round(ses[i,j]*100,1),')'))
    out
}

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


