library(grid)
library(gridExtra)
library(tidyr)
library(ggplot2)
library(dplyr)
library(poLCA)

load('clusteringData1yr.RData')
## note: is.na(hincp) only true for group quarters
sdat2 <- subset(sdat,agep<41 & agep>=25 & !is.na(hincp)& esr==6 )#& inSchool==0)

sdat2 <- droplevels(sdat2)

levels(sdat2$hupac) <- c('0-6','6-17','0-6','noKids')

sdat2$native <- sdat2$citizenship=='native'

sdat2$married <- sdat2$married=='married'

sdat2$white <- sdat2$raceEth=='White'

dis <- c('ddrs','deye','dout','dphy','drem')
sdat2$ageCat <- cut(sdat2$agep,c(24,30,35,40),include.lowest=TRUE,labels=FALSE)

sdat2$hincCat <- cut(sdat2$hincp,quantile(sdat2$hincp,na.rm=TRUE),include.lowest=TRUE,labels=FALSE)

lcadat <- model.frame(~ ageCat + married + native + ddrs+deye+dout+dphy+drem+
                          hupac + hincCat + attain + white+sex,data=sdat2)

lcadat <- lcadat%>%rename(selfCare=ddrs,blind=deye,indepLiving=dout,ambulatory=dphy,cognitive=drem)


for(i in 1:ncol(lcadat)){
    if(is.factor(lcadat[[i]])) lcadat[[i]] <- as.numeric(lcadat[[i]])
    if(is.logical(lcadat[[i]])) lcadat[[i]] <- 2-lcadat[[i]]
}

lcadat$inSchool <- 2-sdat2$inSchool

lcas <- lapply(1:10,function(k) poLCA(cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex,inSchool)~1,nclass=k,nrep=20,na.rm=FALSE,verbose=FALSE,calc.se=FALSE,data=lcadat))
save(lcas,file='lcas.RData')


ic <- sapply(lcas,function(x) c(aic=x$aic,bic=x$bic,Log.Lik=x$llik))

## plot ICs
icPlot <- data.frame(nclass=rep(1:10,3),IC=c(ic[1,],ic[2,],-2*ic[3,]),what=rep(c('AIC','BIC','-2Log.Lik'),each=10))
ggplot(icPlot,aes(nclass,IC,group=what,color=what))+geom_point()+geom_line()


library(xtable)
print(xtable(ic[,1:10],digits=0),type='html',file='tab2.2.html')

plot(ic[1,],ylim=range(ic[-3,]),type='b')
lines(ic[2,],type='b')
for(i in 1:length(lcas)) lcas[[i]]$call$nclass <- i

lca5 <- poLCA(cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex)~1,nclass=5,nrep=100,na.rm=FALSE,verbose=FALSE,calc.se=TRUE,data=lcadat)

lca6 <- poLCA(cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex,inSchool)~1,nclass=6,nrep=100,na.rm=FALSE,verbose=FALSE,calc.se=TRUE,data=lcadat)

###  put them in the order I originally sent to Carrie Lou:
adjp <- as.vector(crossprod(lca6$posterior,sdat2$pwgtp/sum(sdat2$pwgtp)))
intendedP <- c(0.2346, 0.1177, 0.0773,0.3439, 0.1146, 0.1120)
neword <- sapply(intendedP,function(p) which.min(abs(round(adjp,4)-p)))

attempts <- lca6$attempts
pstart <- lca6$probs.start
pstart <- poLCA.reorder(pstart,neword)
lca6 <- poLCA(#formula(lca6)
    cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex,inSchool)~1
   ,lcadat, nclass=6,na.rm=FALSE,probs.start=pstart,verbose=FALSE,calc.se=TRUE)
lca6$attempts <- attempts


probsBig <- lca6$probs
ad <- NULL
oth <- sdat2[,'otherDiss']
for(cc in 1:6){
    post <- lca6$posterior[,cc]
    p <- sum(oth*post)/sum(post)
    ad <- rbind(ad,c(p,1-p))
}
probsBig$other <- ad





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


lca6$probs$other <- ad
lca6$probs <- lca6$probs[c(1:4,15,5:14)]
lca6$probs <- nameCat(lca6$probs) #probsAll <- nameCat(probsAll)

lca6$probs.se$other <- matrix(0,nrow(ad),ncol(ad))
lca6$probs.se <- lca6$probs.se[c(1:4,15,5:14)]


itemNames <- c('Age','Income\nQuartile','Marital\nStatus','Immig.','Deafdisabled','Self\n-Care','Vision','Indep.\nLiving','Amb.','Cog','Kids at\nHome','Ed.\nAttain.','Race','Sex','In\nSchool')

porig <- lca6$P
lca6$P <- crossprod(lca6$posterior,sdat2$pwgtp/sum(sdat2$pwgtp))#/nrow(sdat2)

prep <-
    sapply(1:80,
           function(x)
               crossprod(sdat2[[paste0('pwgtp',x)]],lca6$posterior)/sum(sdat2[[paste0('pwgtp',x)]]))

P.seOrig <- lca6$P.se

lca6$P.se <- sqrt(apply(sweep(prep,1,lca6$P),1,function(x) mean(x^2)*4)+
                      lca6$P.se^2)

save(lca6,file='lca6.RData')

lcadat$other <- ifelse(sdat2$otherDiss, 'Yes','No')
makeplot2(lca6,NULL,itemNames, calc='other', include.margin=TRUE,data=lcadat)
ggsave('pooledCluster6.png',height=6.5,width=9,units='in')

#### plot groups of variables
probs <- lca6$probs
for(nn in names(probs)) probs[[nn]] <- rbind(probs[[nn]],Overall=margProb(lcadat[[nn]]))

probs.se <- lca6$probs.se
for(nn in names(probs.se)){
    probs.se[[nn]] <- rbind(probs.se[[nn]],Overall=0)
    dimnames(probs.se[[nn]]) <- dimnames(probs[[nn]])
}


keepColName <- function(x,cc=1){
    cnames <- colnames(x)
    out <- x[,cc]
    attr(out,"category") <- cnames[cc]
    out
}

pdatFun <- function(varbs,se,vnames=NULL){
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

pdat1 <- function(varbs,vnames=NULL){
    probDat <- pdatFun(varbs,se=FALSE,vnames=vnames)
    seDat <- pdatFun(varbs,se=TRUE,vnames=vnames)
    merge(probDat,seDat)
}

pdatTot <- function(varbs,vnames=NULL,calc=NULL,catName){
    pdat <- pdat1(varbs=varbs,vnames=vnames)
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

plotGroup <- function(varbs,vnames=NULL,calc=NULL,catName='n/a'){

    pdat <- pdatTot(varbs,vnames=vnames,calc=calc,catName=catName)

    ggplot(pdat,aes(var,probability,fill=overall))+geom_col(position='dodge')+
        geom_errorbar(aes(ymin=ebmin,ymax=ebmax),width=0)+
        facet_wrap(~class,nrow=1)+
        theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust=.5),
                  legend.position='none',text=element_text(size=12))+
        xlab(NULL)+
        scale_y_continuous(name=NULL,labels=scales::percent,limits=c(0,1))

}




## disDat <- pdatTot(c('other','selfCare','blind','indepLiving','ambulatory','cognitive'),catName='disability')

## edDat <- pdatTot(c('attain','inSchool'),catName='education')
## ageDat <- pdatTot('ageCat',catName='age')

## pdat <- rbind(disDat,edDat,ageDat)

## ggplot(pdat,aes(var,probability,fill=overall))+geom_col(position='dodge')+
##         geom_errorbar(aes(ymin=ebmin,ymax=ebmax),width=0)+
##         facet_grid(category~class,scales="free")+
##         theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust=.5),
##                   legend.position='none',text=element_text(size=12))+
##         xlab(NULL)+
##         scale_y_continuous(name=NULL,labels=scales::percent,limits=c(0,1))


## plot disabilities
plotGroup(varbs=c('other','selfCare','blind','indepLiving','ambulatory','cognitive'),
          vnames=c(other='Deafdisabled',selfCare='Self Care',blind='Blind',indepLiving='Independent Living',ambulatory='Ambulatory',cognitive='Cognitive'),
          calc='Deafdisabled',catName='Disabilities')

ggsave('disability.png',width=6.5,height=3)

## plot demographics
plotGroup(varbs=c('native','white','sex'),
          vnames=c(native='Native Born',white='White',sex='Female'),catName='Demographics')

ggsave('demographic.png',width=6.5,height=3)

### plot family
plotGroup(varbs=c('sex','married','hupac'))+
    scale_x_discrete(labels=c('Female','Married','Kids at Home (<6yo)','Kids at Home (all >5yo)',
                         'No Kids at Home'),catName='Family')
ggsave('family.png',width=6.5,height=3)


#### education
plotGroup(varbs=c('attain','inSchool'),catName='Education')+
    scale_x_discrete(labels=c('<HS','HS','Some College','BA+','Enrolled'))

ggsave('education.png',width=6.5,height=3)

### age
plotGroup('ageCat')+
    scale_x_discrete(labels=c('25-30','31-35','36-40'))+
    ggtitle('Age')

ggsave('age.png',width=6.5,height=3)

##
plotGroup('hincCat')+scale_x_discrete(labels=colnames(probs$hincCat))+
    ggtitle('Household Income Quartile')
ggsave('income.png',width=6.5,height=3)



## plot proportions
propDat <- data.frame(Class=factor(1:6,levels=6:1),Proportion=lca6$P,SE=lca6$P.se)
propDat$label_pos <- cumsum(propDat$Proportion)-propDat$Proportion/2
ggplot(propDat,aes(1,Proportion,fill=Class,label=Class))+geom_col()+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
        geom_text(aes(1,label_pos))+coord_flip()+scale_fill_manual(values=subwayPalette,guide='none')+ylab(NULL)
ggsave("ClusterProportions.png",width=3,height=1)

printProb <- function(varbs,vnames=NULL){
    ddd <- pdat1(varbs,vnames)
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
         disability=printProb(c('other','selfCare','blind','indepLiving','ambulatory','cognitive'),vnames=c(other='Deafdisabled',selfCare='Self Care',blind='Blind',indepLiving='Independent Living',ambulatory='Ambulatory',cognitive='Cognitive')),
         demographics=printProb(c('native','white','sex'),c(native='Native Born',white='White',sex='Female')),
         family=printProb(c('sex','married','hupac'),c('Female','Married','KidsAtHome')),
         education=printProb(c('attain','inSchool')),
         age=printProb('ageCat','age'),
         HHincome=printProb('hincCat','')),
    row.names=TRUE,
    file='LCAprobabilities.xlsx')

## mean(rowMax(lca5$posterior)>0.8)
## lcasD <- lapply(1:10,function(k) poLCA(cbind(ageCat,hincCat,married,native,hupac,attain,white,sex)~1,nclass=k,nrep=20,na.rm=FALSE,verbose=FALSE,calc.se=FALSE,data=lcadat[!sdat2$otherDiss,]))
## sapply(lcasD,function(x) c(aic=x$aic,bic=x$bic))

## odDat <- lcadat[sdat2$otherDiss,]
## odDat$cognitive[odDat$selfCare==2 & odDat$blind==2 & odDat$indepLiving==2 & odDat$ambulatory==2] <- NA
## lcasOD <- lapply(1:10,function(k) poLCA(cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex)~1,nclass=k,nrep=20,na.rm=FALSE,verbose=FALSE,calc.se=FALSE,data=odDat))
## sapply(lcasOD,function(x) c(aic=x$aic,bic=x$bic))

## margProb <- function(x) as.vector(table(x))/length(x)

## probsD <- nameCat(lcasD[[3]]$probs)
## makeplot2(probsD,itemNames=itemNames[c(1:4,11:14)],P=c(lcasD[[3]]$P,1))
## ggsave('lcaDeaf.pdf',height=6.5,width=9,units='in')

## probsOD <- lcasOD[[4]]$probs
## cog <- NULL
## cogDiff <- 2-lcadat[sdat2$otherDiss,'cognitive']
## numDis <- NULL
## nd <- with(lcadat[sdat2$otherDiss,], (selfCare==1) + (blind==1) +(indepLiving==1) +(ambulatory==1) +(cognitive==1))
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

## probsOD <- lapply(names(probsOD),function(x) rbind(probsOD[[x]],margProb(lcadat[sdat2$otherDiss,x])))

## itemNames2 <- itemNames
## itemNames2[5] <- '# of\nDisabilities'
## makeplot2(probsOD, itemNames=itemNames2,P=c(lcasOD[[4]]$P,1))
## ggsave('lcaDeafDisabled.pdf',height=6.5,width=9,units='in')


