library(grid)
library(gridExtra)
library(tidyr)
library(ggplot2)
library(dplyr)
library(poLCA)

load('data/clusteringData1yr.RData')

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
lca6$probs.se <- lca6$probs.se[c(1:4,14,5:14)]

itemNames <- c('Age','Income\nQuartile','Marital\nStatus','Immig.','Deaf-\nDisabled','Self\n-Care','Vision','Indep.\nLiving','Amb.','Cog','Kids at\nHome','Ed.\nAttain.','Race','Sex','In\nSchool')

porig <- lca6$P
lca6$P <- crossprod(lca6$posterior,sdat2$pwgtp/sum(sdat2$pwgtp))#/nrow(sdat2)

lcadat$other <- ifelse(sdat2$otherDiss, 'Yes','No')
makeplot2(lca6,NULL,itemNames, calc='other', include.margin=TRUE,data=lcadat)
ggsave('pooledCluster6.png',height=6.5,width=9,units='in')

#### plot groups of variables
probs <- lca6$probs
for(nn in names(probs)) probs[[nn]] <- rbind(probs[[nn]],Overall=margProb(lcadat[[nn]]))

## plot disabilities
disDat <- sapply(c('selfCare','blind','indepLiving','ambulatory','cognitive','other'),
  function(x) probs[[x]][,1])
disDat <- as.data.frame(disDat)
disDat$class <- c(paste('Class',1:6),'Overall')

library(tidyr)
disDat <- gather(disDat, 'disability','probability',1:6)

disDat$disability <- c(selfCare='Self Care',blind='Blind',indepLiving='Independent Living',ambulatory='Ambulatory',cognitive='Cognitive',other='Deafdisabled')[disDat$disability]

disDat$disability <- factor(disDat$disability)
disDat$disability <- relevel(disDat$disability,ref='Deafdisabled')
disDat$overall <- disDat$disability=='Deafdisabled'|disDat$class=='Overall'

ggplot(disDat,aes(disability,probability,fill=overall))+geom_col(position='dodge')+facet_wrap(~class,nrow=1)+theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust=.5),legend.position='none',text=element_text(size=12))+xlab(NULL)
ggsave('disability.png',width=6.5,height=3)

## plot demographics
demDat <- sapply(c('native','white','sex'),function(x) probs[[x]][,1])
demDat <- as.data.frame(demDat)
demDat$class <- c(paste('Class',1:6),'Overall')

library(tidyr)
demDat <- gather(demDat, 'demographic','probability',1:3)

demDat$demographic <- c(native='Native Born',white='White',sex='Female')

demDat$demographic <- factor(demDat$demographic)
demDat$overall <- demDat$class=='Overall'

ggplot(demDat,aes(demographic,probability,fill=overall))+geom_col(position='dodge')+facet_wrap(~class,nrow=1)+theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust=.5),legend.position='none',text=element_text(size=12))+xlab(NULL)
ggsave('demographic.png',width=6.5,height=3)

### plot family
famDat <- with(probs,cbind(Female=sex[,1],Married=married[,1],hupac))

famDat <- as.data.frame(famDat)
famDat$class <- c(paste('Class',1:6),'Overall')

famDat <- gather(famDat, 'fam','probability',1:5)

famDat$fam <- factor(famDat$fam,levels=unique(famDat$fam))
famDat$overall <- famDat$class=='Overall'

ttt <- textGrob("Kids at Home")

ggplot(famDat,aes(fam,probability,fill=overall))+geom_col(position='dodge')+facet_wrap(~class,nrow=1)+theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust=.5),legend.position='none',text=element_text(size=12),plot.margin = unit(c(1,1,2,1), "lines"))+xlab(NULL)+scale_y_continuous(name=NULL,labels=scales::percent,limits=c(0,1))+annotation_custom(ttt,xmin=3,xmax=5,ymin=-0.07,ymax=0)

ggsave('family.png',width=6.5,height=3)

#### education
edDat <- with(probs,cbind(attain,inSchool))

edDat <- as.data.frame(edDat)
edDat$class <- c(paste('Class',1:6),'Overall')

edDat <- gather(edDat, 'ed','probability',1:5)
edDat$ed[edDat$ed=='Yes'] <- 'Enrolled'

edDat$ed <- factor(edDat$ed,levels=unique(edDat$ed))
edDat$overall <- edDat$class=='Overall'

ggplot(edDat,aes(ed,probability,fill=overall))+geom_col(position='dodge')+facet_wrap(~class,nrow=1)+theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust=.5),legend.position='none',text=element_text(size=12),plot.margin = unit(c(1,1,2,1), "lines"))+xlab(NULL)+scale_y_continuous(name=NULL,labels=scales::percent,limits=c(0,1))

ggsave('education.png',width=6.5,height=3)

### age
ageDat <- probs$age
ageDat <- as.data.frame(ageDat)
ageDat$class <- c(paste('Class',1:6),'Overall')

ageDat <- gather(ageDat, 'age','probability',1:3)

ageDat$age <- factor(ageDat$age,levels=unique(ageDat$age))
ageDat$overall <- ageDat$class=='Overall'

ggplot(ageDat,aes(age,probability,fill=overall))+geom_col(position='dodge')+facet_wrap(~class,nrow=1)+theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust=.5),legend.position='none',text=element_text(size=12),plot.margin = unit(c(1,1,2,1), "lines"))+xlab(NULL)+scale_y_continuous(name=NULL,labels=scales::percent,limits=c(0,1))+ggtitle('Age')

ggsave('age.png',width=6.5,height=3)

##
incomeDat <- probs$hincCat
incomeDat <- as.data.frame(incomeDat)
incomeDat$class <- c(paste('Class',1:6),'Overall')

incomeDat <- gather(incomeDat, 'income','probability',1:4)

incomeDat$income <- factor(incomeDat$income,levels=unique(incomeDat$income))
incomeDat$overall <- incomeDat$class=='Overall'

ggplot(incomeDat,aes(income,probability,fill=overall))+geom_col(position='dodge')+facet_wrap(~class,nrow=1)+theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust=.5),legend.position='none',text=element_text(size=12),plot.margin = unit(c(1,1,2,1), "lines"))+xlab(NULL)+scale_y_continuous(name=NULL,labels=scales::percent,limits=c(0,1))+ggtitle('Income')

ggsave('income.png',width=6.5,height=3)



mean(rowMax(lca5$posterior)>0.8)
lcasD <- lapply(1:10,function(k) poLCA(cbind(ageCat,hincCat,married,native,hupac,attain,white,sex)~1,nclass=k,nrep=20,na.rm=FALSE,verbose=FALSE,calc.se=FALSE,data=lcadat[!sdat2$otherDiss,]))
sapply(lcasD,function(x) c(aic=x$aic,bic=x$bic))

odDat <- lcadat[sdat2$otherDiss,]
odDat$cognitive[odDat$selfCare==2 & odDat$blind==2 & odDat$indepLiving==2 & odDat$ambulatory==2] <- NA
lcasOD <- lapply(1:10,function(k) poLCA(cbind(ageCat,hincCat,married,native,selfCare,blind,indepLiving,ambulatory,cognitive,hupac,attain,white,sex)~1,nclass=k,nrep=20,na.rm=FALSE,verbose=FALSE,calc.se=FALSE,data=odDat))
sapply(lcasOD,function(x) c(aic=x$aic,bic=x$bic))

margProb <- function(x) as.vector(table(x))/length(x)

probsD <- nameCat(lcasD[[3]]$probs)
makeplot2(probsD,itemNames=itemNames[c(1:4,11:14)],P=c(lcasD[[3]]$P,1))
ggsave('lcaDeaf.pdf',height=6.5,width=9,units='in')

probsOD <- lcasOD[[4]]$probs
cog <- NULL
cogDiff <- 2-lcadat[sdat2$otherDiss,'cognitive']
numDis <- NULL
nd <- with(lcadat[sdat2$otherDiss,], (selfCare==1) + (blind==1) +(indepLiving==1) +(ambulatory==1) +(cognitive==1))
for(cc in 1:4){
    post <- lcasOD[[4]]$posterior[,cc]
    p <- sum(cogDiff*post)/sum(post)
    cog <- rbind(cog,c(p,1-p))
    ps <- sapply(1:5,function(n) sum((nd==n)*post)/sum(post))
    numDis <- rbind(numDis,ps)
}

probsOD$cognitive <- cog
probsOD <- nameCat(probsOD)

colnames(numDis) <- 1:5
numDis <- rbind(numDis,margProb(nd))
probsOD$other <- numDis
probsOD <- probsOD[names(probsAll)]

probsOD <- lapply(names(probsOD),function(x) rbind(probsOD[[x]],margProb(lcadat[sdat2$otherDiss,x])))

itemNames2 <- itemNames
itemNames2[5] <- '# of\nDisabilities'
makeplot2(probsOD, itemNames=itemNames2,P=c(lcasOD[[4]]$P,1))
ggsave('lcaDeafDisabled.pdf',height=6.5,width=9,units='in')


