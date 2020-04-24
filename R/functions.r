
cn <- list(
        ageCat=c('26-30','31-35','36-40'),
        sex=c('Female','Male'),
        white=c('White','POC'),
        attain=c('No HS','HS','Some Col.','Bach+'),
        hincCat=c('1st Qu','2nd Qu','3rd Qu','4th Qu'),
        married=c('Married','Unmarried'),
        native=c('Native','Immigrant'),
        selfCare=c('Diff.','No Diff'),
        blind=c('Deafblind','Deaf'),
        otherDis=c('Deafdisabled','Deaf'),
        indepLiving=c('Diff.','No Diff'),
        ambulatory=c('Diff.','No Diff'),
        cognitive=c('Diff.','No Diff'),
        other=c('Yes','No'),
        inSchool=c('Yes','No'),
        hupac=c('Little','Big','None'))

nameCat <- function(probs){
    nnn <- names(probs)

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

tree <- function(fit,dat,...){
    for(nn in names(dat)) dat[[nn]] <- cn[[nn]][lcadat[[nn]]]
    dat <- dat%>%mutate_at(vars(ageCat,hincCat),ordered)%>%mutate(attain=ordered(attain, levels=c('No HS','HS','Some Col.','Bach+')))
    dat$class <- fit$predclass
    tree <- rpart(class~.,dat,method='class',...)
    rpart.plot(tree,type=0,extra=2)
}

figures <- function(fit){

    nclass <- length(fit$P)
    if(!is.element(paste0('figures/combined/model',nclass,'class'),list.dirs('figures/combined')))
        dir.create(paste0('figures/combined/model',nclass,'class'))

    fit$probs <- nameCat(fit$probs) #probsAll <- nameCat(probsAll)

  itemNames <- c('Age','Income\nQuartile','Marital\nStatus','Immig.','Deafblind','Deafdisabled','Kids at\nHome','Ed.\nAttain.','Race','Sex','In\nSchool')

  porig <- fit$P
  fit$P <- crossprod(fit$posterior,sdat$pwgtp/sum(sdat$pwgtp))#/nrow(sdat)

    prep <-
    sapply(1:80,
      function(x)
        crossprod(sdat[[paste0('pwgtp',x)]],fit$posterior)/sum(sdat[[paste0('pwgtp',x)]]))

  P.seOrig <- fit$P.se

  fit$P.se <- sqrt(apply(sweep(prep,1,fit$P),1,function(x) mean(x^2)*4)+
                     fit$P.se^2)

  makeplot2(fit,NULL,itemNames,include.margin=TRUE,data=lcadat)
  ggsave(paste0('figures/combined/model',nclass,'class/pooledCluster.png'),height=6.5,width=9,units='in')



## plot disabilities
      plotGroup(fit,varbs=c('otherDis','blind'),
                vnames=c(otherDis='Deafdisabled',blind='Deafblind'),
                catName='Disabilities')
      ggsave(paste0('figures/combined/model',nclass,'class/disability.png'),width=6.5,height=3)


  ## plot demographics
  plotGroup(fit,varbs=c('native','white','sex'),
    vnames=c(native='Native Born',white='White',sex='Female'),catName='Demographics')

  ggsave(paste0('figures/combined/model',nclass,'class/demographic.png'),width=6.5,height=3)

  ### plot family
  plotGroup(fit,varbs=c('sex','married','hupac'),catName='Family')+
    scale_x_discrete(labels=c('Female','Married','Kids at Home (<6yo)','Kids at Home (all >5yo)',
      'No Kids at Home'))
  ggsave(paste0('figures/combined/model',nclass,'class/family.png'),width=6.5,height=3)


  #### education
  plotGroup(fit,varbs=c('attain','inSchool'),catName='Education')+
    scale_x_discrete(labels=c('<HS','HS','Some College','BA+','Enrolled'))

  ggsave(paste0('figures/combined/model',nclass,'class/education.png'),width=6.5,height=3)

  ### age
  plotGroup(fit,'ageCat')+
    scale_x_discrete(labels=c('25-30','31-35','36-40'))+
    ggtitle('Age')

  ggsave(paste0('figures/combined/model',nclass,'class/age.png'),width=6.5,height=3)

  ##
  plotGroup(fit,'hincCat')+scale_x_discrete(labels=colnames(probs$hincCat))+
    ggtitle('Household Income Quartile')
  ggsave(paste0('figures/combined/model',nclass,'class/income.png'),width=6.5,height=3)



  ## plot proportions
  propDat <- data.frame(Class=factor(1:nclass,levels=nclass:1),Proportion=fit$P,SE=fit$P.se)
  propDat$label_pos <- cumsum(propDat$Proportion)-propDat$Proportion/2
  ggplot(propDat,aes(1,Proportion,fill=Class,label=Class))+geom_col()+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    geom_text(aes(1,label_pos))+coord_flip()+scale_fill_manual(values=subwayPalette,guide='none')+ylab(NULL)
  ggsave(paste0("figures/combined/model",nclass,'class/ClusterProportions.png'),width=3,height=1)
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
