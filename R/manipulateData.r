sdat <- filter(sdat,DEAR==1)


sdat$state <- states$abb[match(sdat$ST,states$x)]

names(sdat) <- tolower(names(sdat))

sdat <- sdat%>%filter(type!=2)%>%#filter(AGEP<30)%>%filter(AGEP>=20)%>%
  mutate(hs = schl>=16,
    ba = schl>=21,
    employed = esr%in%c(1,2,4,5),
    unemployed = esr==3,
    fulltime=(wkw==1 & wkhp>=35))




sdat <- mutate(sdat,attain = cut(schl,breaks=c(0,15,17,20,21,Inf),
  labels=c('No HS','HS','Some College','Bachelors','>Bachelors'),
  ordered=TRUE))

sdat <- mutate(sdat,employment=ifelse(esr==6,'NotinLaborForce',
  ifelse(esr==3,'Unemployed','Employed')))

sdat <- mutate(sdat,raceEth=ifelse(hisp>1,"Hispanic",
  ifelse(rac1p==2,"African American",
    ifelse(rac1p==6| rac1p==7,"Asian/PacIsl",
      ifelse(rac1p%in%c(3,4,5),'American Indian',
        ifelse(rac1p==1,"White","Other"))))))

raceNs <- xtabs(~raceEth,data=sdat)



sdat <- mutate(sdat,ssip= (ssip>0))

sdat <- mutate(sdat,inSchool=ifelse(sch==1,0,1))

sdat <- mutate(sdat,english=ifelse(is.na(eng) | eng==1,1,0))

sdat$newmom <- ifelse(!is.na(sdat$fer) & sdat$fer==1,1,0)

sdat <- mutate(sdat,married=factor(ifelse(mar==1,'married',
  ifelse(mar<5,'prev.married','never.married'))))
sdat <- mutate(sdat,vet=!is.na(vps))

sdat <- mutate(sdat,citizenship =ifelse(cit<=3,'native',
                                     ifelse(cit==4,'naturalized','notCitizen')))
sdat <- mutate(sdat,whereBorn=ifelse(waob==1,'US',
                                  ifelse(waob<4,'latinAm',
                                    ifelse(waob==4,'asia',
                                      ifelse(waob==5,'europe','other')))))
sdat <- mutate(sdat,hupac = factor(hupac,exclude=NULL))

    sdat <- mutate(sdat,
      ownChildrenSmall=hupaoc%in%c(1,3),
      ownChildrenBig=hupaoc%in%c(2,3))

sdat <- within(sdat,{
  attain[attain=='>Bachelors'] <- "Bachelors"
                   raceEth[raceEth%in%c('American Indian','Asian/PacIsl')] <- 'Other'
  whereBorn[whereBorn=='europe'] <- 'other'
})

sdat$sex <- factor(c('M','F')[sdat$sex])

sdat <- within(sdat,
  otherDiss <-
    ddrs==1|deye==1|dout==1|dphy==1|drem==1)


## note: is.na(hincp) only true for group quarters
sdat <- subset(sdat,agep<41 & agep>=25 & !is.na(hincp)& esr==6 )#& inSchool==0)

sdat <- droplevels(sdat)

levels(sdat$hupac) <- c('0-6','6-17','0-6','noKids')

sdat$native <- sdat$citizenship=='native'

sdat$married <- sdat$married=='married'

sdat$white <- sdat$raceEth=='White'

dis <- c('ddrs','deye','dout','dphy','drem')
sdat$ageCat <- cut(sdat$agep,c(24,30,35,40),include.lowest=TRUE,labels=FALSE)

sdat$hincCat <- cut(sdat$hincp,quantile(sdat$hincp,na.rm=TRUE),include.lowest=TRUE,labels=FALSE)

lcadat <- model.frame(~ ageCat + married + native + ddrs+deye+dout+dphy+drem+
                          hupac + hincCat + attain + white+sex,data=sdat)

lcadat <- lcadat%>%rename(selfCare=ddrs,blind=deye,indepLiving=dout,ambulatory=dphy,cognitive=drem)


for(i in 1:ncol(lcadat)){
    if(is.factor(lcadat[[i]])) lcadat[[i]] <- as.numeric(lcadat[[i]])
    if(is.logical(lcadat[[i]])) lcadat[[i]] <- 2-lcadat[[i]]
}

lcadat$inSchool <- 2-sdat$inSchool


