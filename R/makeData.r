library(tidyverse)
library(gridExtra)
library(cluster)
library(survey)

states <- read.csv('../generalCode/states.csv')

pVars <- c('SERIALNO','DEAR','ST','AGEP','ADJINC','SCHL','ESR','WKW','WKHP','SCH','DDRS','DEYE','DOUT','DPHY','DRAT','DRATX','DREM','ENG','FER','MAR','INTP', 'OIP', 'PAP', 'PERNP', 'PINCP', 'RETP', 'SEMP', 'SSIP', 'SSP', 'WAGP','VPS','PAOC','SFR','RAC1P','HISP','RELP','MSP','WAOB','SCIENGRLP','CIT','SEX','PWGTP',paste0('PWGTP',1:80))

hVars <- c('SERIALNO','TYPE','LNGI','HUPAC','HUPAOC','HUPARC','FPARC','FES','HHT','PARTNER','WKEXREL','WIF','HINCP')

## need: DEAR, attain, employment,PERNP, fulltime

sdat <- read_csv('../../data/byYear/ss18pusa.csv')

#sdat <- read_csv('../data/acs5yr2016/ss16pusa.csv')
sdat <- sdat%>%filter(DEAR==1)
gc()
sdat <- sdat[,pVars]
for(nn in names(sdat)) if(is.character(sdat[[nn]])) sdat[[nn]] <- parse_integer(sdat[[nn]])
gc()

sdat2 <- read_csv('../../data/byYear/ss18pusb.csv')
#sdat <- read_csv('../data/acs5yr2016/ss16pusa.csv')
sdat2 <- sdat2%>%filter(DEAR==1)
gc()
sdat2 <- sdat2[,pVars]
for(nn in names(sdat2)) if(is.character(sdat2[[nn]])) sdat2[[nn]] <- parse_integer(sdat2[[nn]])
gc()

sdat <- bind_rows(sdat,sdat2)
rm(sdat2);gc()


#hdat <- read_csv('../data/acs5yr2016/ss16husa.csv')
hdat <- read_csv('../../data/byYear/ss18husa.csv')
hdat <- hdat[,hVars]
for(nn in names(hdat)) if(is.character(hdat[[nn]])) hdat[[nn]] <- parse_integer(hdat[[nn]])

hdat2 <- read_csv('../../data/byYear/ss18husa.csv')
hdat2 <- hdat2[,hVars]
for(nn in names(hdat2)) if(is.character(hdat2[[nn]])) hdat2[[nn]] <- parse_integer(hdat2[[nn]])

hdat <- bind_rows(hdat,hdat2)
rm(hdat2);gc()

sdat <- left_join(sdat,hdat)#cbind(sdat,hdat[match(sdat$SERIALNO,hdat$SERIALNO),])

rm(hdat); gc()
sdat <- select(sdat,-SERIALNO)

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


save(sdat,file='data/clusteringData1yr2018.RData')


