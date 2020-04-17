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

### merge (actually "left join" sdat and hdat
### however, left_join() apparently takes up a ton of RAM and overloads the computer, whereas this works quick and easy
### that said, I'm always nervous about match() cuz it has led to bugs before. Hence the check below
hdatBind <- hdat[match(sdat$SERIALNO,hdat$SERIALNO),]
stopifnot(all.equal(hdatBind$SERIALNO,sdat$SERIALNO))

cat('dim(sdat)=',nrow(sdat),ncol(sdat),'\n')
cat('dim(hdatBind)=',nrow(hdatBind),ncol(hdatBind),'\n')
sdat <- bind_cols(sdat,hdatBind)
cat('after merge...\n')
cat('dim(sdat)=',nrow(sdat),ncol(sdat),'\n')


rm(hdat,hdatBind); gc()
sdat <- select(sdat,-SERIALNO)
