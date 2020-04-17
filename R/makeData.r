library(tidyverse)
library(gridExtra)
library(cluster)
library(survey)

states <- read.csv('../generalCode/states.csv')

source('R/readInData.r')

source('R/manipulateData.r')

save(sdat,file='data/clusteringData1yr2018.RData')


