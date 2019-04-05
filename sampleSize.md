---
title: cluster sample size
format: "html_document"
---


```r
pload('clusteringData1yr.RData')
```

```
## [1] "sdat"
```
Excluded: non-deaf, institutionalized


```r
nrow(sdat) ## initial sample size
```

```
## [1] 127563
```

```r
table(sdat$dear) ## deaf people
```

```
## 
##      1 
## 127563
```

```r
range(sdat$agep) ## age range
```

```
## [1]  0 96
```

```r
sum(sdat$esr==6,na.rm=TRUE) ## NILF; na for age<16
```

```
## [1] 92602
```

```r
mean(sdat$esr==6,na.rm=TRUE) ## NILF; na for age<16
```

```
## [1] 0.7439
```

Now copying exclusion code from `lca.r`

Age:

```r
sdat <- subset(sdat,agep<41 & agep>=25)
nrow(sdat)
```

```
## [1] 5735
```

```r
sum(sdat$esr==6)
```

```
## [1] 1834
```

```r
mean(sdat$esr==6)
```

```
## [1] 0.3198
```

Exclude income=NA (i.e. group quarters):

```r
sdat <- subset(sdat,!is.na(hincp))
nrow(sdat)
```

```
## [1] 5511
```

```r
sum(sdat$esr==6)
```

```
## [1] 1705
```

```r
mean(sdat$esr==6)
```

```
## [1] 0.3094
```

Only NILF

```r
sdat <- subset(sdat,esr==6)
nrow(sdat)
```

```
## [1] 1705
```
