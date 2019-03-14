knitr::opts_chunk$set(echo = TRUE)

rm(list=(ls()))

library(DATRAS)
library(mgcv)
library(parallel)
library(maps) 
library(mapdata)
library(surveyIndex)
library(lattice)
library(grid)
library(gridExtra)

setwd("C:/git/wg_HAWG/NSAS/data/IBTS index/")
#source("D:/UserData/Work/2017_course/spatial_temporal_model/01_Monday/Data/HighstatLibV10.R")

## function
Mypairs <- function(Z, mytitle, MyVarx) {
  #MyVarx <- colnames(Z)
  pairs(Z, labels = MyVarx,
        cex.labels =  2,
        lower.panel = function(x, y, digits=2, prefix="", cex.cor = 3) {
          panel.cor(x, y, digits, prefix, cex.cor)}, 
        upper.panel =  function(x, y) points(x, y, 
                                             pch = 16, cex = 0.8, 
                                             col = gray(0.1)),
        diag.panel = function(...) {
          rect(par("usr")[1], par("usr")[3],
               par("usr")[2], par("usr")[4], col="grey80")},
        #text.panel = function(x, y, labels) text(x, y, labels, cex=0.5),
        main=mytitle, cex.main=1.5)
  #print(P)
}

# IBTS-Q3: 1998-2018

year_last   <- 2018
yearlist    <- 1998:year_last   

cmSize      <- 1
spectrumMax <- 40
agesQ3      <- 0:6
outFolder   <- "."
species     <- "Clupea harengus"

# workspace created using datras package. Needs some work, this has been extracted by Casper
#load("NSIBTSherring_1984_2018.RData") # data set using DATRAS package (Casper)
#IBTSQ1 <- subset(d,Species==species,Quarter == 1,Year %in% yearlist,HaulVal=="V",StdSpecRecCode==1)
#IBTSQ1 <- addSpatialData(IBTSQ1,"./shapefiles/ICES_areas.shp")

# zip files extracted from datras manually
IBTS <- readExchangeDir(paste(".","/IBTSQ3/",sep=""), strict = FALSE)
IBTSQ3 <- subset(IBTS,Species==species,Quarter == 3,Year %in% yearlist,HaulVal=="V",StdSpecRecCode==1)
IBTSQ3 <- addSpatialData(IBTSQ3,"./shapefiles/ICES_areas.shp")

#IBTS <- readExchangeDir(paste(".","/IBTSQ3/",sep=""),strict=FALSE)
#IBTS <- addSpatialData(IBTS,"./shapefiles/ICES_areas.shp")
#table(IBTS[[1]]$Year, IBTS[[1]]$Country, useNA="always")
#table(IBTS[[1]]$Year, IBTS[[1]]$Gear, useNA="always")
#table(IBTS[[2]]$Year, IBTS[[2]]$Gear, useNA="always")
#table(IBTS[[1]]$Year, IBTS[[1]]$Ship, useNA="always")
#table(IBTS[[2]]$Year, IBTS[[2]]$Ship, useNA="always")

#IBTS <- subset(IBTS,Species==species,Year %in% years,HaulVal=="V",StdSpecRecCode==1)

res <- NA

## load all data
dAll <- c(IBTSQ3)

## select year
iyearlist  <- yearlist[1:(length(yearlist))]
dAll       <- subset(dAll,Year %in% iyearlist)

## this I dont know... CA indidual fish with age weight... HH haul specific information... 
## HL length frequency. Add N field (raw number of fish, not normalized), length frequency per haul, this is for convenience use in other functions
dAll <- addSpectrum(dAll,cm.breaks=seq(0,spectrumMax,by=cmSize))


## impute missing depths
summary(dAll$Depth)
dmodel    <- gam(log(Depth) ~ s(lon,lat,k=200),data=dAll[[2]])
sel       <- subset(dAll,is.na(Depth))
sel$Depth <- 0      ## Guard against NA-error
dAll$Depth[is.na(dAll$Depth)] <- exp(predict(dmodel,newdata=sel[[2]]))
dmodel    <- NULL
sel       <- NULL
gc()


####---select area, gear, vessel, country---
summary(dAll[[2]]$ICES_area)
table(dAll[[2]]$Year, dAll[[2]]$ICES_area, useNA="always")
##exclude area 23, VIId
table(dAll[[2]]$Year, dAll[[2]]$Month, useNA="always")
##some years samples are collected in June(exclude), few samples in September in recent years
table(dAll[[2]]$Year, dAll[[2]]$Gear, useNA="always")
##Only GOV gear with few samples in H18 and HOB

##---OPTION 1: keep september
dQ3  <- subset(dAll,ICES_area %in% as.character(c("IVa","IVb","IVc")), Gear %in% "GOV", Month>6)
##---OPTION 2: exclude september
#dQ3  <- subset(dAll,ICES_area %in% as.character(c("IVa","IVb","IVc")), Gear %in% "GOV", Month>6, Month<9)

table(dQ3[[2]]$Year, dQ3[[2]]$Month, useNA="always")
table(dQ3[[2]]$Year, dQ3[[2]]$Gear, useNA="always")
table(dQ3[[1]]$Year, dQ3[[1]]$Age, useNA="always")

###----extra QA----
summary(dQ3[[3]]$LngtCm)
table(dQ3[[1]]$Year, dQ3[[1]]$Age, useNA="always")

tmp <- NULL
gc()


removeAgeNAs<-function(x) {
  x[[1]]=subset(x[[1]],!is.na(x[[1]]$Age))
  x[[1]]=subset(x[[1]],!is.na(x[[1]]$NoAtALK))
  x
}

dQ3      <- removeAgeNAs(dQ3)

##################

###############
## Declare settings for ALK model
mf = "" 
ack=TRUE;
useBICs=TRUE;
varCofs=FALSE;
maxKs=50;
mc.cores=1

add.ALK<-function(d){
  ages=agesQ3
  if(d$Quarter[1]=="1"){
    d[[1]]=subset(d[[1]],Age>=1)
    d=fixAgeGroup(d,1)
    ages=agesQ1
  }
  
  d        <- addSpectrum(d,cm.breaks=seq(0,spectrumMax,by=cmSize))
  d.ysplit <- split(d,d$Year)
  d.ALK <- mclapply(d.ysplit,fitALK,minAge=min(ages),maxAge=max(ages),autoChooseK=ack,useBIC=useBICs,varCof=varCofs,maxK=maxKs,mc.cores=mc.cores)
  
  d.Nage=mclapply(d.ALK,predict,mc.cores=mc.cores)
  for(i in 1:length(d.ALK)) d.ysplit[[i]]$Nage=d.Nage[[i]];
  dd <- do.call("c",d.ysplit)
  dd    
}

table(dQ3[[1]]$Age, dQ3[[1]]$Year)
table(dQ3[[1]]$Age)
dQ3 <- add.ALK(dQ3)

grid.Q3  <- getGrid(dQ3, nLon=40) 


IBTSmodels <- list()

## Make ctime : a numeric time variable 
dQ3$ctime     <- as.numeric(as.character(dQ3$Year))

#######################
## Model formulae
#######################

## Stationary model
modelsStatZ   <- rep("Year+s(lon,lat,bs=c('tp'),k=kvecZ[a])+s(Depth,bs='ts',k=6)+offset(log(HaulDur))",length(agesQ3))
modelsStatP   <- rep("Year+s(lon,lat,bs=c('tp'),k=kvecP[a])+s(Depth,bs='ts',k=6)+offset(log(HaulDur))",length(agesQ3))
modelsNonStat <- rep("Year+te(ctime,lon,lat,d=c(1,2),bs=c('cs','tp'),k=c(5,25))+s(Depth,bs='ts',k=6)+offset(log(HaulDur))",length(agesQ3))

mc.cores <- 1
IBTSmodels$SI.ac <- getSurveyIdx(dQ3,ages=agesQ3,myids=grid.Q3[[3]],cutOff=0.5,fam="LogNormal",mc.cores=mc.cores,modelZ=modelsStatZ,modelP=modelsStatP)


temp        <- data.frame(iyearlist,IBTSmodels$SI.ac$idx)
names(temp) <- c("year",paste("age", as.character(agesQ3), sep=""))
res         <- rbind(res, temp)

res <- res[-1,]

save(res, file="./IBTSQ3/result_Q3.RData")

res[1:dim(res)[1],1] <- 1
write.table(round(res, digits = 2), "./IBTSQ3/IBTSQ3_output.txt", sep=" ",row.names = FALSE,col.names=FALSE)