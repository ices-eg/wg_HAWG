######################################################################################################
# Malin FLSAM Assessment
####################################################################################################

rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

library(FLSAM)
library(FLCore)

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
#path                <- "C:/Users/Lusseaus/Documents/ICES Working Groups/HAWG/2016/Repository/wg_HAWG/VIa/"
#path                <- "D:/Repository/ICES_HAWG/wg_HAWG/VIa/IBP2018/"
path                <- "D:/GIT/wg_HAWG/VIa/IBP2018/"
try(setwd(path),silent=FALSE)

data.source         <- file.path("../","data")      #Data source, not code or package source!!!
indices.data        <- file.path("./data")
output.dir          <- file.path(".","results")       #Output directory
output.base         <- file.path(output.dir) #Output base filename, including directory. Other output filenames are built by appending onto this one

### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
MSH                         <- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)
MSH@catch.n                 <- MSH@landings.n
MSH@catch                   <- MSH@landings
MSH@catch.wt                <- MSH@landings.wt
MSH@stock.wt[1,ac(2013)]    <- yearMeans(MSH@stock.wt[1,ac(2010:2012)])
MSH@stock.wt[1,ac(2015)]    <- yearMeans(MSH@stock.wt[1,ac(2010:2014)])
MSH@stock.wt[1,ac(2016)]    <- yearMeans(MSH@stock.wt[1,ac(2013:2015)])
MSH@stock.wt[1,ac(2017)]    <- yearMeans(MSH@stock.wt[1,ac(2013:2015)])
units(MSH)[1:17]            <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))

#Set fbar
range(MSH)[c("minfbar","maxfbar")] <- c(3,6)

#Set plus group
MSH <- setPlusGroup(MSH,MSH@range["max"])

#- Functions to set the plusgroups
setMatrixPlusGroupN <- function(x){
  x[nrow(x)-1,] <- colSums(x[(nrow(x)-1):(nrow(x)),],na.rm=T)
  return(x[-nrow(x),])}
setMatrixPlusGroupWt <- function(x,y){
  y[nrow(x)-1,] <- colSums(x[(nrow(x)-1):(nrow(x)),]*y[(nrow(y)-1):(nrow(y)),],na.rm=T) / colSums(x[(nrow(x)-1):(nrow(x)),],na.rm=T)
  y[is.nan(y)] <- 0
  return(y[-nrow(y),])}

VIaN_canum  <- readVPAFile(file.path(indices.data,"multifleet/VIaN_canum.txt"))
VIaS_canum  <- readVPAFile(file.path(indices.data,"multifleet/VIaS_canum.txt"))
VIaN_weca   <- readVPAFile(file.path(indices.data,"multifleet/VIaN_weca.txt"))
VIaS_weca   <- readVPAFile(file.path(indices.data,"multifleet/VIaS_weca.txt"))

MSHm <- FLCore::expand(MSH,area=c("N","S"))
MSHm@catch.n[,,,,"N"]   <- VIaN_canum
MSHm@catch.wt[,,,,"N"]  <- VIaN_weca
MSHm@catch.n[,,,,"S"]   <- VIaS_canum
MSHm@catch.wt[,,,,"S"]  <- VIaS_weca
MSHm@landings.n         <- MSHm@catch.n
MSHm@landings.wt        <- MSHm@catch.wt
MSHm@landings           <- computeLandings(MSHm)
MSHm@catch              <- computeCatch(MSHm)

# remove most recent catches; just for testing
# MSHm@catch.n[,ac(2014:2017),,,"N"]   <- NA
# MSHm@catch.n[,ac(2014:2017),,,"S"]   <- NA

#Set stock object name - this is propagated through into the figure titles
MSHm@name               <- "Herring in VIaN and VaS,VIIbc multifleet"

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
#Load and modify all index data
MSH.tun                 <- readFLIndices(file.path(data.source, "fleetStandardized.txt"))
IBTSQ1                  <- read.csv(file.path(indices.data,"SWC_Q1_94-18_Idx_model_SI.2.csv"),header=T,stringsAsFactors=F)
IBTSQ4                  <- read.csv(file.path(indices.data,"SWC_Q4_96-17_Idx_model_SI.2.csv"),header=T,stringsAsFactors=F)

## Call the index whatever
MSH.tun[[1]]@name               <- "MS HERAS"
MSH.tun[[2]]@name               <- "WoS HERAS"
MSH.tun[[3]]@name               <- "IBTS_Q1"
MSH.tun[[4]]@name               <- "IBTS_Q4"

#Set names
names(MSH.tun)                  <- lapply(MSH.tun,name)
MSH.tun[["IBTS_Q1"]]@index[]    <- t(IBTSQ1)[-1,]
MSH.tun[["IBTS_Q4"]]@index[]    <- t(IBTSQ4)[-1,]

## Set the Index Type
MSH.tun[[1]]@type               <- "number"
MSH.tun[[2]]@type               <- "number"
MSH.tun[[3]]@type               <- "number"
MSH.tun[[4]]@type               <- "number"

MSH.tun[["MS HERAS"]]                 <- window(MSH.tun[["MS HERAS"]],start=1991)
MSH.tun[["MS HERAS"]][,ac(1991:2007)] <- MSH.tun[["WoS HERAS"]]

#- Drop WoS as it is merged with MS
MSH.tun                         <- MSH.tun[-which(names(MSH.tun)=="WoS HERAS")]

## Set the plus group or specify there isnt one
MSH.tun[[1]]@range["plusgroup"] <- 9
MSH.tun[[2]]@range["plusgroup"] <- 9
MSH.tun[[3]]@range["plusgroup"] <- 9

## Trim indices to have the required ages
MSH.tun                         <- FLIndices("MS HERAS"=MSH.tun[[1]],"IBTS_Q1"=trim(MSH.tun[[2]],age=2:9),"IBTS_Q4"=trim(MSH.tun[[3]],age=2:9))

# load(file="D:/TEMP/MSH.tun.Rdata")

### ======================================================================================================
### Prepare FLSAM object
### ======================================================================================================
MSH.ctrl <- FLSAM.control(MSHm,MSH.tun)

#All fishing mortality states are free except
#oldest ages to ensure stablity
#MSH.ctrl@states["catch unique",]            <- c(1:7,8,8)

#Correlated Random walks for fishing mortalities - Default = FALSE = independent)
MSH.ctrl@cor.F                              <- 2

# Catchabilities
#MSH.ctrl@catchabilities["MS HERAS",]        <- c(1,2,3,4,4,4,4,4,4)
#MSH.ctrl@catchabilities["IBTS_Q1",ac(2:9)]  <- c(1,rep(2,7)) + 101
#MSH.ctrl@catchabilities["IBTS_Q4",ac(2:9)]  <- c(rep(1,3),rep(2,5)) + 101

#Fishing mortality RWs are set from an analysis of ICA VPA results
MSH.ctrl@f.vars["catch S",]            <- c(1,2,rep(3,5),rep(4,2))
MSH.ctrl@f.vars["catch N",]            <- c(1,rep(2,8)) + 101


#Set the variances. Separate variance for recruitment and plus group
MSH.ctrl@logN.vars[]                        <- c(1,rep(2,8))

#Bind the observation variances
MSH.ctrl@obs.vars["catch S",]               <- c(001,002,rep(003,4),rep(004,3))
MSH.ctrl@obs.vars["catch N",1:9]            <- c(101,102,rep(103,5),rep(104,2))
MSH.ctrl@obs.vars["MS HERAS",1:9]           <- c(201,202,rep(203,4),rep(204,3))
MSH.ctrl@obs.vars["IBTS_Q1",2:9]            <- c(    302,rep(303,3),rep(304,2),rep(305,2))
MSH.ctrl@obs.vars["IBTS_Q4",2:9]            <- c(    402,rep(403,4),rep(404,3))

#- Correlation structure in the indices
MSH.ctrl@cor.obs["MS HERAS",]               <- c(0,0,rep(1,6))
MSH.ctrl@cor.obs["IBTS_Q1",2:8]             <- c(rep(202,7))
MSH.ctrl@cor.obs["IBTS_Q4",2:8]             <- c(302,303,304,rep(305,4))
MSH.ctrl@cor.obs.Flag[3:5]                  <- as.factor("AR")

MSH.ctrl@residuals                          <- TRUE
MSH.ctrl                                    <- update(MSH.ctrl)

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
MSHm.ctrl            <- MSH.ctrl
MSHm.sam             <- FLSAM(MSHm,MSH.tun,MSH.ctrl)

# add in results
stk0            <- MSHm
stock.n(stk0)[] <- stock.n(window(MSHm.sam, end = 2017)) / dims(stk0)$area
harvest(stk0)   <- harvest(window(MSHm.sam, end = 2017))

stock(stk0) <- computeStock(stk0)
stk         <- collapseAreas(stk0)
plot(stk)

stk@harvest

### ======================================================================================================
### Perform the retrospective analysis (takes around 15 minutes)
### ======================================================================================================
MSH.ctrl@residuals   <- FALSE
MSHm.retro           <- retro(MSHm,MSH.tun,MSH.ctrl,7)

### ======================================================================================================
### Generate outputs
### ======================================================================================================
run_name            <- "finalRun"
source("./createAssessmentPlotsMF.r")

save(MSHm,MSHm.sam,MSH.tun,MSH.ctrl,MSHm.retro,
     file=file.path(output.dir,paste0("IBP_VIaHerring_",run_name,"_MF.Rdata")))


### ======================================================================================================
### What is the purpose here?
### ======================================================================================================

load("D:/Downloads/6a7bcHerring.RData")
MSH.ctrl@cor.obs["HERAS",]                  <- c(0,0,rep(1,6))
MSH.ctrl@f.vars[1,]                         <- c(1,2,rep(3,5),rep(4,2))
MSH.ctrl                                    <- update(MSH.ctrl)
MSH.ctrl@residuals    <- TRUE
MSH.sam               <- FLSAM(MSH,MSH.tun,MSH.ctrl)

MSH.ctrl@residuals    <- FALSE
MSH.retro            <- retro(MSH,MSH.tun,MSH.ctrl,7)

save(MSH.ctrl,MSH.tun,MSH,MSHm,MSHm.sam,MSHm.retro,MSH.sam,MSH.retro,file=file.path(output.dir,paste0("IBP_VIaHerring_",run_name,"_SF_MF.Rdata")))


### ======================================================================================================
### Leave one out analysis?
### ======================================================================================================

MSHm.looi               <- looi(MSHm,MSH.tun,MSH.ctrl,type="full",MSHm.sam)
save(MSHm.looi,file=file.path(output.dir,paste0("IBP_VIaHerring_",run_name,"_MF_LOOI.RData")))

MSH.ctrl@residuals      <- FALSE
MSHm.all_retro          <- retro(MSHm,MSH.tun,MSH.ctrl,7)
MSHm.noIBTS_Q1_retro    <- retro(MSHm,MSH.tun[-2],drop.from.control(MSH.ctrl,fleet="IBTS_Q1"),7)
MSHm.noIBTS_Q4_retro    <- retro(MSHm,MSH.tun[-3],drop.from.control(MSH.ctrl,fleet="IBTS_Q4"),7)
MSHm.noHERAS_retro      <- retro(MSHm,MSH.tun[-1],drop.from.control(MSH.ctrl,fleet="MS HERAS"),7)
MSHm.onlyIBTS_Q1_retro  <- retro(MSHm,MSH.tun[c(-1,-3)],drop.from.control(MSH.ctrl,fleet=c("IBTS_Q4","MS HERAS")),7)
MSHm.onlyIBTS_Q4_retro  <- retro(MSHm,MSH.tun[c(-1,-2)],drop.from.control(MSH.ctrl,fleet=c("IBTS_Q1","MS HERAS")),7)
MSHm.onlyHERAS_retro    <- retro(MSHm,MSH.tun[c(-2,-3)],drop.from.control(MSH.ctrl,fleet=c("IBTS_Q4","IBTS_Q1")),7)
save(MSHm.all_retro,MSHm.noIBTS_Q1_retro,MSHm.noIBTS_Q4_retro,MSHm.noHERAS_retro,MSHm.onlyIBTS_Q1_retro,MSHm.onlyIBTS_Q4_retro,MSHm.onlyHERAS_retro,file=file.path(output.dir,paste0("IBP_VIaHerring_",run_name,"_MF_LOOI_retros.RData")))

fileNames   <- c("MSHm.all_retro",
                 "MSHm.noIBTS_Q1_retro",
                 "MSHm.noIBTS_Q4_retro",
                 "MSHm.noHERAS_retro",
                 "MSHm.onlyIBTS_Q1_retro",
                 "MSHm.onlyIBTS_Q4_retro",
                 "MSHm.onlyHERAS_retro")
indicators  <- matrix(NA,
                      nrow=length(fileNames),
                      ncol=9,
                      dimnames=list(run=substr(fileNames,6,nchar(fileNames)),
                                    indicator=c("AIC","MohnsRhoSSB","MohnsRhoF","MohnsRhoR","MohnsRhoSSBAbs","MohnsRhoFAbs","MohnsRhoRAbs","loglik","nparam")))
for(iFile in fileNames){
  if(length(get(iFile))==8){
    indicators[substr(iFile,6,nchar(iFile)),"MohnsRhoSSB"]     <- mean(mohns.rho(get(iFile),ref.year=2017,span=7,type="ssb")[3:7,1])
    indicators[substr(iFile,6,nchar(iFile)),"MohnsRhoSSBAbs"]  <- mean(abs(mohns.rho(get(iFile),ref.year=2017,span=7,type="ssb")[3:7,1]))
    indicators[substr(iFile,6,nchar(iFile)),"MohnsRhoF"]   <- mean(mohns.rho(get(iFile),ref.year=2017,span=7,type="fbar")[3:7,1])
    indicators[substr(iFile,6,nchar(iFile)),"MohnsRhoFAbs"]   <- mean(abs(mohns.rho(get(iFile),ref.year=2017,span=7,type="fbar")[3:7,1]))
    indicators[substr(iFile,6,nchar(iFile)),"MohnsRhoR"]   <- mean(mohns.rho(get(iFile),ref.year=2017,span=7,type="rec")[3:7,1])
    indicators[substr(iFile,6,nchar(iFile)),"MohnsRhoRAbs"]   <- mean(abs(mohns.rho(get(iFile),ref.year=2017,span=7,type="rec")[3:7,1]))
  }
}
indicators <- cbind(indicators,sumRho=rowSums(abs(indicators[,2:4])),sumRhoAbs=rowSums(abs(indicators[,5:7])))
indicators <- cbind(indicators,sumRhos=indicators[,"sumRho"] + indicators[,"sumRhoAbs"])
indicators

for(iFile in names(MSHm.looi)){
  run_name             <- paste0("finalQ4corrLOOI_",iFile,"_mf")
  MSHm.sam             <- MSHm.looi[[iFile]]
  idx                  <- which(iFile == names(MSHm.looi))
  MSHm.retro           <- get(fileNames[c(1,3,2,7,4,5,6)][idx])
  source("./createAssessmentPlotsMF.r")
}



# Fix Qs for IBTS Q4
res   <- catchabilities(MSHm.retro)
meanQ <- aggregate(res$value,by=as.list(res[,c("age","fleet")]),FUN=mean)

data <- FLSAM2SAM(FLStocks(residual=MSHm),MSH.tun)
conf <- ctrl2conf(MSH.ctrl,data)
par  <- stockassessment::defpar(data,conf)
par$logFpar <- log(unique(meanQ$x))

fit <- stockassessment::sam.fit(data,conf,par),map=list(logFpar=as.factor(rep(NA,length(par$logFpar)))))

MSH.samfixQ <- SAM2FLR(fit,MSH.ctrl)
plot(FLSAMs(original=MSHm.retro[[1]],fixQ=MSH.samfixQ))

MSH.samLF <- SAM2FLR(fit,MSH.ctrl)


MSHm.retroFixQ <- list()
for(iYr in 2017:2010){
  print(iYr)
  data <- FLSAM2SAM(FLStocks(residual=window(MSHm,end=iYr)),FLIndices("MS HERAS"=window(MSH.tun[[1]],end=iYr),
                                                                      "IBTS_Q1"=window(MSH.tun[[2]],end=iYr+1),
                                                                      "IBTS_Q4"=window(MSH.tun[[3]],end=iYr)))
  MSH.ctrl@range[5] <- iYr+1
  conf <- ctrl2conf(MSH.ctrl,data)
  par  <- stockassessment::defpar(data,conf)
  par$logFpar <- log(unique(meanQ$x))

  fit <- stockassessment::sam.fit(data,conf,par,map=list(logFpar=as.factor(rep(NA,length(par$logFpar)))))
  MSHm.retroFixQ[[ac(iYr)]] <- SAM2FLR(fit,MSH.ctrl)
}

dat <- ssb(MSHm.retro)

plot(y=ssb(MSHm.retro[[1]])$value / mean(ssb(MSHm.retro[[1]])$value),x=ssb(MSHm.retro[[1]])$year,type="l")
for(iYr in 2016:2010)
  lines(y=ssb(MSHm.retro[[ac(iYr)]])$value / mean(ssb(MSHm.retro[[ac(iYr)]])$value),x=ssb(MSHm.retro[[ac(iYr)]])$year,type="l",col=iYr-2008)

#- Change from year to year
plot(y=diff(ssb(MSHm.retro[[1]])$value / mean(ssb(MSHm.retro[[1]])$value)),x=ssb(MSHm.retro[[1]])$year[-1],type="l",xlab="Year",ylab="Change from year to year",main="SSB rate of change")
for(iYr in 2016:2010)
  lines(y=diff(ssb(MSHm.retro[[ac(iYr)]])$value / mean(ssb(MSHm.retro[[ac(iYr)]])$value)),x=ssb(MSHm.retro[[ac(iYr)]])$year[-1],type="l",col=iYr-2008)

plot(y=diff(rev(rev(fbar(MSHm.retro[[1]])$value)[-1]) / mean(fbar(MSHm.retro[[1]])$value)),x=fbar(MSHm.retro[[1]])$year[-c(1:2)]+1,type="l",xlab="Year",ylab="Change from year to year",main="F rate of change")
for(iYr in 2016:2010)
  lines(y=diff(rev(rev(fbar(MSHm.retro[[ac(iYr)]])$value)[-1]) / mean(fbar(MSHm.retro[[ac(iYr)]])$value)),x=fbar(MSHm.retro[[ac(iYr)]])$year[-c(1:2)]+1,type="l",col=iYr-2008)



res <- as.data.frame(sweep(catch.n(MSHm),c(2:6),quantSums(catch.n(MSHm)),"/"))
surv1 <- as.data.frame(sweep(index(MSH.tun[[1]]),c(2:6),quantSums(index(MSH.tun[[1]])),"/")); surv1$area <- "MS_HERAS"
surv2 <- as.data.frame(sweep(index(MSH.tun[[2]]),c(2:6),quantSums(index(MSH.tun[[2]])),"/")); surv2$area <- "IBTS_Q1"
surv3 <- as.data.frame(sweep(index(MSH.tun[[3]]),c(2:6),quantSums(index(MSH.tun[[3]])),"/")); surv3$area <- "IBTS_Q4"
rescomb <- rbind(res,surv1,surv2,surv3)

xyplot(data ~ age | as.factor(year),data=subset(res,year>=1991),group=area,type="l",auto.key=T)
xyplot(data ~ age | as.factor(year),data=subset(rescomb,year>=1991),group=area,type="l",auto.key=T)

rescomb <- as.data.frame(sweep(areaSums(catch.n(MSHm)),c(2:6),areaSums(quantSums(catch.n(MSHm))),"/"))
res <- as.data.frame(sweep(catch.n(MSHm),c(2:6),quantSums(catch.n(MSHm)),"/"))
rescomb <- rbind(rescomb,res)
xyplot(data ~ an(age) | as.factor(year),data=subset(rescomb,year>=1991),type="l",group=area,auto.key=T)

catch <- as.data.frame(sweep(areaSums(catch.n(MSHm)),c(2:6),areaSums(quantSums(catch.n(MSHm))),"/"))
surv1 <- as.data.frame(sweep(index(MSH.tun[[1]]),c(2:6),quantSums(index(MSH.tun[[1]])),"/")); surv1$area <- "MS_HERAS"
surv2 <- as.data.frame(sweep(index(MSH.tun[[2]]),c(2:6),quantSums(index(MSH.tun[[2]])),"/")); surv2$area <- "IBTS_Q1"
surv3 <- as.data.frame(sweep(index(MSH.tun[[3]]),c(2:6),quantSums(index(MSH.tun[[3]])),"/")); surv3$area <- "IBTS_Q4"

rescomb <- rbind(catch,surv1)
xyplot(data ~ an(age) | as.factor(year),data=subset(rescomb,year>=1991),type="l",group=area,auto.key=T)

x11()
rescomb <- rbind(catch,surv2)
xyplot(data ~ an(age) | as.factor(year),data=subset(rescomb,year>=1991),type="l",group=area,auto.key=T)

x11()
rescomb <- rbind(catch,surv3)
xyplot(data ~ an(age) | as.factor(year),data=subset(rescomb,year>=1991),type="l",group=area,auto.key=T)
