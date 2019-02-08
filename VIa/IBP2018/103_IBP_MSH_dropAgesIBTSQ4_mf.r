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
path                <- "D:/Repository/ICES_HAWG/wg_HAWG/VIa/IBP2018/"
try(setwd(path),silent=TRUE)

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
MSHm@landings.wt        <- MSHm@catch.wt
MSHm@landings           <- computeLandings(MSHm)
MSHm@catch              <- computeCatch(MSHm)


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
MSH.tun[["IBTS_Q4"]]           <- trim(MSH.tun[["MS HERAS"]],age=2:8)

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
MSH.ctrl@logN.vars[]                        <- c(1,2,rep(3,7))

#Bind the observation variances
MSH.ctrl@obs.vars["catch S",]               <- c(001,002,rep(003,4),rep(004,3))
MSH.ctrl@obs.vars["catch N",1:9]            <- c(101,102,rep(103,5),rep(104,2))
MSH.ctrl@obs.vars["MS HERAS",1:9]           <- c(201,202,rep(203,4),rep(204,3))
MSH.ctrl@obs.vars["IBTS_Q1",2:9]            <- c(    302,rep(303,3),rep(304,2),rep(305,2))
MSH.ctrl@obs.vars["IBTS_Q4",2:8]            <- c(    402,rep(403,4),rep(404,2))

#- Correlation structure in the indices
MSH.ctrl@cor.obs["MS HERAS",]               <- c(0,rep(1,7))
MSH.ctrl@cor.obs["IBTS_Q1",2:8]             <- c(rep(202,7))
MSH.ctrl@cor.obs.Flag[3:4]                  <- as.factor("AR")

MSH.ctrl@residuals                          <- TRUE
MSH.ctrl                                    <- update(MSH.ctrl)


### ======================================================================================================
### Perform the assessment
### ======================================================================================================
MSHm.sam             <- FLSAM(MSHm,MSH.tun,MSH.ctrl)
MSH.ctrl@residuals   <- FALSE
MSHm.retro           <- retro(MSHm,MSH.tun,MSH.ctrl,7)


run_name            <- "dropAgesMSHERAS1to4_mf"
source("./createAssessmentPlotsMF.r")
save(MSHm,MSHm.sam,MSH.tun,MSH.ctrl,MSHm.retro,file=file.path(output.dir,paste0("IBP_VIaHerring_",run_name,"_MF.Rdata")))




#- Additional diagnostics and runs
xyplot(value ~ an(name) | fleet,group=age,data=catchabilities(MSH.retro),type="b",pch=19,scales=list(y="free"))

MSH.loi <- looi(MSH,MSH.tun,MSH.ctrl,type="loo",MSH.sam)

MSH.noMSHERAS_retro  <- retro(MSH,MSH.tun[-1],drop.from.control(MSH.ctrl,fleet="MS HERAS"),7)
MSH.noWoSHERAS_retro <- retro(MSH,MSH.tun[-2],drop.from.control(MSH.ctrl,fleet="WoS HERAS"),7)
MSH.noIBTS_Q1_retro  <- retro(MSH,MSH.tun[-3],drop.from.control(MSH.ctrl,fleet="IBTS_Q1"),7)
MSH.noIBTS_Q4_retro  <- retro(MSH,MSH.tun[-4],drop.from.control(MSH.ctrl,fleet="IBTS_Q4"),7)