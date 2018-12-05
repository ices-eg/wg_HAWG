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

#Set stock object name - this is propagated through into the figure titles
MSH@name    <- "Herring in VIa (combined) and VIIbc"

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
#Load and modify all index data
MSH.tun   <- readFLIndices(file.path(data.source, "fleetStandardized.txt"))
IBTSQ1    <- read.csv(file.path(indices.data,"SWC_Q1_94-18_Idx_model_SI.2_spALK_G30_ages1-9_LN_G9412.csv"),header=T,stringsAsFactors=F)
IBTSQ4    <- read.csv(file.path(indices.data,"SWC_Q4_96-17_Idx_model_SI.2_spALK_G30_ages0-9_LN_G9609.csv"),header=T,stringsAsFactors=F)

## Set the Index Type
MSH.tun[[1]]@type               <- "number"
MSH.tun[[2]]@type               <- "number"
MSH.tun[[3]]@type               <- "number"
MSH.tun[[4]]@type               <- "number"

## Call the index whatever
MSH.tun[[1]]@name               <- "MS HERAS"
MSH.tun[[2]]@name               <- "WoS HERAS"
MSH.tun[[3]]@name               <- "IBTS_Q1"
MSH.tun[[4]]@name               <- "IBTS_Q4"
#Set names
names(MSH.tun)                  <- lapply(MSH.tun,name)

MSH.tun[["IBTS_Q1"]]@index[]    <- t(IBTSQ1)[-1,]
MSH.tun[["IBTS_Q4"]]@index[]    <- t(IBTSQ4)[-1,]


## Set the plus group or specify there isnt one
MSH.tun[[1]]@range["plusgroup"] <- 9
MSH.tun[[2]]@range["plusgroup"] <- 9
MSH.tun[[3]]@range["plusgroup"] <- 9
MSH.tun[[4]]@range["plusgroup"] <- 9


## Trim indices to have the required ages
MSH.tun                         <- FLIndices("MS HERAS"=MSH.tun[[1]],"WoS HERAS"=MSH.tun[[2]],"IBTS_Q1"=trim(MSH.tun[[3]],age=2:9),"IBTS_Q4"=trim(MSH.tun[[4]],age=2:9))

### ======================================================================================================
### Prepare FLSAM object
### ======================================================================================================
MSH.ctrl <- FLSAM.control(MSH,MSH.tun)

#All fishing mortality states are free except
#oldest ages to ensure stablity
#MSH.ctrl@states["catch unique",]            <- c(1:7,8,8)

#Correlated Random walks for fishing mortalities - Default = FALSE = independent)
MSH.ctrl@cor.F                              <- 2

# Catchabilities
MSH.ctrl@catchabilities["MS HERAS",]        <- c(1,1,2,rep(3,2),rep(4,4))
MSH.ctrl@catchabilities["WoS HERAS",]       <- c(101,102,103,rep(104,2),rep(105,2),rep(106,2))
MSH.ctrl@catchabilities["IBTS_Q1",ac(2:9)]  <- c(202,rep(203,3),rep(204,2),rep(205,2))
MSH.ctrl@catchabilities["IBTS_Q4",ac(2:9)]  <- c(302,rep(303,2),rep(304,3),rep(305,2))

#Fishing mortality RWs are set from an analysis of ICA VPA results
MSH.ctrl@f.vars["catch unique",]            <- c(1,2,rep(3,3),rep(3,4))#c(1,rep(2,8))

#Set the variances. Separate variance for recruitment and plus group
MSH.ctrl@logN.vars[]                        <- c(1,2,rep(3,7))

#Bind the observation variances
MSH.ctrl@obs.vars["catch unique",]          <- c(1,2,rep(3,5),rep(4,2))
MSH.ctrl@obs.vars["MS HERAS",1:9]           <- c(101,102,rep(103,5),rep(104,2))
MSH.ctrl@obs.vars["WoS HERAS",1:9]          <- c(201,rep(202,4),rep(203,2),rep(204,2))
MSH.ctrl@obs.vars["IBTS_Q1",2:9]            <- c(302,rep(303,2),rep(304,3),rep(305,2))
MSH.ctrl@obs.vars["IBTS_Q4",2:9]            <- c(402,rep(403,5),rep(404,2))

#- Correlation structure in the indices
MSH.ctrl@cor.obs["MS HERAS",]               <- c(0,rep(1,7))
MSH.ctrl@cor.obs["WoS HERAS",]              <- c(101,rep(102,7))
MSH.ctrl@cor.obs["IBTS_Q1",2:8]             <- rep(202,7)
MSH.ctrl@cor.obs["IBTS_Q4",2:8]             <- rep(302,7)
MSH.ctrl@cor.obs.Flag[2:5]                  <- as.factor("AR")

MSH.ctrl                                    <- update(MSH.ctrl)



### ======================================================================================================
### Perform the assessment
### ======================================================================================================
MSH.sam             <- FLSAM(MSH,MSH.tun,MSH.ctrl)
MSH.ctrl@residuals  <- FALSE
MSH.retro           <- retro(MSH,MSH.tun,MSH.ctrl,7)

run_name            <- "corInd_IBTSQ4"
save(MSH,MSH.sam,MSH.tun,MSH.ctrl,MSH.retro,file=file.path(output.dir,paste0("IBP_VIaHerring_",run_name,"baserun.Rdata")))


source("./createAssessmentPlots.r")


#- Additional diagnostics and runs
xyplot(value ~ an(name) | fleet,group=age,data=catchabilities(MSH.retro),type="b",pch=19,scales=list(y="free"))

MSH.loi <- looi(MSH,MSH.tun,MSH.ctrl,type="loo",MSH.sam)

MSH.noMSHERAS_retro  <- retro(MSH,MSH.tun[-1],drop.from.control(MSH.ctrl,fleet="MS HERAS"),7)
MSH.noWoSHERAS_retro <- retro(MSH,MSH.tun[-2],drop.from.control(MSH.ctrl,fleet="WoS HERAS"),7)
MSH.noIBTS_Q1_retro  <- retro(MSH,MSH.tun[-3],drop.from.control(MSH.ctrl,fleet="IBTS_Q1"),7)
MSH.noIBTS_Q4_retro  <- retro(MSH,MSH.tun[-4],drop.from.control(MSH.ctrl,fleet="IBTS_Q4"),7)