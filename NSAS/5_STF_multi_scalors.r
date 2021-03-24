#-------------------------------------------------------------------------------
# Code to do the multifleet short term forecast for North Sea Herring
#
# By: Benoit Berges (WMR)
#
# 12/03/2019 Rearranging previous code (Benoit Berges)
# 17/03/2019 Small modifications to calculating uptake and split (3 year averages)
# 18/03/2019 Checking code and adding additional comments (Martin Pastoors) 
# 22/03/2020 code update
# 25/03/2020 integrated longer term forecast into the code (last scenario)
#-------------------------------------------------------------------------------

rm(list=ls());

library(FLCore)
library(FLSAM)
library(minpack.lm)  # install.packages("minpack.lm")
require(msm)         # install.packages("msm")

library(tidyverse)
library(pander)
library(ggthemes)

# source publication theme
source("../_common/theme_publication.r")


#-------------------------------------------------------------------------------
# 1) read data
#    setup paths
#   load mf and sf objects and stf functions
#-------------------------------------------------------------------------------

path <- "J:/git/wg_HAWG/NSAS/"
#path <- "D:/GIT/wg_HAWG/NSAS/"

try(setwd(path),silent=FALSE)
 
dir.create("stf",showWarnings = FALSE)

assessment.dir <- file.path(".","assessment")

dataPath      <- file.path(".","data")
outPath       <- file.path(".","stf")
functionPath  <- file.path(".","functions")

#load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2020 Meeting Docs/06. Data/her.27.3a47d/NSH_HAWG2020_sf.Rdata")
#load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2020 Meeting Docs/06. Data/her.27.3a47d/NSH_HAWG2020_mf.Rdata")
#load(file=file.path(assessment.dir,"NSH_HAWG2021_sf.RData"))
#load(file=file.path(assessment.dir,"NSH_HAWG2021_M0.11_mf.RData"))
#load(file=file.path(assessment.dir,"NSH_HAWG2020_mf.RData"))

#quantMeans(NSH3f.sam[,ac(2018:2020)]@harvest[ac(2:6),,,,'A',])

#quantMeans(NSH3f.sam[,ac(2018:2020)]@harvest[ac(2:6),,,,,])

#load(file=file.path(assessment.dir,"NSH_HAWG2021_M0.11_mf.RData"))

#quantMeans(NSH3f.sam[,ac(2018:2020)]@harvest[ac(2:6),,,,'A',])

#load(file=file.path(assessment.dir,"NSH_HAWG2021_M0.11_sf.RData"))

#load(file=file.path(assessment.dir,"NSH_HAWG2021_M0.11_sf.RData"))

#quantMeans(NSH.sam@harvest)

#load(file=file.path(assessment.dir,"NSH_HAWG2020_sf.RData"))

#quantMeans(NSH.sam@harvest)

load(file=file.path(assessment.dir,"NSH_HAWG2021_M0.11_mf.RData"))
load(file=file.path(assessment.dir,"NSH_HAWG2021_M0.11_sf.RData"))

# use of retro obhect
#load(file=file.path(assessment.dir,"NSH_HAWG2021_M0.11_sf_retro.RData"))
#NSH@stock.n           <- NSH.sam@stock.n[,ac(range(NSH)["minyear"]:range(NSH)["maxyear"])]
#NSH@harvest           <- NSH.sam@harvest[,ac(range(NSH)["minyear"]:range(NSH)["maxyear"])]

#NSH <- NSH[,ac(1947:2019)]
#NSH.sam               <- NSH.retro[[2]]
#NSH@stock.n           <- NSH.sam@stock.n[,ac(range(NSH)["minyear"]:range(NSH)["maxyear"])]
#NSH@harvest           <- NSH.sam@harvest[,ac(range(NSH)["minyear"]:range(NSH)["maxyear"])]

#NSH <- ssb(NSH.retro[[2]])

# ImY forecast functions
source(file.path(functionPath,"fleet.harvest.r"))
source(file.path(functionPath,"rescaleF.r"))

# fmsyAR function
source(file.path(functionPath,"fmsyAR_fun_transfer.r"))
source(file.path(functionPath,"fmsyAR_fun_no_transfer.r"))
source(file.path(functionPath,"find.FCAR.r"))

# fmsyAR function F01 target
source(file.path(functionPath,"fmsyAR_fun_transfer_Btarget.r"))
source(file.path(functionPath,"fmsyAR_fun_no_transfer_Btarget.r"))
source(file.path(functionPath,"find.FCAR_Btarget.r"))

# MP functions
source(file.path(functionPath,"find.FAB_HCRA.r"))
source(file.path(functionPath,"find.FAB_HCRB.r"))
source(file.path(functionPath,"CATCH2sel.r"))     # Find F that matches a certain catch
source(file.path(functionPath,"MP_fun.r"))        # Management plan function

# no fishing function
source(file.path(functionPath,"nf_fun.r"))

# TAC scaling functions
source(file.path(functionPath,"TAC_scaling_fun.r"))
source(file.path(functionPath,"fleet.harvest2.r")) # only used for having FBsq
source(file.path(functionPath,"rescaleF2.r")) # only used for having FBsq

# F scaling functions
source(file.path(functionPath,"F_scaling_fun.r"))
source(file.path(functionPath,"find.FC.r"))

# B scaling function
source(file.path(functionPath,"B_scaling_fun.r"))
source(file.path(functionPath,"find.BC.r"))

# auxiliary
source(file.path(functionPath,"harvestCatch.r"))
source(file.path(functionPath,"writeSTF.out.r"))

#-------------------------------------------------------------------------------
# 2) setup control variables
#-------------------------------------------------------------------------------

DtY   <- ac(range(NSH)["maxyear"]) #Data year
ImY   <- ac(an(DtY)+1)             #Intermediate year
FcY   <- ac(an(DtY)+2)             #Forecast year
CtY   <- ac(an(DtY)+3)             #Continuation year
CtY1  <- ac(an(DtY)+4)
FuY   <- c(ImY,FcY,CtY)            #Future years


# flag on TAC assumptions for C and D fleet.
# If true, one takes TAC from WBSS advice
# If false, sq in TAC, i.e. one takes TAC from ImY (fed from WBSS advice)
TAC_CD_advice   <- TRUE

if(TAC_CD_advice == TRUE){
  stfFileName   <- paste0('NSAS_stf_',ImY)
}else{
  stfFileName   <- paste0('NSAS_stf_CD_TAC_sq_',ImY)
}

# slots copied from last year of assessment
yrs1        <- list("m.spwn","harvest.spwn","stock.wt")

# slots copied as mean of last 3 years of assessment
yrs3        <- list("mat")

# slots copied as mean of last 5 years of assessment
yrs5        <- list("m")

dsc         <- "North Sea Herring"
nam         <- "NSH"
dms         <- dimnames(NSH@m)
dms$year    <- c(rev(rev(dms$year)[1:3]),ImY,FcY,CtY)
dms$unit    <- c("A","B","C","D")

f01         <- ac(0:1)
f26         <- ac(2:6)

stf.options <- c('fmsyAR_transfer',
                 'fmsyAR_transfer_Btarget',
                 'fmsyAR_no_transfer',
                 'fmsyAR_no_transfer_Btarget',
                 "mpA",
                 "mpAC",
                 "mpAD",
                 "mpB",
                 "fmsy",
                 "nf",
                 "tacro",
                 "-15%",
                 "+15%",
                 "fsq",
                 "fpa",
                 "flim",
                 "bpa",
                 "blim",
                 "MSYBtrigger")
# mp    =according to management plan,
# +/-%  =TAC change,
# nf    =no fishing,
# bpa   =reach BPA in CtY or if ssb > bpa fish at fpa,
# tacro =same catch as last year
# fmsy  = fmsy implemented
# flim,fpa,blim,bpa = reach F or biomass targets

#-------------------------------------------------------------------------------
# 3) TAC information, reference points and management rule
#
# Note 1: TACS are the realised NSAS catches for the different fleets
# Note 2: TACS.orig are the original set TACs
#-------------------------------------------------------------------------------

############## reference points ##############
#- Set reference points (Fstatus-quo will be filled after intermediate year
referencePoints <- list(Fmsy = 0.26,
                        Fsq  = NA,
                        Flim = 0.34,
                        Fpa  = 0.30,
                        Blim = 800000,
                        Bpa  = 900000,
                        MSYBtrigger = 1400000,
                        Ftarget  = 0, # set this for individual cases
                        F01   = 0.05,
                        Btrigger = 0) # set this for individual cases

############## TAC variables in the ImY ##############
# initialize TAC variables array
TAC_var                   <- list(Ctransfer = NA,
                                  Csplit = NA,
                                  Dsplit = NA,
                                  WBSS_NSAS = NA,
                                  Buptake = NA,
                                  Duptake = NA)

# Splits of WBSS and NSAS 
# Note: splits are only available up to the terminal year
# Note: split in table are the percentage of WBSS in the total catch. Split for NSAS is 1-split
splitTab            <- read.table(file.path(dataPath,'TAC_var','NSAS_split.csv'),sep = ",",header=T) # historical C fleet WBSS/NSAS split (Henrik Mosegaard)
rownames(splitTab)  <- splitTab[,1]

# C split as average over the last 3 years
TAC_var$Csplit      <- mean(1-splitTab[ac((an(DtY)-2):an(DtY)),'C'])    # Proportion NSAS in C fleet catch; 3 year average 
TAC_var$Dsplit      <- mean(1-splitTab[ac((an(DtY)-2):an(DtY)),'D'])    # Proportion NSAS in C fleet catch; 3 year average 

# proportion WBSS caught in North Sea. use hard value from HAWG2018. Need to ask Norbert for historical values
TAC_var$WBSS_NSAS   <- splitTab[DtY,'WBSS_NSAS']

# Ctransfer. Transfer of TAC from IIIa to IVa for C fleet in assessment year
# Note: C transfer in table is the percentage transferred to NSAS from WBSS
CtransferTab            <- read.table(file.path(dataPath,'TAC_var','NSAS_C_transfer.csv'),sep = ",",header=T)
rownames(CtransferTab)  <- CtransferTab[,1]
TAC_var$Ctransfer       <- CtransferTab[ImY,2]

# B & D fleets uptakes (Henrik Mosegaard)
# What should we assume for the uptake?
uptakeTab               <- read.table(file.path(dataPath,'TAC_var','NSAS_uptake.csv'),sep = ",",header=T)
rownames(uptakeTab)     <- uptakeTab[,1]
TAC_var$Buptake         <- mean(uptakeTab[ac((an(DtY)-2):an(DtY)),'B'])   # Uptake of Bfleet as 3 year average
TAC_var$Duptake         <- mean(uptakeTab[ac((an(DtY)-2):an(DtY)),'D'])   # Uptake of the Dfleet as 3 year average

############## TAC for the different fleets in the intermediate year ##############
# TAC A is TACNSA; HER/4AB. + HER/4CXB7D 
# TAC B is TACNSB; HER/2A47DX 
# TAC C is TAC3aC; HER/03A.
# TAC D is TAC3aD; HER/03A-BC

# TAC information for current year
TACTab            <- read.table(file.path(dataPath,'TAC_var','NSAS_TAC.csv'),sep = ",",header=T)
rownames(TACTab)  <- TACTab[,1]

# tAC advice information
adviceTab             <- read.table(file.path(dataPath,'TAC_var','NSAS_advice.csv'),sep = ",",header=T)
rownames(adviceTab)   <- adviceTab[,1]

# Create TAC objects for current year, forecast year and last year
TACS        <- FLQuant(NA,dimnames=list(age="all",year=FuY,unit=c("A","B","C","D"),
                                        season="all",area=1,iter=1:dims(NSH)$iter))

TACS[,ImY,'A'] <- TACTab[ImY,'A']
TACS[,ImY,'B'] <- TACTab[ImY,'B']
# assume 0 TAC for FcY and CtY for C and D fleet
TACS[,ImY,'C'] <- TACTab[ImY,'C']
TACS[,ImY,'D'] <- TACTab[ImY,'D']

# TAC C and D fleet in FcY and CtY
# set TAC to 0.1 if 0 catch (for optimizers)
if(TAC_CD_advice == TRUE){
  TACS[,c(FcY,CtY),'C'] <- adviceTab[FcY,'C']
  if(TACS[,FcY,'C'] == 0){
    TACS[,c(FcY,CtY),'C'] <- 0.1
  }
}else{
  TACS[,c(FcY,CtY),'C'] <- TACTab[ImY,'C']
}

# D fleet
if(TAC_CD_advice == TRUE){
  TACS[,c(FcY,CtY),'D'] <- adviceTab[FcY,'D']
  if(TACS[,FcY,'D'] == 0){
    TACS[,c(FcY,CtY),'D'] <- 0.1
  }
}else{
  TACS[,c(FcY,CtY),'D'] <- TACTab[ImY,'D']
}

############## realised catches ##############
CATCH        <- FLQuant(NA,dimnames=list( age="all",year=FuY,unit=c("A","B","C","D"),
                                          season="all",area=1,iter=1:dims(NSH)$iter))

CATCH[,ImY,'A'] <- TACS[,ImY,'A'] + TAC_var$Ctransfer*TACS[,ImY,'C'] - (TACS[,ImY,'A'] + TAC_var$Ctransfer*TACS[,ImY,'C'])*TAC_var$WBSS_NSAS
CATCH[,ImY,'B'] <- TACS[,ImY,'B']*TAC_var$Buptake
CATCH[,ImY,'C'] <- TACS[,ImY,'C']*(1-TAC_var$Ctransfer)*TAC_var$Csplit
CATCH[,ImY,'D'] <- TACS[,ImY,'D']*TAC_var$Dsplit*TAC_var$Duptake

# assume same catch as ImY
# This is an alternative to the previous procedure of estimating the B-fleet catch from the Management plan
CATCH[,c(FcY,CtY),'B'] <- CATCH[,ImY,'B']

# zero catch in FcY and CtY for C and D fleet, because of zero catch advice for WBSS herring
if(TACS[,FcY,'C'] == 0.1){
  CATCH[,c(FcY,CtY),'C'] <- 0.1
}else{
  # no transfer assumed
  CATCH[,c(FcY,CtY),'C'] <- TACS[,FcY,'C']*TAC_var$Csplit#*(1-TAC_var$Ctransfer)
}
if(TACS[,FcY,'D'] == 0.1){
  CATCH[,c(FcY,CtY),'D'] <- 0.1
}else{
  CATCH[,c(FcY,CtY),'D'] <- TACS[,FcY,'D']*TAC_var$Dsplit*TAC_var$Duptake
}

#-------------------------------------------------------------------------------
# 4) Recruitment for intermediate, forecast and continuation years
#    ImY recruitment: comes from sf assessment (informed by IBTS0)
#    FcY and CtY: weighted average over 10 year using uncertainty as weighting
#-------------------------------------------------------------------------------
# Retrieve uncertainty estimate on recruitment estimates by the model for weighted average
recWeights  <- subset(NSH.sam@params, name=="logR")$std.dev^2

# fill in recruitment for the different years
RECS <- FLQuants( "ImY" =FLQuant(subset(rec(NSH.sam),year==ImY)$value),
                  "FcY" =exp(apply(log(rec(NSH)[,ac((an(DtY)-9):DtY)]),3:6,weighted.mean,w=1/rev(rev(recWeights)[2:11]),na.rm=T)),
                  "CtY" =exp(apply(log(rec(NSH)[,ac((an(DtY)-9):DtY)]),3:6,weighted.mean,w=1/rev(rev(recWeights)[2:11]),na.rm=T)))


#-------------------------------------------------------------------------------
# 5) Setup stock file
#-------------------------------------------------------------------------------

# Create an stf object 
stf         <- FLStock(name=nam,
                       desc=dsc,
                       m=FLQuant(NA,
                                 dimnames=dms))

# copy fields from current assessment to all fleets
for(i in dms$unit)
  stf[,,i]  <- window(NSH,start=an(dms$year)[1],end=rev(an(dms$year))[1])

units(stf)  <- units(NSH)

# Fill slots that are the same for all fleets
for(i in c(unlist(yrs1),unlist(yrs3),unlist(yrs5))){
  if(i %in% unlist(yrs1)){ for(j in dms$unit){ slot(stf,i)[,FuY,j] <- slot(NSH,i)[,DtY]}}
  if(i %in% unlist(yrs3)){ for(j in dms$unit){ slot(stf,i)[,FuY,j] <- yearMeans(slot(NSH,i)[,ac((an(DtY)-2):an(DtY))])}}
  if(i %in% unlist(yrs5)){ for(j in dms$unit){ slot(stf,i)[,FuY,j] <- yearMeans(slot(NSH,i)[,ac((an(DtY)-4):an(DtY))])}}
}

# taking data from multifleet assessment
for(i in 1:length(dms$unit)){
  
  # Fill the D fleet with the data from the B fleet; same selection pattern
  if(i == 4) {
    stf@harvest[,ac(an(dms$year)[1]:(an(dms$year)[1]+2)),4]     <- NSH3f.sam@harvest[,ac(an(dms$year)[1]:(an(dms$year)[1]+2)),,,2]
    stf@harvest[,FuY,4]     <- NSH3f.sam@harvest[,ImY,,,2]
    stf@catch.wt[,FuY,4]    <- NSHs3[["residual"]]@catch.wt[,DtY,,,2]
    stf@landings.wt[,FuY,4] <- NSHs3[["residual"]]@catch.wt[,DtY,,,2]
  }
  
  if(i != 4) {
    stf@harvest[,ac(an(dms$year)[1]:(an(dms$year)[1]+2)),i]     <- NSH3f.sam@harvest[,ac(an(dms$year)[1]:(an(dms$year)[1]+2)),,,i]
    stf@harvest[,FuY,i]     <- NSH3f.sam@harvest[,ImY,,,i]
    stf@catch.wt[,FuY,i]    <- NSHs3[["residual"]]@catch.wt[,DtY,,,i]
    stf@landings.wt[,FuY,i] <- NSHs3[["residual"]]@catch.wt[,DtY,,,i]
  }
}

# Fill slots that have no meaning for NSAS
stf@discards.n[]          <- 0
stf@discards[]            <- 0
stf@discards.wt[]         <- 0

#-------------------------------------------------------------------------------
# 5) Compute F in intermediate year
#-------------------------------------------------------------------------------

# stock number in ImY, using SAM estimate
for(i in dms$unit)
  stf@stock.n[,ImY,i]     <- NSH.sam@stock.n[,ImY]

# computing F 
stf@harvest[,ImY]         <- fleet.harvest(stk=stf,
                                           iYr=ImY,
                                           CATCH=CATCH[,ImY])
for(i in dms$unit){
  stf@catch.n[,ImY,i]     <- stf@stock.n[,ImY,i]*(1-exp(-unitSums(stf@harvest[,ImY])-stf@m[,ImY,i]))*(stf@harvest[,ImY,i]/(unitSums(stf@harvest[,ImY])+stf@m[,ImY,i]))
  stf@catch[,ImY,i]       <- computeCatch(stf[,ImY,i])
  stf@landings.n[,ImY,i]  <- stf@catch.n[,ImY,i]
  stf@landings[,ImY,i]    <- computeLandings(stf[,ImY,i])
}


#Intermediate year stf option table
stf.table <- array(NA,dim=c(c(length(stf.options)+1),12,3),
                   dimnames=list("options"=c("intermediate year",stf.options),
                                 "values" =c("Fbar 2-6 A","Fbar 0-1 B","Fbar 0-1 C","Fbar 0-1 D","Fbar 2-6","Fbar 0-1","Catch A","Catch B","Catch C","Catch D","SSB","SSB"),
                                 "stats"  =c("5%","50%","95%")))

stf.table[1,"Fbar 2-6 A",]                                  <- quantMeans(stf@harvest[f26,ImY,"A"])
stf.table[1,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(quantMeans(stf@harvest[f01,ImY,c("B","C","D")]),c(2:6,1))
stf.table[1,"Fbar 2-6",]                                    <- quantMeans(unitSums(stf@harvest[f26,ImY,]))
stf.table[1,"Fbar 0-1",]                                    <- quantMeans(unitSums(stf@harvest[f01,ImY,]))
stf.table[1,grep("Catch ",dimnames(stf.table)$values),]     <- aperm(harvestCatch(stf,ImY),c(2:6,1))
stf.table[1,grep("SSB",dimnames(stf.table)$values)[1],]     <- quantSums(stf@stock.n[,ImY,1] * stf@stock.wt[,ImY,1] *
                                                                           exp(-unitSums(stf@harvest[,ImY])*stf@harvest.spwn[,ImY,1]-stf@m[,ImY,1]*stf@m.spwn[,ImY,1]) *
                                                                           stf@mat[,ImY,1])

referencePoints$Fsq <- stf.table["intermediate year","Fbar 2-6","50%"]


#-------------------------------------------------------------------------------
# 6) propagate stock number to forecast year setup
#-------------------------------------------------------------------------------

for(i in dms$unit) stf@stock.n[1,FcY,i]                     <- RECS$FcY
for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),FcY,i]   <- (stf@stock.n[,ImY,1]*exp(-unitSums(stf@harvest[,ImY])-stf@m[,ImY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
for(i in dms$unit) stf@stock.n[dims(stf)$age,FcY,i]         <- apply((stf@stock.n[,ImY,1]*exp(-unitSums(stf@harvest[,ImY])-stf@m[,ImY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)

#-------------------------------------------------------------------------------
# 7a) Fmsy Advice rule transfer
#-------------------------------------------------------------------------------

if("fmsyAR_transfer" %in% stf.options){
  
  caseName <- "fmsyAR_transfer"
  
  res <- fmsyAR_fun_transfer( stf,
                              FuY,
                              TACS,
                              RECS,
                              referencePoints,
                              TAC_var,
                              f01,
                              f26)
  
  # update stf table
  stf.table[caseName,"Fbar 2-6 A",]                                  <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                    <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                    <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]     <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]     <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]     <- res$ssb.CtY
  
  # Save the stf object to an RData file for later comparison
  save(res, file=file.path(outPath,paste0(stfFileName,'_FmsyAR.Rdata')))
}

#-------------------------------------------------------------------------------
# 7a) Fmsy Advice rule with C fleet transfer and 0.05 target for F01
#-------------------------------------------------------------------------------

if("fmsyAR_transfer_Btarget" %in% stf.options){
  
  caseName <- "fmsyAR_transfer_Btarget"

  res <- fmsyAR_fun_transfer_Btarget( stf,
                                      FuY,
                                      TACS,
                                      RECS,
                                      referencePoints,
                                      TAC_var,
                                      f01,
                                      f26)
  
  # update stf table
  stf.table[caseName,"Fbar 2-6 A",]                                  <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                    <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                    <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]     <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]     <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]     <- res$ssb.CtY
  
  # Save the stf object to an RData file for later comparison
}

#-------------------------------------------------------------------------------
# 7a) Fmsy Advice rule no transfer
#-------------------------------------------------------------------------------
#fmsyAR_fun_no_transfer.r
if("fmsyAR_no_transfer" %in% stf.options){
  
  caseName <- "fmsyAR_no_transfer"
  
  res <- fmsyAR_fun_no_transfer(  stf,
                                  FuY,
                                  TACS,
                                  RECS,
                                  referencePoints,
                                  TAC_var,
                                  f01,
                                  f26)
  
  # update stf table
  stf.table[caseName,"Fbar 2-6 A",]                                  <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                    <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                    <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]     <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]     <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]     <- res$ssb.CtY
  
  # Save the stf object to an RData file for later comparison
}

#-------------------------------------------------------------------------------
# 7a) Fmsy Advice rule no transfer with F01=0.05 as target
#-------------------------------------------------------------------------------
#fmsyAR_fun_no_transfer.r
if("fmsyAR_no_transfer_Btarget" %in% stf.options){
  
  caseName <- "fmsyAR_no_transfer_Btarget"
  
  res <- fmsyAR_fun_no_transfer_Btarget(  stf,
                                          FuY,
                                          TACS,
                                          RECS,
                                          referencePoints,
                                          TAC_var,
                                          f01,
                                          f26)
  
  # update stf table
  stf.table[caseName,"Fbar 2-6 A",]                                  <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                    <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                    <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]     <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]     <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]     <- res$ssb.CtY
  
  # Save the stf object to an RData file for later comparison
}

#-------------------------------------------------------------------------------
# 7b) Management plan HCR A
# HCR A without IAV
# Note: no implementation for BB
#-------------------------------------------------------------------------------

if("mpA" %in% stf.options){
  
  caseName <- "mpA"
  
  referencePoints$Ftarget   <- 0.23
  referencePoints$Btrigger  <- 1.5E06
  
  managementRule  <- list(HCR = 'A',
                          TACIAV= NULL, #"A",c("A","B"),"NULL"
                          BB = NULL)
  
  res <- MP_fun(stf,
                FuY,
                CATCH,
                RECS,
                referencePoints,
                managementRule,
                TAC_var,
                TACS,
                f01,
                f26)
  
  ##fill stf.table
  stf.table[caseName,"Fbar 2-6 A",]                                  <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                    <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                    <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]     <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]     <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]     <- res$ssb.CtY
  
}

#-------------------------------------------------------------------------------
# 
# 
# Important step/assumption here!!!!!!!!!!
# we set CATCH for the B fleet to the results from mpA
# 
# 
#-------------------------------------------------------------------------------

#CATCH[,c(FcY,CtY),'B'] <- res$stf@catch[,FcY,'B']

#-------------------------------------------------------------------------------
# 7b) Management plan HCR B
# HCR B without IAV
# Note: no implementation for BB
#-------------------------------------------------------------------------------

if("mpB" %in% stf.options){
  
  caseName <- "mpB"
  
  referencePoints$Ftarget   <- 0.22
  referencePoints$Btrigger  <- 1.4E06
  
  managementRule  <- list(HCR = 'B',
                          TACIAV= NULL, #"A",c("A","B"),"NULL"
                          BB = NULL)
  
  res <- MP_fun(stf,
                FuY,
                CATCH,
                RECS,
                referencePoints,
                managementRule,
                TAC_var,
                TACS,
                f01,
                f26)
  
  ##fill stf.table
  stf.table[caseName,"Fbar 2-6 A",]                                  <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                    <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                    <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]     <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]     <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]     <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7b) Management plan HCR A + C
# HCR A with IAV for A fleet
# Note: no implementation for BB
#-------------------------------------------------------------------------------

if("mpAC" %in% stf.options){
  
  caseName <- "mpAC"
  
  referencePoints$Ftarget   <- 0.23
  referencePoints$Btrigger  <- 1.5E06
  
  managementRule  <- list(HCR = 'A',
                          TACIAV= 'A', #"A",c("A","B"),"NULL"
                          BB = NULL)
  
  res <- MP_fun(stf,
                FuY,
                CATCH,
                RECS,
                referencePoints,
                managementRule,
                TAC_var,
                TACS,
                f01,
                f26)
  
  ##fill stf.table
  stf.table[caseName,"Fbar 2-6 A",]                                  <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                    <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                    <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]     <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]     <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]     <- res$ssb.CtY 
}

#-------------------------------------------------------------------------------
# 7b) Management plan HCR A + D
# HCR A with IAV for A & B fleets
# Note: no implementation for BB
#-------------------------------------------------------------------------------

if("mpAD" %in% stf.options){
  
  caseName <- "mpAD"
  
  referencePoints$Ftarget   <- 0.23
  referencePoints$Btrigger  <- 1.5E06
  
  managementRule  <- list(HCR = 'A',
                          TACIAV= c("A","B"), #"A",c("A","B"),"NULL"
                          BB = NULL)
  
  res <- MP_fun(stf,
                FuY,
                CATCH,
                RECS,
                referencePoints,
                managementRule,
                TAC_var,
                TACS,
                f01,
                f26)
  
  ##fill stf.table
  stf.table[caseName,"Fbar 2-6 A",]                                  <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                    <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                    <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]     <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]     <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]     <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7c) No fishing
#-------------------------------------------------------------------------------

if("nf" %in% stf.options){
  
  caseName <- "nf"
  
  res <- nf_fun(stf,
                RECS,
                FuY)
  
  # update stf table
  stf.table[caseName,"Fbar 2-6 A",]                                 <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                   <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                   <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]    <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]    <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]    <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7d) 15% increase in TAC for the A-fleet
#-------------------------------------------------------------------------------

if("+15%" %in% stf.options){
  
  caseName <- "+15%"
  
  res <- TAC_scaling_fun( stf,
                          FuY,
                          TACS,
                          CATCH,
                          RECS,
                          TAC_var,
                          1.15)

  stf.table[caseName,"Fbar 2-6 A",]                                 <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                   <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                   <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]    <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]    <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]    <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7e) 15% reduction in TAC for the A-fleet
#-------------------------------------------------------------------------------

if("-15%" %in% stf.options){
  
  caseName <- "-15%"
  
  res <- TAC_scaling_fun( stf,
                          FuY,
                          TACS,
                          CATCH,
                          RECS,
                          TAC_var,
                          0.85)
  
  stf.table[caseName,"Fbar 2-6 A",]                                 <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                   <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                   <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]    <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]    <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]    <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7f) Same TAC for A-fleet as last year
#
# for 2015: use estimated B-fleet TAC from mp for the FcY and the NSAS 
# proportion of the advised C-fleet catch in the FcY
#-------------------------------------------------------------------------------

if("tacro" %in% stf.options){
  
  caseName <- "tacro"
  
  res <- TAC_scaling_fun( stf,
                          FuY,
                          TACS,
                          CATCH,
                          RECS,
                          TAC_var,
                          1)
  
  stf.table[caseName,"Fbar 2-6 A",]                                 <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                   <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                   <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]    <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]    <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]    <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7g) Fmsy option
# Note: it is important to have a catch assumption for the B fleet in FcY and CtY
#-------------------------------------------------------------------------------

if("fmsy" %in% stf.options){
  
  caseName <- "fmsy"
  
  res <- F_scaling_fun( stf,
                        FuY,
                        CATCH,
                        RECS,
                        referencePoints,
                        referencePoints$Fmsy,
                        f01,
                        f26)
  
  stf.table[caseName,"Fbar 2-6 A",]                                 <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                   <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                   <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]    <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]    <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]    <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7h) Fpa option
#-------------------------------------------------------------------------------

if("fpa" %in% stf.options){
  
  caseName <- "fpa"
  
  res <- F_scaling_fun( stf,
                        FuY,
                        CATCH,
                        RECS,
                        referencePoints,
                        referencePoints$Fpa,
                        f01,
                        f26)
  
  stf.table[caseName,"Fbar 2-6 A",]                                 <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                   <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                   <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]    <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]    <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]    <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7i) Flim option
#-------------------------------------------------------------------------------

if("flim" %in% stf.options){
  
  caseName <- "flim"
  
  res <- F_scaling_fun( stf,
                        FuY,
                        CATCH,
                        RECS,
                        referencePoints,
                        referencePoints$Flim,
                        f01,
                        f26)
  
  stf.table[caseName,"Fbar 2-6 A",]                                 <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                   <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                   <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]    <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]    <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]    <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7j) Fsq option
#-------------------------------------------------------------------------------

if("fsq" %in% stf.options){
  
  caseName <- "fsq"
  
  res <- F_scaling_fun( stf,
                        FuY,
                        CATCH,
                        RECS,
                        referencePoints,
                        referencePoints$Fsq,
                        f01,
                        f26)
  
  stf.table[caseName,"Fbar 2-6 A",]                                 <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                   <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                   <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]    <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]    <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]    <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7k) Bpa option
#-------------------------------------------------------------------------------

if("bpa" %in% stf.options){
  
  caseName <- "bpa"
  
  res <- B_scaling_fun( stf,
                        FuY,
                        CATCH,
                        RECS,
                        referencePoints,
                        referencePoints$Bpa)
  
  stf.table[caseName,"Fbar 2-6 A",]                                 <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                   <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                   <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]    <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]    <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]    <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7l) Blim option
#-------------------------------------------------------------------------------

if("blim" %in% stf.options){
  
  caseName <- "blim"
  
  res <- B_scaling_fun( stf,
                        FuY,
                        CATCH,
                        RECS,
                        referencePoints,
                        referencePoints$Blim)
  
  stf.table[caseName,"Fbar 2-6 A",]                                 <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                   <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                   <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]    <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]    <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]    <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7m) MSYBtrigger option
#-------------------------------------------------------------------------------

if("MSYBtrigger" %in% stf.options){
  
  caseName <- "MSYBtrigger"
  
  res <- B_scaling_fun( stf,
                        FuY,
                        CATCH,
                        RECS,
                        referencePoints,
                        referencePoints$MSYBtrigger)
  
  stf.table[caseName,"Fbar 2-6 A",]                                 <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                   <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                   <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]    <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]    <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]    <- res$ssb.CtY
}

#-------------------------------------------------------------------------------
# 7n) Expand the Fmsy AR rule a few more years
#-------------------------------------------------------------------------------

# First run the Fmsy AR again
res <- fmsyAR_fun_no_transfer(stf,
                              FuY,
                              TACS,
                              RECS,
                              referencePoints,
                              TAC_var,
                              f01,
                              f26)



# set up the stf2 object  
stf2  <- window(res[["stf"]],start=an(dms$year)[1],end=(4+rev(an(dms$year))[1]))
FuY2  <- ac((an(CtY)+1):(an(CtY)+4))
stf2@harvest[,CtY]  <- stf2@harvest[,FcY]
stf2@harvest[,FuY2] <- stf2@harvest[,FcY]

# fill the slots
for(i in c("stock.wt", "catch.wt", "mat", "m", "harvest.spwn", "m.spwn")){
  slot(stf2,i)[,FuY2] <- slot(stf2,i)[,CtY]
}

# fill the recruitment
stf2@stock.n["0",FuY2] <- stf2@stock.n["0",CtY]

# calculate catch in 2021
for(i in dms$unit){
  stf2@catch.n[,CtY,i]     <- stf2@stock.n[,CtY,i]*(1-exp(-unitSums(stf2@harvest[,CtY])-stf2@m[,CtY,i]))*(stf2@harvest[,CtY,i]/(unitSums(stf2@harvest[,CtY])+stf2@m[,CtY,i]))
  stf2@catch[,CtY,i]       <- computeCatch(stf2[,CtY,i])
}

# calculate stock and catch in future years
for(y in FuY2) {
  j <- an(y)
  
  # age 2 to plusgroup-1
  stf2@stock.n[2:(dims(stf2)$age-1),y]   <- 
    (stf2@stock.n[,ac(j-1),1]*exp(-unitSums(stf2@harvest[,ac(j-1),])-stf2@m[,ac(j-1),1]))[ac(range(stf2)["min"]:(range(stf2)["max"]-2))]
  
  # plusgroup
  stf2@stock.n[dims(stf2)$age,y]         <- 
    apply((stf2@stock.n[,ac(j-1),1]*
             exp(-unitSums(stf2@harvest[,ac(j-1)])-stf2@m[,ac(j-1),1]))[ac((range(stf2)["max"]-1):range(stf2)["max"]),],2:6,sum,na.rm=T)
  #ssb
  stf2@stock[,y] <- quantSums( stf2@stock.n[,y,1] * stf2@stock.wt[,y,1] *
                                 exp(-unitSums(stf2@harvest[,y])*stf2@harvest.spwn[,y,1]-stf2@m[,y,1] *
                                       stf2@m.spwn[,y,1]) * stf2@mat[,y,1])[,y,1]
  
  
  for(i in dms$unit){
    stf2@catch.n[,y,i]     <- stf2@stock.n[,y,i]*(1-exp(-unitSums(stf2@harvest[,y])-stf2@m[,y,i]))*(stf2@harvest[,y,i]/(unitSums(stf2@harvest[,y])+stf2@m[,y,i]))
    stf2@catch[,y,i]       <- computeCatch(stf2[,y,i])
  }
}

# calculate stock size
stf2@stock[,] <- quantSums( stf2@stock.n[,,1] * stf2@stock.wt[,,1] *
                              exp(-unitSums(stf2@harvest[,])*stf2@harvest.spwn[,,1]-stf2@m[,,1] *
                                    stf2@m.spwn[,,1]) * stf2@mat[,,1])[,,1]



# convert to dataframe
stf2.df <- as.data.frame(stf2) %>% 
#   bind_rows(., mutate(as.data.frame(fbar(stf2)[,,1]), slot="FA2-6")) %>% 
  bind_rows(., mutate(as.data.frame(quantMeans(unitSums(stf2@harvest[f26,,1]))), slot="FA2-6")) %>% 
  bind_rows(., mutate(as.data.frame(rec(stf2)[,,1]), slot="rec", age=as.character(age)))

# quantMeans(unitSums(res$stf@harvest[f26,FcY,]))

# plot
# windows()
stf2.df %>% 
  filter(slot %in% c("stock","catch", "FA2-6", "rec"), 
         unit=="A") %>%
  
  ggplot(aes(x=year,y=data)) +
  theme_publication() +
  theme(legend.position = "none") +
  geom_vline(xintercept=2021, linetype="dashed", size=1, colour="darkgray") +
  geom_line(aes(colour=slot), size=1) +
  labs(x="", y="") +
  expand_limits(y=0) +
  facet_wrap(~slot, scales="free_y")

# savePlot(paste(figure.path,"/",stf_plot_names,"_MT_0_stf_MSYAR_projection.png",sep = ""),type="png")
ggsave(file=file.path(outPath,
                      paste0(stfFileName,"_MT_0_stf_MSYAR_projection.png")),
       width=30, height=16, units="cm", device="png", dpi=300)


#-------------------------------------------------------------------------------
# 8) Save outputs
#-------------------------------------------------------------------------------


# Save the output to an RData file
save(stf, stf.table, file=file.path(outPath,paste0(stfFileName,'.Rdata')))
# save(stf, stf.table, file="ShortTermForecast multifleetmode.RData")

#- Writing the STF to file
for(i in c("catch","catch.n","stock.n","harvest")){
  slot(stf,i)[,FcY] <- ""
  slot(stf,i)[,CtY] <- ""
}

options("width"=80,"scipen"=1000)
stf.out.file <- stf.out(stf,RECS,format="TABLE 2.7.%i NORTH SEA HERRING.")
write(stf.out.file,file=file.path(outPath,paste0(stfFileName,'.out')))

#- Write the stf.table to file
write.csv(stf.table[,,2],
          file=file.path(outPath,paste0(stfFileName,'.csv')))
#           file=paste0("stf.table_mf_","deterministic.csv"))

# table of longer term forecast
stf2.df %>% 
  filter(slot %in% c("stock","catch", "FA2-6", "rec"), 
         unit=="A") %>%
  filter(year >=2019) %>% 
  dplyr::select(slot, year, data) %>% 
  spread(key=slot, value = data) %>% 
  pandoc.table(., 
               style        = "simple",
               split.tables = 200, 
               justify      = "right",
               missing      =" ",
               big.mark     = '', 
               round        = c(0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))


# Table of assumptions
# as.data.frame(CATCH)




