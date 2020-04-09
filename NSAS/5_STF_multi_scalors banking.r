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
# 08/04/2020 added a banking option (removed all other forecasts)
#-------------------------------------------------------------------------------

rm(list=ls());

library(FLCore)
library(FLSAM)
library(minpack.lm)  # install.packages("minpack.lm")
require(msm)         # install.packages("msm")

library(tidyverse)
library(pander)

# source publication theme
source("../_common/theme_publication.r")


#-------------------------------------------------------------------------------
# 1) read data
#    setup paths
#   load mf and sf objects and stf functions
#-------------------------------------------------------------------------------

#path <- "C:/git/wg_HAWG/NSAS/"
path <- "D:/GIT/wg_HAWG/NSAS/"

try(setwd(path),silent=FALSE)
 
dir.create("stf",showWarnings = FALSE)

assessment.dir <- file.path(".","assessment")

dataPath      <- file.path(".","data")
outPath       <- file.path(".","stf")
functionPath  <- file.path(".","functions")

load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2020 Meeting Docs/06. Data/her.27.3a47d/NSH_HAWG2020_sf.Rdata")
load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2020 Meeting Docs/06. Data/her.27.3a47d/NSH_HAWG2020_mf.Rdata")
#load(file=file.path(assessment.dir,"NSH_HAWG2020_sf.RData"))
#load(file=file.path(assessment.dir,"NSH_HAWG2020_mf.RData"))
# load(file=file.path(assessment.dir,"NSH_HAWG2020_sf.RData"))
# load(file=file.path(assessment.dir,"NSH_HAWG2020_mf.RData"))

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
TAC_CD_advice   <- FALSE

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

stf.options <- c('intermediate year_real',
                 'fmsyAR_transfer',
                 'fmsyAR_transfer_real')


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

# Banking information
BANK        <- FLQuant(NA,dimnames=list(age="all",year=FuY,unit=c("A","B","C","D"),
                                        season="all",area=1,iter=1:dims(NSH)$iter))
BANK[,ImY,'A']     <- 0.25
BANK[,ImY,'B']     <- 0
BANK[,ImY,'C']     <- 0
BANK[,ImY,'D']     <- 0

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

# ---------------------------------------------------------------------
# Create an stf_real object (realized stock instead of predicted stock) 
# ---------------------------------------------------------------------

stf_real        <- FLStock(name=nam,
                       desc=dsc,
                       m=FLQuant(NA,
                                 dimnames=dms))

# copy fields from current assessment to all fleets
for(i in dms$unit)
  stf_real[,,i]  <- window(NSH,start=an(dms$year)[1],end=rev(an(dms$year))[1])

units(stf_real)  <- units(NSH)

# Fill slots that are the same for all fleets
for(i in c(unlist(yrs1),unlist(yrs3),unlist(yrs5))){
  if(i %in% unlist(yrs1)){ for(j in dms$unit){ slot(stf_real,i)[,FuY,j] <- slot(NSH,i)[,DtY]}}
  if(i %in% unlist(yrs3)){ for(j in dms$unit){ slot(stf_real,i)[,FuY,j] <- yearMeans(slot(NSH,i)[,ac((an(DtY)-2):an(DtY))])}}
  if(i %in% unlist(yrs5)){ for(j in dms$unit){ slot(stf_real,i)[,FuY,j] <- yearMeans(slot(NSH,i)[,ac((an(DtY)-4):an(DtY))])}}
}

# taking data from multifleet assessment
for(i in 1:length(dms$unit)){
  
  # Fill the D fleet with the data from the B fleet; same selection pattern
  if(i == 4) {
    stf_real@harvest[,ac(an(dms$year)[1]:(an(dms$year)[1]+2)),4]     <- NSH3f.sam@harvest[,ac(an(dms$year)[1]:(an(dms$year)[1]+2)),,,2]
    stf_real@harvest[,FuY,4]     <- NSH3f.sam@harvest[,ImY,,,2]
    stf_real@catch.wt[,FuY,4]    <- NSHs3[["residual"]]@catch.wt[,DtY,,,2]
    stf_real@landings.wt[,FuY,4] <- NSHs3[["residual"]]@catch.wt[,DtY,,,2]
  }
  
  if(i != 4) {
    stf_real@harvest[,ac(an(dms$year)[1]:(an(dms$year)[1]+2)),i]     <- NSH3f.sam@harvest[,ac(an(dms$year)[1]:(an(dms$year)[1]+2)),,,i]
    stf_real@harvest[,FuY,i]     <- NSH3f.sam@harvest[,ImY,,,i]
    stf_real@catch.wt[,FuY,i]    <- NSHs3[["residual"]]@catch.wt[,DtY,,,i]
    stf_real@landings.wt[,FuY,i] <- NSHs3[["residual"]]@catch.wt[,DtY,,,i]
  }
}

# Fill slots that have no meaning for NSAS
stf_real@discards.n[]          <- 0
stf_real@discards[]            <- 0
stf_real@discards.wt[]         <- 0

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

# And now for the real stock -----------------------------

for(i in dms$unit)
  stf_real@stock.n[,ImY,i]     <- NSH.sam@stock.n[,ImY]

# computing F 
stf_real@harvest[,ImY]         <- fleet.harvest(stk=stf_real,
                                           iYr=ImY,
                                           CATCH=CATCH[,ImY] * (1-BANK[,ImY])) 
for(i in dms$unit){
  stf_real@catch.n[,ImY,i]     <- stf_real@stock.n[,ImY,i]*(1-exp(-unitSums(stf_real@harvest[,ImY])-stf_real@m[,ImY,i]))*(stf_real@harvest[,ImY,i]/(unitSums(stf_real@harvest[,ImY])+stf_real@m[,ImY,i]))
  stf_real@catch[,ImY,i]       <- computeCatch(stf_real[,ImY,i])
  stf_real@landings.n[,ImY,i]  <- stf_real@catch.n[,ImY,i]
  stf_real@landings[,ImY,i]    <- computeLandings(stf_real[,ImY,i])
}

stf.table[2,"Fbar 2-6 A",]                                  <- quantMeans(stf_real@harvest[f26,ImY,"A"])
stf.table[2,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(quantMeans(stf_real@harvest[f01,ImY,c("B","C","D")]),c(2:6,1))
stf.table[2,"Fbar 2-6",]                                    <- quantMeans(unitSums(stf_real@harvest[f26,ImY,]))
stf.table[2,"Fbar 0-1",]                                    <- quantMeans(unitSums(stf_real@harvest[f01,ImY,]))
stf.table[2,grep("Catch ",dimnames(stf.table)$values),]     <- aperm(harvestCatch(stf_real,ImY),c(2:6,1))
stf.table[2,grep("SSB",dimnames(stf.table)$values)[1],]     <- quantSums(stf_real@stock.n[,ImY,1] * stf_real@stock.wt[,ImY,1] *
                                                                           exp(-unitSums(stf_real@harvest[,ImY])*stf_real@harvest.spwn[,ImY,1]-stf_real@m[,ImY,1]*stf_real@m.spwn[,ImY,1]) *
                                                                           stf_real@mat[,ImY,1])

#-------------------------------------------------------------------------------
# 6) propagate stock number to forecast year setup
#-------------------------------------------------------------------------------

for(i in dms$unit) stf@stock.n[1,FcY,i]                             <- RECS$FcY
for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),FcY,i]           <- (stf@stock.n[,ImY,1]*exp(-unitSums(stf@harvest[,ImY])-stf@m[,ImY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
for(i in dms$unit) stf@stock.n[dims(stf)$age,FcY,i]                 <- apply((stf@stock.n[,ImY,1]*exp(-unitSums(stf@harvest[,ImY])-stf@m[,ImY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)

for(i in dms$unit) stf_real@stock.n[1,FcY,i]                        <- RECS$FcY
for(i in dms$unit) stf_real@stock.n[2:(dims(stf_real)$age-1),FcY,i] <- (stf_real@stock.n[,ImY,1]*exp(-unitSums(stf_real@harvest[,ImY])-stf_real@m[,ImY,1]))[ac(range(stf_real)["min"]:(range(stf_real)["max"]-2)),]
for(i in dms$unit) stf_real@stock.n[dims(stf_real)$age,FcY,i]       <- apply((stf_real@stock.n[,ImY,1]*exp(-unitSums(stf_real@harvest[,ImY])-stf_real@m[,ImY,1]))[ac((range(stf_real)["max"]-1):range(stf_real)["max"]),],2:6,sum,na.rm=T)

#-------------------------------------------------------------------------------
# 7a) Fmsy Advice rule transfer calculated without banking, but with banking applied
#-------------------------------------------------------------------------------

caseName <- "fmsyAR_transfer"

# calculate the standard fmsy AR option
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


# now add the banked quantities back to the quota for the foreast year
caseName <- "fmsyAR_transfer_real"

stf_real@harvest[,FcY]         <- fleet.harvest(stk=stf_real,
                                                iYr=FcY,
                                                CATCH=res$stf@catch[,FcY] + (CATCH[,ImY] * BANK[,ImY]) ) 

for(i in dms$unit){
  stf_real@catch.n[,FcY,i]     <- stf_real@stock.n[,FcY,i]*(1-exp(-unitSums(stf_real@harvest[,FcY])-stf_real@m[,FcY,i]))*(stf_real@harvest[,FcY,i]/(unitSums(stf_real@harvest[,FcY])+stf_real@m[,FcY,i]))
  stf_real@catch[,FcY,i]       <- computeCatch(stf_real[,FcY,i])
  stf_real@landings.n[,FcY,i]  <- stf_real@catch.n[,FcY,i]
  stf_real@landings[,FcY,i]    <- computeLandings(stf_real[,FcY,i])
  }

# Propagate stock numbers to continuation year
for(i in dms$unit) stf_real@stock.n[1,CtY,i]                        <- RECS$CtY
for(i in dms$unit) stf_real@stock.n[2:(dims(stf_real)$age-1),CtY,i] <- (stf_real@stock.n[,FcY,1]*exp(-unitSums(stf_real@harvest[,FcY])-stf_real@m[,FcY,1]))[ac(range(stf_real)["min"]:(range(stf_real)["max"]-2)),]
for(i in dms$unit) stf_real@stock.n[dims(stf_real)$age,CtY,i]       <- apply((stf_real@stock.n[,FcY,1]*exp(-unitSums(stf_real@harvest[,FcY])-stf_real@m[,FcY,1]))[ac((range(stf_real)["max"]-1):range(stf_real)["max"]),],2:6,sum,na.rm=T)


stf.table[caseName,"Fbar 2-6 A",]                                  <- quantMeans(stf_real@harvest[f26,FcY,"A"])
stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(quantMeans(stf_real@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
stf.table[caseName,"Fbar 2-6",]                                    <- quantMeans(unitSums(stf_real@harvest[f26,FcY,]))
stf.table[caseName,"Fbar 0-1",]                                    <- quantMeans(unitSums(stf_real@harvest[f01,FcY,]))
stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]     <- stf_real@catch[,FcY]
stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]     <- quantSums(stf_real@stock.n[,FcY,1] * stf_real@stock.wt[,FcY,1] *
                                                                                  exp(-unitSums(stf_real@harvest[,FcY])*stf_real@harvest.spwn[,FcY,1]-stf_real@m[,FcY,1]*stf_real@m.spwn[,FcY,1]) *
                                                                                  stf_real@mat[,FcY,1])
stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]     <- quantSums(stf_real@stock.n[,CtY,1] * stf_real@stock.wt[,CtY,1] *
                                                                                  exp(-unitSums(stf_real@harvest[,FcY])*stf_real@harvest.spwn[,CtY,1]-stf_real@m[,CtY,1]*stf_real@m.spwn[,CtY,1]) *
                                                                                  stf_real@mat[,CtY,1])


#-------------------------------------------------------------------------------
# 8) Save outputs
#-------------------------------------------------------------------------------

# Save the output to an RData file
save(stf, stf_real, stf.table, file=file.path(outPath,paste0(stfFileName,"_BANK",'.Rdata')))

#- Writing the STF to file
# for(i in c("catch","catch.n","stock.n","harvest")){
#   slot(stf,i)[,FcY] <- ""
#   slot(stf,i)[,CtY] <- ""
# }

# options("width"=80,"scipen"=1000)
# stf.out.file <- stf.out(stf,RECS,format="TABLE 2.7.%i NORTH SEA HERRING.")
# write(stf.out.file,file=file.path(outPath,paste0(stfFileName,'.out')))

#- Write the stf.table to file
write.csv(stf.table[,,2],
          file=file.path(outPath,paste0(stfFileName,"_BANK",'.csv')))
#           file=paste0("stf.table_mf_","deterministic.csv"))





