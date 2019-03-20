#-------------------------------------------------------------------------------
# Code to do the multifleet short term forecast for North Sea Herring
#
# By: Benoit Berges
# WMR
# 12/03/2019
# 
# 
#-------------------------------------------------------------------------------

rm(list=ls());

library(FLCore)
library(FLSAM)
library(minpack.lm)  # install.packages("minpack.lm")
require(msm)         # install.packages("msm")

#-------------------------------------------------------------------------------
# 1) read data
#    setup paths
#   load mf and sf objects and stf functions
#-------------------------------------------------------------------------------

#path <- "C:/git/wg_HAWG/NSAS/"
path <- "D:/GIT/wg_HAWG/NSAS/"

try(setwd(path),silent=FALSE)
output.dir <- file.path(".","results")

dataPath      <- file.path(".","data/")
outPath       <- file.path(".","stf/results/")
functionPath  <- file.path(".","stf/functions/")
scriptPath    <- file.path(".","stf/side_script/")

load(file=file.path(output.dir,"NSH_HAWG2018_sf.RData"))
load(file=file.path(output.dir,"NSH_HAWG2018_mf.RData"))

# ImY forecast functions
source(file.path(functionPath,"fleet.harvest.r"))
source(file.path(functionPath,"rescaleF.r"))

# fmsyAR function
source(file.path(functionPath,"fmsyAR_fun.r"))
source(file.path(functionPath,"find.FCAR.r"))

# MP functions
source(file.path(functionPath,"find.FAB_HCRA.r"))
source(file.path(functionPath,"find.FAB_HCRB.r"))
source(file.path(functionPath,"CATCH2sel.r"))     # Find F that matches a certain catch
source(file.path(functionPath,"MP_fun.r"))        # Management plan function

# no fishing function
source(file.path(functionPath,"nf_fun.r"))

# TAC scaling functions
source(file.path(functionPath,"TAC_scaling_fun.r"))

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

stf.options <- c("mpA",      # HCR A (F declines linearly to zero below Btrigger)
                 "mpAC",     # HCR A with TAC constraint
                 "mpAD",     # HCR A with TAC constraint and banking and borrowing
                 "mpB",      # HCR B (F declines below Btrigger and is kept at fixed value below Blim)
                 "tacro",    # TAC Roll Over (same as in ImY)
                 "-15%",
                 "+15%",
                 "fmsy",
                 "fmsyAR",
                 "fpa",
                 "flim",
                 "fsq",
                 "bpa",
                 "blim",
                 "MSYBtrigger",
                 "nf")
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
                        F01   = 0.05, # target F for juveniles ages 0 and 1 WR
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
TAC_var$WBSS_NSAS   <- splitTab[ImY,'WBSS_NSAS']

# Ctransfer. Transfer of TAC from IIIa to IVa for C fleet in assessment year
# Note: C transfer in table is the percentage transferred to NSAS from WBSS
CtransferTab            <- read.table(file.path(dataPath,'TAC_var','NSAS_C_transfer.csv'),sep = ",",header=T)
rownames(CtransferTab)  <- CtransferTab[,1]
TAC_var$Ctransfer       <- CtransferTab[ImY,2]

# B & D fleets uptakes (Henrik Mosegaard)
# What should we assume for the uptake?
uptakeTab               <- read.table(file.path(dataPath,'TAC_var','NSAS_uptake.csv'),sep = ",",header=T)
rownames(uptakeTab)     <- uptakeTab[,1]
TAC_var$Buptake         <- uptakeTab[DtY,'B']   # Uptake of Bfleet TAC in the terminal year
TAC_var$Duptake         <- uptakeTab[DtY,'D']   # Uptake of the Dfleet TAC in the terminal year

############## TAC for the different fleets in the intermediate year ##############
# TAC A is TACNSA; HER/4AB. + HER/4CXB7D 
# TAC B is TACNSB; HER/2A47DX 
# TAC C is TAC3aC; HER/03A.
# TAC D is TAC3aD; HER/03A-BC

# TAC information for 2018
TACTab            <- read.table(file.path(dataPath,'TAC_var','NSAS_TAC.csv'),sep = ",",header=T)
rownames(TACTab)  <- TACTab[,1]

# Create TAC objects for current year, forecast year and last year
TACS        <- FLQuant(NA,dimnames=list(age="all",year=FuY,unit=c("A","B","C","D"),
                                        season="all",area=1,iter=1:dims(NSH)$iter))

TACS[,ImY,'A'] <- TACTab[ImY,'A']
TACS[,ImY,'B'] <- TACTab[ImY,'B']
# 20522.38

# assume 0 TAC for FcY and CtY for C and D fleet
TACS[,ImY,'C'] <- TACTab[ImY,'C']
TACS[,c(FcY,CtY),'C'] <- 0.1
TACS[,ImY,'D'] <- TACTab[ImY,'D']
TACS[,c(FcY,CtY),'D'] <- 0.1

############## realised catches ##############
CATCH        <- FLQuant(NA,dimnames=list( age="all",year=FuY,unit=c("A","B","C","D"),
                                          season="all",area=1,iter=1:dims(NSH)$iter))

CATCH[,ImY,'A'] <- TACS[,ImY,'A'] + TAC_var$Ctransfer*TACS[,ImY,'C'] - (TACS[,ImY,'A'] + TAC_var$Ctransfer*TACS[,ImY,'C'])*TAC_var$WBSS_NSAS
CATCH[,ImY,'B'] <- TACS[,ImY,'B']#*TAC_var$Buptake
CATCH[,ImY,'C'] <- TACS[,ImY,'C']*(1-TAC_var$Ctransfer)*0.3#TAC_var$Csplit
CATCH[,ImY,'D'] <- TACS[,ImY,'D']*0.46*0.6#*TAC_var$Dsplit*TAC_var$Duptake

# 0 catch in FcY and CtY
CATCH[,c(FcY,CtY),'C'] <- 0.1#TACS[,ImY,'C']*(1-TAC_var$Ctransfer)*TAC_var$Csplit
CATCH[,c(FcY,CtY),'D'] <- 0.1#TACS[,ImY,'D']*TAC_var$Dsplit*TAC_var$Duptake

#-------------------------------------------------------------------------------
# 4) Recruitment for intermediate, forecast and continuation years
#    ImY recruitment: comes from sf assessment (informed by IBTS0)
#    FcY and CtY: weighted average over 10 year using uncertainty as weighting
#-------------------------------------------------------------------------------
# Retrieve uncertainty estimate on recruitment estimates by the model for weighted average
recWeights  <- subset(NSH.sam@params, name=="logR")$std.dev^2

# fill in recruitment for the different years
RECS <- FLQuants( "ImY" =FLQuant(subset(rec(NSH.sam),year==ImY)$value),
                  "FcY" =exp(apply(log(rec(NSH)[,ac((an(DtY)-10):DtY)]),3:6,weighted.mean,w=1/rev(rev(recWeights)[2:12]),na.rm=T)),
                  "CtY" =exp(apply(log(rec(NSH)[,ac((an(DtY)-10):DtY)]),3:6,weighted.mean,w=1/rev(rev(recWeights)[2:12]),na.rm=T)))


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


# Temporary fix to set the B fleet CATCH; This was previously derived from the management plan option. 
CATCH[,c(FcY,CtY),'B'] <- 20522.38

#-------------------------------------------------------------------------------
# 7b) Management plan HCR A
# HCR A without IAV
# Note: no implementation for BB
#-------------------------------------------------------------------------------

if("mpA" %in% stf.options){
  
  caseName <- "mpA"
  
  referencePoints$Ftarget   <- 0.24
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
                TACS)
  
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
# 7b) Management plan HCR B
# HCR B without IAV
# Note: no implementation for BB
#-------------------------------------------------------------------------------

if("mpB" %in% stf.options){
  
  caseName <- "mpB"
  
  referencePoints$Ftarget   <- 0.24
  referencePoints$Btrigger  <- 1.5E06
  
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
                TACS)
  
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
  
  referencePoints$Ftarget   <- 0.24
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
                TACS)
  
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
  
  referencePoints$Ftarget   <- 0.24
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
                TACS)
  
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
# 7a) Fmsy Advice rule
#-------------------------------------------------------------------------------

if("fmsyAR" %in% stf.options){
  
  caseName <- "fmsyAR"
  
  source(file.path(functionPath,"fmsyAR_fun.r"))
  source(file.path(functionPath,"find.FCAR.r"))
  
  res <- fmsyAR_fun(stf,
                    FuY,
                    CATCH,
                    RECS,
                    referencePoints)
  
  # update stf table
  stf.table[caseName,"Fbar 2-6 A",]                                  <- quantMeans(res$stf@harvest[f26,FcY,"A"])
  stf.table[caseName,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(quantMeans(res$stf@harvest[f01,FcY,c("B","C","D")]),c(2:6,1))
  stf.table[caseName,"Fbar 2-6",]                                    <- quantMeans(unitSums(res$stf@harvest[f26,FcY,]))
  stf.table[caseName,"Fbar 0-1",]                                    <- quantMeans(unitSums(res$stf@harvest[f01,FcY,]))
  stf.table[caseName,grep("Catch ",dimnames(stf.table)$values),]     <- res$stf@catch[,FcY]
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[1],]     <- res$ssb.FcY
  stf.table[caseName,grep("SSB",dimnames(stf.table)$values)[2],]     <- res$ssb.CtY
  
  # Save the stf object to an RData file for later comparison
  save(res, file=file.path(outPath, paste0("STF",ImY," FmsyAR.RData")))
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

CATCH[,c(FcY,CtY),'B'] <- 20522.38

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
                        referencePoints$Fmsy)
  
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
                        referencePoints$Fpa)
  
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
                        referencePoints$Flim)
  
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
                        referencePoints$Fsq)
  
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
# 8) Save outputs
#-------------------------------------------------------------------------------


# Save the output to an RData file
save(stf, stf.table, file=file.path(outPath,"ShortTermForecast multifleetmode_f01WBSS.RData"))
# save(stf, stf.table, file="ShortTermForecast multifleetmode.RData")

#- Writing the STF to file
for(i in c("catch","catch.n","stock.n","harvest")){
  slot(stf,i)[,FcY] <- ""
  slot(stf,i)[,CtY] <- ""
}

options("width"=80,"scipen"=1000)
stf.out.file <- stf.out(stf,RECS,format="TABLE 2.7.%i NORTH SEA HERRING.")
write(stf.out.file,file=paste("./","stf_mf.out",sep="."))
write(stf.out.file,file=file.path(outPath,"stf_mf.out"))

#- Write the stf.table to file
write.csv(stf.table[,,2],
          file=file.path(outPath,"stf.table_mf_deterministic_old_way.csv"))
#           file=paste0("stf.table_mf_","deterministic.csv"))


