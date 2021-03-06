#-------------------------------------------------------------------------------
# Code to do the multifleet short term forecast for North Sea Herring
#
# By: Niels Hintzen
# IMARES
# 2 January 2012
# 
# 02/04/2016 Martin Pastoors
# 16/03/2018 Martin Pastoors; Multifleet data + adapted for mpOption15.r that 
#            takes into account the F when SSB in the current year is below management
#            plan trigger. 
# 08/05/2018 Updated with new WBSS catch during ADG
#-------------------------------------------------------------------------------

rm(list=ls());

library(FLCore)
library(FLSAM)
library(minpack.lm)  # install.packages("minpack.lm")
require(msm)         # install.packages("msm")
#install.packages("C:/DATA/R/FLAssess-master.zip")

#Read in data                           1

# path <- "D:/Repository/HAWG/wg_HAWG.git/trunk/NSAS/"
# path <- "C:/DATA/GIT/HAWG/NSAS/"
# path <- "D:/Repository/ICES_HAWG/wg_HAWG/NSAS/"
#path <- "D:/git/wg_HAWG/NSAS/"
path <- "C:/git/wg_HAWG/NSAS/"

try(setwd(path),silent=FALSE)
output.dir <- file.path(".","results")

#load(file=file.path(output.dir,"North Sea Herring.RData"))
load(file=file.path(output.dir,"NSH_HAWG2018_sf.RData"))
load(file=file.path(output.dir,"NSH_HAWG2018_mf.RData"))

#load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2018 Meeting docs1/09. Personal Folders/Benoit/NSH_final.RData")
try(setwd("./stf/"))

#-------------------------------------------------------------------------------
# Setup control file
#-------------------------------------------------------------------------------

DtY   <- ac(range(NSH)["maxyear"]) #Data year
ImY   <- ac(an(DtY)+1)             #Intermediate year
FcY   <- ac(an(DtY)+2)             #Forecast year
CtY   <- ac(an(DtY)+3)             #Continuation year
FuY   <- c(ImY,FcY,CtY)            #Future years

#- Source the code to read in the weights at age and numbers at age by fleet, to compute partial F's and partial weights
source("stfFunctions_2018.r")
source("writeSTF.out.r")


#- Generate multi-iters opbject
  #Deterministic
NSH@stock.n <- NSH.sam@stock.n[,ac(range(NSH)["minyear"]:range(NSH)["maxyear"])]
NSH@harvest <- NSH.sam@harvest[,ac(range(NSH)["minyear"]:range(NSH)["maxyear"])]

stk     <- NSH
stk.sam <- NSH.sam

#- Set reference points (Fstatus-quo will be filled after intermediate year
referencePoints <- list(Fmsy = 0.26,
                        Fsq  = NA,
                        Flim = 0.34,
                        Fpa  = 0.30,
                        Blim = 800000,
                        Bpa  = 900000,
                        MSYBtrigger = 1400000)

#-------------------------------------------------------------------------------
# TAC information
#
#   For the TAC's there is some special things going on. The A-fleet, you can get the TAC from the agreed management plan between Norway-EU. However, there might be catches from Norway from the C-fleet
#   which they are allowed to take in the North Sea. So, substract these catches (which are a percentage of their total C-fleet quota) from the total C-fleet quota and move that into the North Sea
#   Then split the C-fleet and D-fleet into parts from the WBSS and NSH
#   The B-fleet is the bycatch in the North Sea
#   Where to find the information: TAC A-Fleet: Agreement EU-Norway North Sea --> table
#                                  TAC B-Fleet: Agreement EU-Norway North Sea --> in text
#                                  TAC C-Fleet: Agreement EU-Norway Kattegat - Skaggerak --> table
#                                  TAC D-Fleet: Agreement EU-Norway Kattegat - Skaggerak --> in text
#                                  Transfer C-Fleet to A-fleet: Agreement EU-Norway Kattegat - Skaggerak --> in text, and combination with TAC of C-Fleet of Norway
#   In the A fleet, also WBSS are caught (under the NSAS TAC) which need to be substracted from the A-fleet intermediate year catch expectation
#   Split the C-Fleet and D-Fleet into the WBSS and NSH fraction of the TAC (after taking the transfer from Norway from the C-Fleet)
#   Split information comes from the 'Wonderful Table'
#   Split is based on parts taken in the history
#   TAC's for the C and D fleet for the Forecast and Continuation year come from the Danish. Or:
#   Compute the fraction of NSH caught by the C fleet from the total C fleet catch (wonderful table), the same for the D-fleet
#   Then calculate the proportion of the C fleet that was catching WBSS. Then take the forecasted TAC and
#   multiply it by the C fleet share in WBSS, then divide it by 1-fraction NSH and then times the fraction of NSH. This gives you the TAC's for the C and D Fleet
#   However, this might be changed by the Danish
#-------------------------------------------------------------------------------

# TAC information for 2018
TACNSA      <- 600588  # taken from TAC regulation document HER/4AB. + HER/4CXB7D
TACNSB      <- 9669    # taken from TAC regulation document HER/2A47DX
TAC3aC      <- 48427   # HER/03A. Split
TAC3aD      <- 6659    # HER/03A-BC

# Set Dummy catch
dummy       <- 0.1

# Splits and transfers
Csplit      <- 0.30    # Proportion NSAS in C fleet catch; 3 year average (from WBSS assessment)
Dsplit      <- 0.60    # Proportion NSAS in D fleet catch; 3 year average (from WBSS assessment)
Ctransfer   <- 0.46    # Transfer of TAC from IIIa to IVa for C fleet in assessment year

# WBSScatch   <- 13155 #26849   # Recommended MSY catch for WBSS herring; from Henrik
# WBSScatch   <- dummy   # Similar to zero catch
WBSScatch   <- 13155   # Recommended catch (F=0.1)for WBSS herring; ADG 2018

transfer    <- 0.46    # Assumed transfer of C-fleet TAC into A-fleet

Buptake     <- 1       # Uptake of Bfleet TAC in the previous year
Duptake     <- 0.46    # Uptake of the Dfleet TAC in the previous year (3 year average)

WBSSsplit   <- 0.005   # 3 year average proportion WBSS caught in North Sea

# Create TAC objects for current year, forecast year and last year
TACS        <- FLQuant(NA,dimnames=list(age="all",year=FuY,unit=c("A","B","C","D"),
                                        season="all",area=1,iter=1:dims(stk)$iter))
TACS.orig   <- TACS
TACS.orig[,,"A"] <- c(TACNSA,NA,NA)
TACS.orig[,,"B"] <- c(TACNSB,NA,NA)
TACS.orig[,,"C"] <- c(TAC3aC,NA,NA)
TACS.orig[,,"D"] <- rep(TAC3aD,3)

# Fill TAC objects with data on expected catches
TACS[,,"A"] <- c(TACNSA + Ctransfer*TAC3aC - (TACNSA + Ctransfer*TAC3aC)*WBSSsplit,
                 NA,
                 NA)
TACS[,,"B"] <- c(TACNSB * Buptake,
                 NA ,
                 NA); #estimated B-fleet catch for FcY & CtY from mp option added afterwards
TACS[,,"C"] <- c(TAC3aC*(1-Ctransfer)*Csplit,
                 5557/(1-Csplit),#TAC3aC*(1-Ctransfer)*Csplit,
                 5557/(1-Csplit))
TACS[,,"D"] <- c(TAC3aD*Dsplit*Duptake,
                 332/(1-Dsplit),#TAC3aD*Dsplit*Duptake,
                 332/(1-Dsplit))#TAC3aD*Dsplit*Duptake)

# Retrieve uncertainty estimate on recruitment estimates by the model

#recWeights  <- subset(NSH.sam@params,name=="U");  # takes the output "U" from the model parameters. Uncertainty of the recruitment. 
#recWeights  <- (recWeights[seq(1,nrow(recWeights),dims(NSH.sam)$age+length(unique(NSH.sam@control@states["catch unique",]))),]$std.dev)^2

recWeights  <- subset(NSH.sam@params, name=="logR")$std.dev^2


RECS        <- FLQuants("ImY" =FLQuant(subset(rec(NSH.sam),year==ImY)$value,dimnames=list(age="0",year=ImY,unit="unique",season="all",area="unique",iter=1:dims(stk)$iter)),
                        "FcY" =exp(apply(log(rec(stk)[,ac((an(DtY)-10):DtY)]),3:6,weighted.mean,w=1/rev(rev(recWeights)[2:12]),na.rm=T)),
                        "CtY" =exp(apply(log(rec(stk)[,ac((an(DtY)-10):DtY)]),3:6,weighted.mean,w=1/rev(rev(recWeights)[2:12]),na.rm=T)))


# Combining the F and weight vectors for the previous year

# FS          <- list("A"=FA,"B"=FB,"C"=FC,"D"=FD)
# WS          <- list("A"=WA,"B"=WB,"C"=WC,"D"=WD)

yrs1        <- list("m.spwn","harvest.spwn","stock.wt")
yrs3        <- list("mat")
yrs5        <- list("m")

dsc         <- "North Sea Herring"
nam         <- "NSH"
dms         <- dimnames(stk@m)
dms$year    <- c(rev(rev(dms$year)[1:3]),ImY,FcY,CtY)
dms$unit    <- c("A","B","C","D")

f01         <- ac(0:1)
f26         <- ac(2:6)

stf.options <- c("mp","tacro","-15%","+15%","fmsy","fmsyAR","fpa","flim","fsq","bpa","blim","MSYBtrigger","nf")#"mp transfer"
# mp    =according to management plan,
  # +/-%  =TAC change,
  # nf    =no fishing,
  # bpa   =reach BPA in CtY or if ssb > bpa fish at fpa,
  # tacro =same catch as last year
  # fmsy  = fmsy implemented
  # flim,fpa,blim,bpa = reach F or biomass targets
mp.options  <- c("i")
  # i   = increase in B fleet is allowed,
  # fro = B fleet takes fbar as year before

#===============================================================================
# Setup stock file
#===============================================================================

# Create an stf object 
stf         <- FLStock(name=nam,desc=dsc,m=FLQuant(NA,dimnames=dms))

for(i in dms$unit)
  stf[,,i]  <- window(stk,start=an(dms$year)[1],end=rev(an(dms$year))[1])

units(stf)  <- units(stk)

# Fill slots that are the same for all fleets
for(i in c(unlist(yrs1),unlist(yrs3),unlist(yrs5))){
  if(i %in% unlist(yrs1)){ for(j in dms$unit){ slot(stf,i)[,FuY,j] <- slot(stk,i)[,DtY]}}
  if(i %in% unlist(yrs3)){ for(j in dms$unit){ slot(stf,i)[,FuY,j] <- yearMeans(slot(stk,i)[,ac((an(DtY)-2):an(DtY))])}}
  if(i %in% unlist(yrs5)){ for(j in dms$unit){ slot(stf,i)[,FuY,j] <- yearMeans(slot(stk,i)[,ac((an(DtY)-4):an(DtY))])}}
}

# Fill slots that are unique for the fleets; taking data from multifleet assessment
for(i in 1:length(dms$unit)){
  
  # Fill the D fleet with the data from the B fleet; same selection pattern
  if(i == 4) {
    stf@harvest[,FuY,4]     <- NSH3f.sam@harvest[,ImY,,,2]
    stf@catch.wt[,FuY,4]    <- NSHs3[["residual"]]@catch.wt[,DtY,,,2]
    stf@landings.wt[,FuY,4] <- NSHs3[["residual"]]@catch.wt[,DtY,,,2]
  }
  
  if(i != 4) {
    stf@harvest[,FuY,i]     <- NSH3f.sam@harvest[,ImY,,,i]
    stf@catch.wt[,FuY,i]    <- NSHs3[["residual"]]@catch.wt[,DtY,,,i]
    stf@landings.wt[,FuY,i] <- NSHs3[["residual"]]@catch.wt[,DtY,,,i]
  }
}


# Fill slots that have no meaning for NSAS
stf@discards.n[]          <- 0
stf@discards[]            <- 0
stf@discards.wt[]         <- 0

#===============================================================================
# Intermediate year
#===============================================================================

for(i in dms$unit)
  stf@stock.n[,ImY,i]     <- stk.sam@stock.n[,ImY]
##-Fix in 2014
#for(i in dms$unit)
#  stf@stock.n[1,ImY,i]    <- RECS[[1]]

stf@harvest[,ImY]         <- fleet.harvest(stk=stf,iYr=ImY,TACS=TACS[,ImY])
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

#- Fix in 2017 for iterQuantile function:

iterQuantile <- function(x){return(x)}

stf.table[1,"Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,ImY,"A"]))
stf.table[1,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,ImY,c("B","C","D")])),c(2:6,1))
stf.table[1,"Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,ImY,])))
stf.table[1,"Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,ImY,])))
stf.table[1,grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,ImY)),c(2:6,1))
stf.table[1,grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,ImY,1] * stf@stock.wt[,ImY,1] *
                                                                 exp(-unitSums(stf@harvest[,ImY])*stf@harvest.spwn[,ImY,1]-stf@m[,ImY,1]*stf@m.spwn[,ImY,1]) *
                                                                 stf@mat[,ImY,1]))

referencePoints$Fsq <- stf.table["intermediate year","Fbar 2-6","50%"]


#===============================================================================
# Forecast year
#===============================================================================

for(i in dms$unit) stf@stock.n[1,FcY,i]                     <- RECS$FcY
for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),FcY,i]   <- (stf@stock.n[,ImY,1]*exp(-unitSums(stf@harvest[,ImY])-stf@m[,ImY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
for(i in dms$unit) stf@stock.n[dims(stf)$age,FcY,i]         <- apply((stf@stock.n[,ImY,1]*exp(-unitSums(stf@harvest[,ImY])-stf@m[,ImY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)

###--- Management options ---###

if("fmsyAR" %in% stf.options){
   #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  TACS[,FcY,"A" ]   <- TACS.orig[,ImY,"A"]
  TACS2018          <- TACS
  if(WBSScatch<=1){
    TACS[,FcY,"C"]            <- dummy
  } else { TACS[,FcY,"C"]     <- TAC3aC * Csplit}

  res <- matrix(NA,nrow=dims(stf)$unit,ncol=dims(stf)$iter,dimnames=list(dimnames(stf@stock.n)$unit,dimnames(stf@stock.n)$iter))
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,dims(stf)$unit),lower=rep(1e-08,4), upper=NULL,find.FCAR,stk=iter(stf[,FcY],iTer),f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),WBSScatch=WBSScatch,Csplit=Csplit,refs.=referencePoints,jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par

  #-2018------------
  #- 2018 situation with no MP
  TACS[,c(FcY,ImY),"C"]         <-   TACS2018[,FcY,"C"] * Csplit
  TACS[,c(FcY,ImY),"D"]         <-   TACS2018[,FcY,"D"] * Dsplit
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,3),lower=rep(1e-08,3), upper=NULL,find.FCAR,stk=iter(stf[,FcY],iTer),f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),WBSScatch=WBSScatch,Csplit=Csplit,refs.=referencePoints,jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par[c(1,1,2,3)]
  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
  #-end 2018------------

  for(i in dms$unit){
    stf@catch.n[,FcY,i]         <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]           <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]      <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]        <- computeLandings(stf[,FcY,i])
  }



  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["fmsyAR","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["fmsyAR",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["fmsyAR","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["fmsyAR","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["fmsyAR",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["fmsyAR",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                      exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1] *
                                                                      stf@m.spwn[,FcY,1]) * stf@mat[,FcY,1]))
  stf.table["fmsyAR",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
  
  BcatchMP    <- stf.table["fmsyAR","Catch B","50%"]

  TACS[,,"B"] <- c(TACNSB * Buptake,
                   BcatchMP ,
                   BcatchMP); #estimated B-fleet catch for FcY & CtY from mp option added afterwards
  TACSlst     <- TACS

  
  
}


if("mp" %in% stf.options){

  #-2018------------
  #- 2018 situation with no MP
  TACS[,FcY,"C"]              <-   TACS2018[,FcY,"C"] * Csplit
  TACS[,FcY,"D"]              <-   TACS2018[,FcY,"D"] * Dsplit
  res <- matrix(NA,nrow=dims(stf)$unit,ncol=dims(stf)$iter,dimnames=list(dimnames(stf@stock.n)$unit,dimnames(stf@stock.n)$iter))
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- optim(par=rep(1,4),find.FABCD,
                                       Fs=    stf@harvest[,FcY,,,,iTer,drop=T],
                                       Ns=    stf@stock.n[,FcY,1,,,iTer,drop=T],
                                       Wts=   stf@stock.wt[,FcY,1,,,iTer,drop=T],
                                       CWts=  stf@catch.wt[,FcY,,,,iTer,drop=T],
                                       fspwns=stf@harvest.spwn[,FcY,1,,,iTer,drop=T],
                                       mspwns=stf@m.spwn[,FcY,1,,,iTer,drop=T],
                                       mats=  stf@mat[,FcY,1,,,iTer,drop=T],
                                       Ms=    stf@m[,FcY,1,,,iTer,drop=T],f01=f01,f26=f26,
                                       Tcs=   iter(TACS[,c(ImY,FcY)],iTer),
                                       Tcsorig=iter(TACS.orig[,c(ImY,FcY)],iTer),
                                       mixprop=Csplit,WBSScatch=WBSScatch,
                                       lower=rep(1e-10,4),method="L-BFGS-B",control=list(maxit=1000))$par

  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],c(3,6),res,"*")


  #-end 2018------------



#  transfer  <- 0
#  scen      <- "mp"
#  source("./mpOption15.r")

#  stf@harvest[,FcY] <- fleet.harvest(stk=stf,iYr=FcY,TACS=TACS[,ImY])
#
#  #- Calculate F multipliers for each of the fleets (make take some time)
#  res <- optim(par=rep(1,dims(stf)$unit),find.FABCD,
#                   Fs=stf@harvest[,FcY,,,,,drop=T],
#                   Ns=stf@stock.n[,FcY,1,,,,drop=T],
#                   Wts=stf@stock.wt[,FcY,1,,,,drop=T],
#                   CWts=stf@catch.wt[,FcY,,,,,drop=T],
#                   fspwns=stf@harvest.spwn[,FcY,1,,,,drop=T],
#                   mspwns=stf@m.spwn[,FcY,1,,,,drop=T],
#                   mats=stf@mat[,FcY,1,,,,drop=T],
#                   Ms=stf@m[,FcY,1,,,,drop=T],f01=f01,f26=f26,
#                   Tcs=TACS[,c(ImY,FcY)],
#                   Tcsorig=TACS.orig[,c(ImY)],
#                   lower=rep(1e-10,4),mixprop=Csplit,WBSScatch=WBSScatch,method="L-BFGS-B",control=list(trace=1))$par
#
#  #- Update harvest accordingly
#  stf@harvest[,FcY]         <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
#
#  #- Calculate catches based on new harvest pattern
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }

  #  Update to continuation year
  stf@harvest[,CtY]                                           <- stf@harvest[,FcY]
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,CtY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  ##fill stf.table
  stf.table["mp","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["mp",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["mp","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["mp","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["mp",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["mp",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                     exp(-unitSums(stf@harvest[,FcY])*
                                                                           stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                     stf@mat[,FcY,1]))
   stf.table["mp",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)


  # Bfleet catch estimated in TAC year during MP option;
#  BcatchMP    <- stf.table["mp","Catch B","50%"]
#
#  TACS[,,"B"] <- c(TACNSB * Buptake,
#                   BcatchMP ,
#                   BcatchMP); #estimated B-fleet catch for FcY & CtY from mp option added afterwards
#  TACSlst     <- TACS
#  cat("WBSS in A-fleet",stf.table["mp","Catch A","50%"] * WBSSsplit,"\n")
#  cat("WBSS in C-fleet",(TAC.C - TAC.C10) * (1-Csplit),"\n")
#  cat("WBSS in D-fleet",TAC3aD * (1-Dsplit),"\n")
#  cat("NSAS in A-fleet",stf.table["mp","Catch A","50%"] * (1-WBSSsplit),"\n")
#  cat("NSAS in C-fleet",(TAC.C - TAC.C10) * (Csplit),"\n")
#  cat("NSAS in D-fleet",TAC3aD * (Dsplit),"\n")

}

# Management plan with 50% transfer

if("mp transfer" %in% stf.options){

#  transfer  <- 0.50
#  scen      <- "mp transfer"
#  source("./mpOption15.r")


  #- Calculate F multipliers for each of the fleets (may take some time)
#  res <- optim(par=rep(1,dims(stf)$unit),find.FABCD,
#                   Fs=stf@harvest[,FcY,,,,,drop=T],
#                   Ns=stf@stock.n[,FcY,1,,,,drop=T],
#                   Wts=stf@stock.wt[,FcY,1,,,,drop=T],
#                   CWts=stf@catch.wt[,FcY,,,,,drop=T],
#                   fspwns=stf@harvest.spwn[,FcY,1,,,,drop=T],
#                   mspwns=stf@m.spwn[,FcY,1,,,,drop=T],
#                   mats=stf@mat[,FcY,1,,,,drop=T],
#                   Ms=stf@m[,FcY,1,,,,drop=T],f01=f01,f26=f26,
#                   Tcs=TACS[,c(ImY,FcY)],
#                   Tcsorig=TACS.orig[,c(ImY)],
#                   mixprop=Csplit,WBSScatch=WBSScatch,
#                   lower=rep(0.01,4),method="L-BFGS-B",control=list(maxit=100))$par
#
#  #- Update harvest accordingly
#  stf@harvest[,FcY]         <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
#
#  # calculate the C fleet TAC
#  TAC.C <- 0.057 * sum(stf@catch[,FcY,c("A")]) + 0.41 * WBSScatch  #combined C-fleet TAC
#
#  # 0% option
#  TAC.trans <- TAC.C * transfer
#
#  # 50% option
#  # TAC.C10 <- TAC.C * 0.5 # 50% thereof, add to A-fleet as the minimum transfer amount
#
#  stf@catch[,FcY,"A"] <- stf@catch[,FcY,"A"] + TAC.trans
#  stf@catch[,FcY,"C"] <- stf@catch[,FcY,"C"] - (TAC.trans * Csplit)
#  stf@harvest[,FcY]   <- fleet.harvest(stk=stf,iYr=FcY,TACS=stf@catch[,FcY]) # recalculate new F's for all fleets after transferring
#
#  #- Calculate catches based on new harvest pattern
#  for(i in dms$unit){
#    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
#    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
#    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
#    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
#  }
#
  #Update to continuation year
#  stf@harvest[,CtY]                                           <- stf@harvest[,FcY]
#  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
#  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
#  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
#  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,CtY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY
#
#  ##fill stf.table
##  stf.table["mp transfer","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
##  stf.table["mp transfer",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
##  stf.table["mp transfer","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
##  stf.table["mp transfer","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
##  stf.table["mp transfer",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
##  stf.table["mp transfer",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
##                                                                             exp(-unitSums(stf@harvest[,FcY])*
##                                                                                   stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
##                                                                             stf@mat[,FcY,1]))
#  stf.table["mp transfer",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
#
#
#  cat("WBSS in A-fleet",stf.table["mp transfer","Catch A","50%"] * WBSSsplit,"\n")
#  cat("WBSS in C-fleet",(TAC.C - TAC.C10) * (1-Csplit),"\n")
#  cat("WBSS in D-fleet",TAC3aD * (1-Dsplit),"\n")
#  cat("NSAS in A-fleet",stf.table["mp transfer","Catch A","50%"] * (1-WBSSsplit),"\n")
#  cat("NSAS in C-fleet",(TAC.C - TAC.C10) * (Csplit),"\n")
#  cat("NSAS in D-fleet",TAC3aD * (Dsplit),"\n")

}


# -------------------------------------------------------------------------

# BcatchMP    <- 7643 # Temporary value based on 2017 !!!

# TACS[,,"B"] <- c(TACNSB * Buptake,
#                  BcatchMP ,
#                  BcatchMP); #estimated B-fleet catch for FcY & CtY from mp option added afterwards


### No fishing ### ----------------------------------------------------------

if("nf" %in% stf.options){
  
  stf@harvest[,FcY] <- 0
  
  #Update to continuation year
  stf@harvest[,CtY]                                           <- stf@harvest[,FcY]
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,CtY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY


  stf.table["nf","Fbar 2-6 A",]                                 <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["nf",grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["nf","Fbar 2-6",]                                   <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["nf","Fbar 0-1",]                                   <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["nf",grep("Catch ",dimnames(stf.table)$values),]    <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["nf",grep("SSB",dimnames(stf.table)$values)[1],]    <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                                exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                                stf@mat[,FcY,1]))
  stf.table["nf",grep("SSB",dimnames(stf.table)$values)[2],]    <- iterQuantile(ssb.CtY)
}

### 15% increase in TAC for the A-fleet ### ----------------------------------------------------------

if("+15%" %in% stf.options){
  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]

  TACS                      <- TACSlst #reset catch forecast
  TACS[,FcY,"A"]            <- TACS.orig[,ImY,"A"]*1.15
  if(WBSScatch<=1){
    TACS[,FcY,"C"]          <- dummy
  } else { TACS[,FcY,"C"]   <- TAC3aC * 1.15 * Csplit}
  stf@harvest[,FcY]         <- fleet.harvest(stk=stf,iYr=FcY,TACS=TACS[,FcY])

  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }
  
  #Update to continuation year
  stf@harvest[,CtY]                                           <- stf@harvest[,FcY]
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,CtY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  TACS[,CtY,"A"]                                              <- c(TACS.orig[,ImY,"A"]*1.15*1.15)
  TACS[,CtY,"C"]                                              <- c(TACS[,FcY,"C"]*1.15*Csplit)
  stf@harvest[,CtY]                                           <- fleet.harvest(stk=stf,iYr=CtY,TACS=TACS[,CtY])
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*
                                                                           exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["+15%","Fbar 2-6 A",]                                 <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["+15%",grep("Fbar 0-1 ",dimnames(stf.table)$values),] <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["+15%","Fbar 2-6",]                                   <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["+15%","Fbar 0-1",]                                   <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["+15%",grep("Catch ",dimnames(stf.table)$values),]    <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["+15%",grep("SSB",dimnames(stf.table)$values)[1],]    <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                                  exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                                  stf@mat[,FcY,1]))
  stf.table["+15%",grep("SSB",dimnames(stf.table)$values)[2],]    <- iterQuantile(ssb.CtY)
  #TACSmp                                                           <- harvestCatch(stf,FcY)
}

### 15% reduction in TAC for the A-fleet ### ----------------------------------------------------------

if("-15%" %in% stf.options){

  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]

  TACS                      <- TACSlst #reset catch forecast
  TACS[,FcY,"A"]            <- TACS.orig[,ImY,"A"]*0.85
  if(WBSScatch<=1){
    TACS[,FcY,"C"]          <- dummy
  } else { TACS[,FcY,"C"]   <- TAC3aC * 0.85 * Csplit}
  stf@harvest[,FcY]         <- fleet.harvest(stk=stf,iYr=FcY,TACS=TACS[,FcY])
  
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }
  
  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  TACS[,CtY,"A"]                                              <- c(TACS.orig[,ImY,"A"]*0.85*0.85)
  TACS[,CtY,"C"]                                              <- c(TAC3aC * 0.85*0.85*Csplit)
  stf@harvest[,CtY]                                           <- fleet.harvest(stk=stf,iYr=CtY,TACS=TACS[,CtY])
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["-15%","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["-15%",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["-15%","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["-15%","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["-15%",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["-15%",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                 exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                 stf@mat[,FcY,1]))
  stf.table["-15%",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
  #TACSmp            <- harvestCatch(stf,FcY)
}


### Same TAC for A-fleet as last year ### ----------------------------------------------------------
#for 2015: use estimated B-fleet TAC from mp for the FcY and the NSAS proportion of the advised C-fleet catch in the FcY

if("tacro" %in% stf.options){
  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]

  TACS                      <- TACSlst #reset catch forecast
  TACS[,FcY,"A"]            <- TACS.orig[,ImY,"A"]
  if(WBSScatch<=1){
    TACS[,FcY,"C"]          <- dummy
  } else { TACS[,FcY,"C"]   <- TAC3aC * Csplit}
  stf@harvest[,FcY]         <- fleet.harvest(stk=stf,iYr=FcY,TACS=TACS[,FcY])
  
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }
  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  TACS[,CtY,"A"]                                              <- c(TACS.orig[,ImY,"A"])
  TACS[,CtY,"C"]                                              <- c(TAC3aC*Csplit)
  stf@harvest[,CtY]                                           <- fleet.harvest(stk=stf,iYr=CtY,TACS=TACS[,CtY])
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["tacro","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["tacro",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["tacro","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["tacro","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["tacro",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["tacro",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                 exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                 stf@mat[,FcY,1]))
  stf.table["tacro",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
}

### Fmsy option  ----------------------------------------------------------

if("fmsy" %in% stf.options){
   #reset harvest for all fleets
  stf@harvest[,FcY]           <- stf@harvest[,ImY]
  TACS[,FcY,"A" ]             <- TACS.orig[,ImY,"A"]
  if(WBSScatch<=1){
    TACS[,FcY,"C"]            <- dummy
  } else { TACS[,FcY,"C"]     <- TAC3aC * Csplit}

  res <- matrix(NA,nrow=dims(stf)$unit,ncol=dims(stf)$iter,dimnames=list(dimnames(stf@stock.n)$unit,dimnames(stf@stock.n)$iter))
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,dims(stf)$unit),lower=rep(1e-08,4), upper=NULL,find.FC,stk=iter(stf[,FcY],iTer),f.=referencePoints$Fmsy,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),WBSScatch=WBSScatch,Csplit=Csplit,jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par

  #-2018------------
  #- 2018 situation with no MP
  TACS[,FcY,"C"]              <-   TACS2018[,FcY,"C"] * Csplit
  TACS[,FcY,"D"]              <-   TACS2018[,FcY,"D"] * Dsplit
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,3),lower=rep(1e-08,3), upper=NULL,find.F,stk=iter(stf[,FcY],iTer),f.=referencePoints$Fmsy,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par[c(1,1,2,3)]
  #-end 2018------------


  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
  for(i in dms$unit){
    stf@catch.n[,FcY,i]         <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]           <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]      <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]        <- computeLandings(stf[,FcY,i])
  }

  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["fmsy","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["fmsy",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["fmsy","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["fmsy","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["fmsy",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["fmsy",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                      exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1] *
                                                                      stf@m.spwn[,FcY,1]) * stf@mat[,FcY,1]))
  stf.table["fmsy",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
}

### Fmsy option  ----------------------------------------------------------

### Fpa option  ----------------------------------------------------------

if("fpa" %in% stf.options){
   #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  TACS[,FcY,"A" ]   <- TACS.orig[,ImY,"A"]
  if(WBSScatch<=1){
    TACS[,FcY,"C"]            <- dummy
  } else { TACS[,FcY,"C"]     <- TAC3aC * Csplit}

  res <- matrix(NA,nrow=dims(stf)$unit,ncol=dims(stf)$iter,dimnames=list(dimnames(stf@stock.n)$unit,dimnames(stf@stock.n)$iter))
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,dims(stf)$unit),lower=rep(1e-8,4), upper=NULL,find.FC,stk=iter(stf[,FcY],iTer),f.=referencePoints$Fpa,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),WBSScatch=WBSScatch,Csplit=Csplit,jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par

  #-2018------------
  #- 2018 situation with no MP
  TACS[,c(FcY,ImY),"C"]         <-   TACS2018[,FcY,"C"] * Csplit
  TACS[,c(FcY,ImY),"D"]         <-   TACS2018[,FcY,"D"] * Dsplit
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,3),lower=rep(1e-08,3), upper=NULL,find.F,stk=iter(stf[,FcY],iTer),f.=referencePoints$Fpa,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par[c(1,1,2,3)]
  #-end 2018------------

  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
  for(i in dms$unit){
    stf@catch.n[,FcY,i]         <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]           <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]      <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]        <- computeLandings(stf[,FcY,i])
  }

  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["fpa","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["fpa",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["fpa","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["fpa","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["fpa",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["fpa",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                      exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1] *
                                                                      stf@m.spwn[,FcY,1]) * stf@mat[,FcY,1]))
  stf.table["fpa",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
}

### Flim option  ----------------------------------------------------------

if("flim" %in% stf.options){
  
  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  TACS[,FcY,"A" ]   <- TACS.orig[,ImY,"A"]
  if(WBSScatch<=1){
    TACS[,FcY,"C"]            <- dummy
  } else { TACS[,FcY,"C"]     <- TAC3aC * Csplit}

  res <- matrix(NA,nrow=dims(stf)$unit,ncol=dims(stf)$iter,dimnames=list(dimnames(stf@stock.n)$unit,dimnames(stf@stock.n)$iter))
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,dims(stf)$unit),lower=rep(1e-8,4), upper=NULL,find.FC,stk=iter(stf[,FcY],iTer),f.=referencePoints$Flim,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),WBSScatch=WBSScatch,Csplit=Csplit,jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par

  #-2018------------
  #- 2018 situation with no MP
  TACS[,c(FcY,ImY),"C"]         <-   TACS2018[,FcY,"C"] * Csplit
  TACS[,c(FcY,ImY),"D"]         <-   TACS2018[,FcY,"D"] * Dsplit
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,3),lower=rep(1e-08,3), upper=NULL,find.F,stk=iter(stf[,FcY],iTer),f.=referencePoints$Flim,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par[c(1,1,2,3)]
  #-end 2018------------

  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
  
  for(i in dms$unit){
    stf@catch.n[,FcY,i]         <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]           <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]      <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]        <- computeLandings(stf[,FcY,i])
  }

  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["flim","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["flim",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["flim","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["flim","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["flim",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["flim",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                      exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1] *
                                                                      stf@m.spwn[,FcY,1]) * stf@mat[,FcY,1]))
  stf.table["flim",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
}

### Fsqoption  ----------------------------------------------------------

if("fsq" %in% stf.options){
  
  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  
  TACS[,FcY,"A" ]   <- TACS.orig[,ImY,"A"]
  if(WBSScatch<=1){
    TACS[,FcY,"C"]            <- dummy
  } else { TACS[,FcY,"C"]     <- TAC3aC * Csplit}

  res <- matrix(NA,nrow=dims(stf)$unit,ncol=dims(stf)$iter,dimnames=list(dimnames(stf@stock.n)$unit,dimnames(stf@stock.n)$iter))
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,dims(stf)$unit),lower=rep(1e-8,4), upper=NULL,find.FC,stk=iter(stf[,FcY],iTer),f.=referencePoints$Fsq,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),WBSScatch=WBSScatch,Csplit=Csplit,jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par

  #-2018------------
  #- 2018 situation with no MP
  TACS[,c(FcY,ImY),"C"]         <-   TACS2018[,FcY,"C"] * Csplit
  TACS[,c(FcY,ImY),"D"]         <-   TACS2018[,FcY,"D"] * Dsplit
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,3),lower=rep(1e-08,3), upper=NULL,find.F,stk=iter(stf[,FcY],iTer),f.=referencePoints$Fsq,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par[c(1,1,2,3)]
  #-end 2018------------


  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
  for(i in dms$unit){
    stf@catch.n[,FcY,i]         <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]           <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]      <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]        <- computeLandings(stf[,FcY,i])
  }

  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["fsq","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["fsq",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["fsq","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["fsq","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["fsq",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["fsq",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                      exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1] *
                                                                      stf@m.spwn[,FcY,1]) * stf@mat[,FcY,1]))
  stf.table["fsq",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
}

### Bpa option  ----------------------------------------------------------

if("bpa" %in% stf.options){
   #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  TACS[,FcY,"A" ]   <- TACS.orig[,ImY,"A"]
  if(WBSScatch<=1){
    TACS[,FcY,"C"]            <- dummy
  } else { TACS[,FcY,"C"]     <- TAC3aC * Csplit}

  res <- matrix(NA,nrow=dims(stf)$unit,ncol=dims(stf)$iter,dimnames=list(dimnames(stf@stock.n)$unit,dimnames(stf@stock.n)$iter))
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,dims(stf)$unit),lower=rep(1e-8,4), upper=NULL,find.BC,stk=iter(stf[,FcY],iTer),b.=referencePoints$Bpa,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),WBSScatch=WBSScatch,Csplit=Csplit,jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par

  #-2018------------
  #- 2018 situation with no MP
  TACS[,c(FcY,ImY),"C"]         <-   TACS2018[,FcY,"C"] * Csplit
  TACS[,c(FcY,ImY),"D"]         <-   TACS2018[,FcY,"D"] * Dsplit
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,3),lower=rep(1e-08,3), upper=NULL,find.BC,stk=iter(stf[,FcY],iTer),b.=referencePoints$Bpa,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),WBSScatch=WBSScatch,Csplit=Csplit,jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par[c(1,1,2,3)]
  #-end 2018------------


  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
  for(i in dms$unit){
    stf@catch.n[,FcY,i]         <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]           <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]      <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]        <- computeLandings(stf[,FcY,i])
  }

  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["bpa","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["bpa",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["bpa","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["bpa","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["bpa",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["bpa",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                      exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1] *
                                                                      stf@m.spwn[,FcY,1]) * stf@mat[,FcY,1]))
  stf.table["bpa",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
}

### Blim option  ----------------------------------------------------------

if("blim" %in% stf.options){
   #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  TACS[,FcY,"A" ]   <- TACS.orig[,ImY,"A"]
  if(WBSScatch<=1){
    TACS[,FcY,"C"]            <- dummy
  } else { TACS[,FcY,"C"]     <- TAC3aC * Csplit}

  res <- matrix(NA,nrow=dims(stf)$unit,ncol=dims(stf)$iter,dimnames=list(dimnames(stf@stock.n)$unit,dimnames(stf@stock.n)$iter))
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,dims(stf)$unit),lower=rep(1e-8,4), upper=NULL,find.BC,stk=iter(stf[,FcY],iTer),b.=referencePoints$Blim,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),WBSScatch=WBSScatch,Csplit=Csplit,jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par

  #-2018------------
  #- 2018 situation with no MP
  TACS[,c(FcY,ImY),"C"]         <-   TACS2018[,FcY,"C"] * Csplit
  TACS[,c(FcY,ImY),"D"]         <-   TACS2018[,FcY,"D"] * Dsplit
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,3),lower=rep(1e-08,3), upper=NULL,find.BC,stk=iter(stf[,FcY],iTer),b.=referencePoints$Blim,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),WBSScatch=WBSScatch,Csplit=Csplit,jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par[c(1,1,2,3)]
  #-end 2018------------


  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
  for(i in dms$unit){
    stf@catch.n[,FcY,i]         <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]           <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]      <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]        <- computeLandings(stf[,FcY,i])
  }

  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["blim","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["blim",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["blim","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["blim","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["blim",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["blim",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                      exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1] *
                                                                      stf@m.spwn[,FcY,1]) * stf@mat[,FcY,1]))
  stf.table["blim",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
}

### MSYBtrigger option  ----------------------------------------------------------

if("MSYBtrigger" %in% stf.options){
  
  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  TACS[,FcY,"A" ]   <- TACS.orig[,ImY,"A"]
  if(WBSScatch<=1){
    TACS[,FcY,"C"]            <- dummy
  } else { TACS[,FcY,"C"]     <- TAC3aC * Csplit}
  
  # prepare results matrix = multiplier
  res <- matrix(NA,nrow=dims(stf)$unit,
                ncol=dims(stf)$iter,
                dimnames=list(dimnames(stf@stock.n)$unit,dimnames(stf@stock.n)$iter))
  
  # 
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,dims(stf)$unit),
                                          lower=rep(1e-08,4), upper=NULL,
                                          find.BC,
                                          stk=iter(stf[,FcY],iTer),
                                          b.=referencePoints$MSYBtrigger,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),WBSScatch=WBSScatch,Csplit=Csplit,
                                          jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=1,lower=1e-8, upper=NULL,find.BC,stk=iter(stf[,FcY],iTer),b.=referencePoints$Btrigger,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),WBSScatch=WBSScatch,Csplit=Csplit,jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par


  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
  for(i in dms$unit){
    stf@catch.n[,FcY,i]         <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]           <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]      <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]        <- computeLandings(stf[,FcY,i])
  }

  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["MSYBtrigger","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["MSYBtrigger",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["MSYBtrigger","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["MSYBtrigger","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["MSYBtrigger",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["MSYBtrigger",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                      exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1] *
                                                                      stf@m.spwn[,FcY,1]) * stf@mat[,FcY,1]))
  stf.table["MSYBtrigger",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
}


# Save the output to an RData file
save(stf, stf.table, file="ShortTermForecast multifleetmode_f01WBSS.RData")
# save(stf, stf.table, file="ShortTermForecast multifleetmode.RData")

#- Writing the STF to file
for(i in c("catch","catch.n","stock.n","harvest")){
  slot(stf,i)[,FcY] <- ""
  slot(stf,i)[,CtY] <- ""
}

source("./writeSTF.out.r")
options("width"=80,"scipen"=1000)
stf.out.file <- stf.out(stf,RECS,format="TABLE 2.7.%i NORTH SEA HERRING.")
write(stf.out.file,file=paste("./","stf_mf.out",sep="."))

#- Write the stf.table to file
write.csv(stf.table[,,2],
          file=paste0("stf.table_mf_","deterministic_f01WBSS.csv"))
#           file=paste0("stf.table_mf_","deterministic.csv"))



