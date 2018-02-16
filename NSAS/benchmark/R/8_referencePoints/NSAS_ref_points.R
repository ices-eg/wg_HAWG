##~--------------------------------------------------------------------------
# Code to take the SAM assessment results from stockassessment.org (new TMB fits), 
# and run ICES standard EqSim reference point analyses
# D.C.M.Miller
##~--------------------------------------------------------------------------
## Issues:
# Doesn't work on old SAM fits from stockassessment.org.

## To Do:
# add option to simply load FLStock object (i.e. make more general)
# double check MSY Btrigger rules
# How many simulations (noSims) are needed? Do some comparison tests?

###-------------------------------------------------------------------------------
### Clean slate
rm(list=ls())

##~--------------------------------------------------------------------------
##        SECTION WHERE CHANGES NEED TO BE MADE   
##~--------------------------------------------------------------------------

##~--------------------------------------------------------------------------
## Directory info
path <- "D:/git/wg_HAWG/NSAS/benchmark/R/8_referencePoints"   # folder were the code is and where results will be saved (in a subfolder)
runName <- "/NSAS_benchmar2018_refPoints" # (no spaces) Results will be saved in a subfolder with this name (so make it descriptive)
## Save plots?
savePlots <- T

##~--------------------------------------------------------------------------
## Stock and assessment
stockName <- "NSH"                # Used only in plots (i.e. titles) and when saving data (i.e. file names)
user <- 3                                     # User 2 = Anders; User 3 = Guest (ALWAYS GETS THE LATEST COMMITTED VERSION); User 108 = David Miller
ages <- 0:8
years <- 1947:2016
meanFages <- c(4:8)
## Uncertainty last year
sigmaF <- NA                                  # Get from last year estimated in the assessment (SAM) unless this is specified as a value
sigmaSSB <- NA                                # Get from last year estimated in the assessment (SAM) unless this is specified as a value

##~--------------------------------------------------------------------------
## Create matrix for reference points
refPts <- matrix(NA,nrow=1,ncol=9, dimnames=list("value",c("MSYBtrigger","5thPerc_SSBmsy","Bpa","Blim","Fpa","Flim", "Fp05","Fmsy_unconstr","Fmsy")))  # "Fmsy_unconstr" is the Fmsy value without any precautionary considerations (i.e. ignore 5% P(SSB<Blim))
#Calculating  Blim below# refPts[,"Blim"]  <- 3482805    #2e6                     # Insert value for Blim, code blow will calculate Bpa and MSY Btrigger

##~--------------------------------------------------------------------------
## Simulation settings
# Number of sims
noSims <- 1001                                # ???

# SR models to use
appModels <- c("Segreg","Ricker","Bevholt")   # Available models:  

# Which years (SSB years, not recruitment years) to exclude from the SRR fits 
rmSRRYrs <- c()                               # leave as 'c()' if the full time series is to be used (default)
#rmSRRYrs <- c(2015:2016)                     # Or specify here which other years (e.g. early period) should be left out

# Autocorrelation in recruitment?
rhoRec <- F                                   # default=F

## Weight at age and selectivity
numAvgYrsB <- 5                               # Number of recent years to use for WAA
bioConst   <- TRUE                            # Constant/average WAA (TRUE) or resampling from the years specified (FALSE)
numAvgYrsS <- 5                               # Number of recent years to use for selectivity
selConst   <- TRUE                            # Constant/average selectivity (TRUE) or resampling from the years specified (FALSE)

## Forecast error (see Guidance document for details on calculation of these values)
# F
cvF  <- 0.212                                 # Default = 0.212
phiF <-	0.423                                 # Default = 0.423
# SSB
cvSSB <- 0                                    # Default = 0
phiSSB <- 0                                   # Default = 0


# 5th percentile of SSB in teh final year
SSB05<-0 #  NEED TO change script to GET THIS FROM THE ASSESSMENT RESULTS, but for now input it here (used in MSY Btrigger calculation)


##~--------------------------------------------------------------------------
##        NO CHANGES NEED TO BE MADE BELOW THIS POINT   
##~--------------------------------------------------------------------------

##~--------------------------------------------------------------------------
## Set working directory
setwd(path)
# create subfolder
shell(paste("md", runName, sep=" "))
setwd(paste(path,runName,"/",sep=""))

# Load libraries
#require(devtools)
#devtools::install_github("fishfollower/SAM/stockassessment")  # run this once if stoassessment has not been installed before
library(stockassessment)
#install_github("wgmg/msy") # run this once if stoassessment has not been installed before
library(msy)
require(FLCore)
require(FLSAM)

##~--------------------------------------------------------------------------
## Loading data
load("basecase_NSH.RData") # loads 'fit' to the workspace (last baserun conducted on stockassessment.org)     

## Get sigmaSSB and sigmaF from the assessment fit
if (is.na(sigmaSSB)) {
  idx <- names(fit$sdrep$value) == "logssb"
  sigmaSSB <- fit$sdrep$sd[idx][fit$data$years==max(years)] # Use last year in status table
  #sigmaSSB <- fit$sdrep$sd[idx][fit$data$years==(max(years)-1)] 
}
if (is.na(sigmaF)) {
  idx <- names(fit$sdrep$value) == "logfbar"
  #sigmaF <- fit$sdrep$sd[idx][fit$data$years==max(years)]
  sigmaF <- fit$sdrep$sd[idx][fit$data$years==(max(years)-1)]  # Use last year in status table
}

# year raneg
minYear <- range(NSH)["minyear"]; maxYear <- range(NSH)["maxyear"]

### Trim off last year of the stock object (only if incomplete data for last assessment year)
# origStk <- stk
# stk <- window(stk, start=minYear, end=(maxYear-1))

###-------------------------------------------------------------------------------
### Set SRR Models for the simulations
#Models: "segreg","ricker", "bevholt"; or specials: "SegregBlim/Bloss" (breakpt. Blim/Bloss)

## SRR years 
# Which years (SSB years) to exclude from the SRR fits
# Keep all except last 2 (poorly estimated rec/selec)
rmSRRYrs <- union(rmSRRYrs, c(maxYear-1,maxYear))  # This removes last two years
srYears <- setdiff(c(minYear:(maxYear-1)),rmSRRYrs)

## determine segreg model with AR1 taken into account
SegregAR1 <- function(){
  logl <- function(a, b, rho, rec, ssb){loglAR1(log(rec), FLQuant(log(ifelse(c(ssb)<=b,a*c(ssb),a*b)),dimnames=dimnames(ssb)),rho=rho)}
  
  model <- rec ~ FLQuant(ifelse(c(ssb)<=b,a*c(ssb),a*b),dimnames=dimnames(ssb))
  
  initial <- structure(function(rec, ssb){
    return(FLPar(a=median(c(rec/ssb),na.rm=TRUE), b=median(c(ssb),na.rm=TRUE),rho=0))},
    lower=c(0, 0, -1),
    upper=c(Inf, Inf, 1))
  
  return(list(logl=logl, model=model, initial=initial))
} 

sr <- as.FLSR(NSH)
model(sr)<-SegregAR1()
srAR1 <-fmle(sr)

refPts[,"Blim"] <- as.numeric(params(srAR1)["b"])

if (savePlots) x11()
plot(srAR1)
if (savePlots) savePlot(paste("05aii_",stockName,"_segregAR1.png"),type="png")
if (savePlots) dev.off()



## determine segreg model with Blim breakpoint and (roughly) geomean rec above this
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= refPts[,"Blim"], ab$a * refPts[,"Blim"], ab$a * ssb))

## determine segreg model with Bloss breakpoint and (roughly) geomean rec above this
SegregBloss  <- function(ab, ssb) log(ifelse(ssb >= min(ssb(stk)), ab$a * min(ssb(stk)), ab$a * ssb))


###~~~~~~~~~~~~~
## autocorrelation
ACFrec <- acf(rec(NSH)[,ac(srYears)])
acfRecLag1 <- round(ACFrec$acf[,,][2],2)
if (savePlots) x11()
acf(rec(NSH), plot=T, main=paste("Autocor. in Rec, Lag1 =",acfRecLag1,sep=" "))
if (savePlots) savePlot(paste("04_",stockName,"_SRautocor.png"),type="png")
if (savePlots) dev.off()


# Set a max for AC?

###-------------------------------------------------------------------------------
## Fit SRRs
FIT_segregBlim <- eqsr_fit(NSH,nsamp=noSims, models = "SegregBlim", remove.years=rmSRRYrs)
#FIT_segregBloss <- eqsr_fit(stk,nsamp=noSims, models = "SegregBloss", remove.years=rmSRRYrs)
FIT_segreg <- eqsr_fit(NSH,nsamp=noSims, models = "Segreg", remove.years=rmSRRYrs)
#FIT_segregAR1 <- eqsr_fit(stk,nsamp=noSims, models = "segregAR1", remove.years=rmSRRYrs)
FIT_All <- eqsr_fit(NSH,nsamp=noSims, models = appModels, remove.years=rmSRRYrs)

# save model proportions and parameters:
write.csv(FIT_segregBlim$sr.det, paste(stockName,"_FIT_segregBlim_SRpars.csv",sep=""))
#write.csv(FIT_segregBloss$sr.det, paste(stockName,"_FIT_segregBloss_SRpars.csv",sep=""))
write.csv(FIT_segreg$sr.det, paste(stockName,"_FIT_segreg_SRpars.csv",sep=""))
write.csv(FIT_All$sr.det, paste(stockName,"_FIT_All_SRpars.csv",sep=""))

# Plot raw SRR results
if (savePlots) x11()
eqsr_plot(FIT_segreg,n=2e4)
if (savePlots) savePlot(paste("05ai_",stockName,"_segreg.png"),type="png")
if (savePlots) dev.off()

if (savePlots) x11()
eqsr_plot(FIT_All,n=2e4)
if (savePlots) savePlot(paste("05b_",stockName,"_SRRall.png"),type="png")
if (savePlots) dev.off()


###-------------------------------------------------------------------------------
## Run simulations
###-------------------------------------------------------------------------------

###-------------------------------------------------------------------------------
## Calculate Bpa based on sigmaSSB
refPts[,"Bpa"]  <- refPts[,"Blim"]*exp(sigmaSSB*1.645) #40000  # Used as Btrigger

###-------------------------------------------------------------------------------
## Simuation 1 - get Flim
# Flim is derived from Blim by simulating the stock with segmented regression S-R function with the point of inflection at Blim 
# Flim = the F that, in equilibrium, gives a 50% probability of SSB > Blim. 
# Note this simulation should be conducted with:
#  fixed F (i.e. without inclusion of a Btrigger)
#  without inclusion of assessment/advice errors. 

SIM_segregBlim <- eqsim_run(FIT_segregBlim,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = TRUE,
                            sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = TRUE,
                            Fcv=0, Fphi=0, SSBcv=0,
                            rhologRec=rhoRec,
                            Btrigger = 0, Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                            Nrun=200, Fscan = seq(0,1.0,len=101),verbose=T)

# save MSY and lim values
tmp1 <- t(SIM_segregBlim$Refs2)
write.csv(tmp1, paste("EqSim_",stockName,"_SegregBlim_eqRes.csv",sep=""))
refPts[,"Flim"] <- tmp1["F50","catF"]

# Fpa is derived from Flim in the reverse of the way Bpa is derived from Blim. i.e.: 
refPts[,"Fpa"] <- round(refPts[,"Flim"] * exp(-sigmaF * 1.645) , 2)
refPts[,"Flim"] <- round(refPts[,"Flim"],2)

###-------------------------------------------------------------------------------
## Simuation 2a - get initial Fmsy
# FMSY should initially be calculated based on:
#     a constant F evaluation 
#     with the inclusion of stochasticity in population and exploitation 
#     as well as assessment/advice error. 
#     Appropriate SRRs should be specified (here using all 3)

SIM_All_noTrig <- eqsim_run(FIT_All,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = FALSE,
                            sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = FALSE,
                            Fcv=cvF, Fphi=phiF, SSBcv=cvSSB,
                            rhologRec=rhoRec,
                            Btrigger = 0, Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                            Nrun=200, Fscan = seq(0,1.0,len=101),verbose=T)

# save MSY and lim values
tmp2 <- t(SIM_All_noTrig$Refs2)
write.csv(tmp2, paste("EqSim_",stockName,"_AllnoTrig_eqRes.csv",sep=""))
Fmsy_tmp <- tmp2["medianMSY","lanF"]

# save Equilibrium plots
if (savePlots) x11()
eqsim_plot(SIM_All_noTrig,catch=TRUE)  
if (savePlots) savePlot(paste("06_",stockName,"_AllnoTrig_eqMSYplots.png"),type="png")
if (savePlots) dev.off()

# To ensure consistency between the precautionary and MSY frameworks, FMSY is not allowed to be above Fpa
refPts[,"Fmsy_unconstr"] <- Fmsy_tmp 
if (Fmsy_tmp > refPts[,"Fpa"]) {
  print("WHOAAA, Fmsy > Fpa!") 
  refPts[,"Fmsy"] <- refPts[,"Fpa"]
} else {
  refPts[,"Fmsy"] <- Fmsy_tmp
}


###-------------------------------------------------------------------------------
## MSY Btrigger

data.05<-SIM_segregBlim$rbp
x.05 <- data.05[data.05$variable == "Spawning stock biomass", ]$Ftarget
b.05 <- data.05[data.05$variable == "Spawning stock biomass", ]$p05
plot(b.05~x.05, ylab="SSB", xlab="F")
b.lm <- loess(b.05 ~ x.05)
refPts[,"5thPerc_SSBmsy"] <- predict(b.lm, refPts[,"Fmsy"])
# check if F<Fmsy last 5 years
if (sum(as.numeric(fbar(stk)[,ac((maxYear-4):maxYear)])<=refPts[,"Fmsy"])<3) {
  refPts[,"MSYBtrigger"]  <- refPts[,"Bpa"]  
} else {
  # Check if Bmsy_05>Bpa
  refPts[,"MSYBtrigger"] <-ifelse(refPts[,"5thPerc_SSBmsy"]>refPts[,"Bpa"],refPts[,"5thPerc_SSBmsy"],refPts[,"Bpa"])
  # Check if Bmsy_05 > SSBcur_05
  refPts[,"MSYBtrigger"] <-ifelse(refPts[,"5thPerc_SSBmsy"] > SSB05,max(refPts[,"Bpa"],SSB05),refPts[,"5thPerc_SSBmsy"])  
}


###-------------------------------------------------------------------------------
## Simuation 2b - get final Fmsy
# MSY Btrigger should be selected to safeguard against an undesirable or unexpected low SSB when fishing at FMSY
# The ICES MSY AR should be evaluated to check that the FMSY and MSY Btrigger combination adheres to precautionary considerations: 
#      in the long term, P(SSB<Blim)<5%
# The evaluation must include:
#      realistic assessment/advice error
#      stochasticity in population biology and fishery exploitation.
#      Appropriate SRRs should be specified (here using all 3)

SIM_All_Trig <- eqsim_run(FIT_All,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = FALSE,
                          sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = FALSE,
                          Fcv=cvF, Fphi=phiF, SSBcv=cvSSB,
                          rhologRec=rhoRec,
                          Btrigger = refPts[,"MSYBtrigger"], Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                          Nrun=200, Fscan = seq(0,1.0,len=101),verbose=T)

# save MSY and lim values
tmp3 <- t(SIM_All_Trig$Refs2)
write.csv(tmp3, paste("EqSim_",stockName,"_AllTrig_eqRes.csv",sep=""))
refPts[,"Fp05"] <- tmp3["F05","catF"]

# save Equilibrium plots
if (savePlots) x11()
eqsim_plot(SIM_All_Trig,catch=TRUE)  
if (savePlots) savePlot(paste("07_",stockName,"_AllTrig_eqMSYplots.png"),type="png")
if (savePlots) dev.off()

# If the precautionary criterion (FMSY < Fp.05) evaluated is not met, then FMSY should be reduced to  Fp.05. 
if (refPts[,"Fmsy"] > refPts[,"Fp05"]) {
  print("WHOAAA, Fmsy > Fp05!") 
  refPts[,"Fmsy"] <- round(refPts[,"Fp05"],2) # If Fmsy > Fp05, Fmsy = Fp05
} else {  
  refPts[,"Fmsy"] <- round(refPts[,"Fmsy"],2) # Otherwise keep value from constant F simulation (which has been checked against Fpa)
}
refPts[,"Fp05"] <- round(refPts[,"Fp05"],2) 



###-------------------------------------------------------------------------------
## Save reference points
write.csv(refPts, paste(stockName,"_RefPts.csv",sep=""))

###-------------------------------------------------------------------------------
## Save run settings
SRused <- appModels[1]
if (length(appModels)>1) for (i in 2:length(appModels)) SRused <- paste(SRused,appModels[i],sep="_")
SRyears_min <- min(srYears); SRyears_max <- max(srYears)
setList <- c("stockName", "runName", "SAOAssessment", "sigmaF", "sigmaSSB", "noSims", "SRused", "SRyears_min", 
             "SRyears_max", "acfRecLag1","rhoRec", "numAvgYrsB", "numAvgYrsS", "cvF", "phiF", "cvSSB", "phiSSB")
runSet <- matrix(NA,ncol=1, nrow=length(setList), dimnames=list(setList,c("Value")))
for (i in setList) runSet[which(setList==i),] <- eval(parse(text = i))

write.csv(runSet, paste(stockName,"_RunSettings.csv",sep=""))

###-------------------------------------------------------------------------------
## Save workspace
save.image(file=paste(stockName,"_",maxYear,"_EqSim_Workspace.Rdata",sep=""))

