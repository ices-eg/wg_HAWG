# ==============================================================================
# Reference points estimation
# 6a7bc herring; final assessment at interbenchmark meeting 2019
#
# Uses: R 3.5
#       FLCore 2.6.10
#       msy 0.1.18
#
# 22/02/2018 Martin Pastoors
# 17/03/2018 Checked and updated folders after cleanup of github
# 21/02/2019 Adapted from North Sea herring code 2018
# 25/02/2019 Used WKWEST 2015 just to check
# ==============================================================================

# install package devtools first
rm(list=ls())

# ***follow the order of loading (1) FLCore and (2) msy
# as SR fuctions have same name but different formulation

#library(devtools)  ## install.packages("devtools")
library(FLCore)
library(FLSAM)
library(msy)       ## devtools::install_github("ices-tools-prod/msy")
library(tidyverse)

# setwd("D:/Repository/ICES_HAWG/wg_HAWG/NSAS/benchmark/")
try(setwd("VIa/IBP2018/refpts"),silent=FALSE)

# source("../refpts/Refpoints functions.R")
# source("../../_Common/eqsr_fit_shift.R")
# load("./results/7_finalModel/NSH_final.RData")

source("../../../NSAS/refpts/Refpoints functions.R")
source("../../../_Common/eqsr_fit_shift.R")

# NEED TO OPEN THE SHAREPOINT FOLDER IN EXPLORER FIRST!!
# load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/benchmarks/2018/wkherring/2014 Meeting docs/06. Data/NSAS/SAM/NSH_final.RData")
# load("//community.ices.dk/ExpertGroups/benchmarks/2019/IBPher6a7bc 2019/2019 Meeting docs/06. Data/FLSAM Single Fleet Runs/6a7bcHerring.RData")
# load("D:/TEMP/IBP_VIaHerring_finalRun_MF.Rdata")
load("D:/TEMP/MSH WKWEST2015.Rdata")

# rename
STK       <- MSH
#STK.retro <- MSHm.retro
STK.sam   <- MSH.sam
STK.ctrl  <- MSH.ctrl
STK.tun   <- MSH.tun

# rm(MSH.ctrl, MSH.tun, MSHm, MSHm.retro, MSHm.sam)

# 0. Deal with multifleet aspects
# STK@stock.n           <- STK.sam@stock.n[,ac(range(STK)["minyear"]:range(STK)["maxyear"])]
# STK@harvest           <- areaSums(STK.sam@harvest[,ac(range(STK)["minyear"]:range(STK)["maxyear"])])
# STK@harvest.spwn      <- areaMeans(STK@harvest.spwn[,ac(range(STK)["minyear"]:range(STK)["maxyear"])])
# STK@m                 <- areaMeans(STK@m[,ac(range(STK)["minyear"]:range(STK)["maxyear"])])
# STK@m.spwn            <- areaMeans(STK@m.spwn[,ac(range(STK)["minyear"]:range(STK)["maxyear"])])
# STK@stock.wt          <- areaMeans(STK@stock.wt[,ac(range(STK)["minyear"]:range(STK)["maxyear"])])
# STK@mat               <- areaMeans(STK@mat[,ac(range(STK)["minyear"]:range(STK)["maxyear"])])

# summary(STK)

# 1. Get estimate of Blim using the whole time series and calculate Bpa
Smooth_hockey <- function(a, ssb) smooth_hockey(a, ssb, gamma = sqrt(0.001 * 4))

FIT_Smoothhockey <- eqsr_fit_shift(STK,nsamp=2000, models = "smooth_hockey", rshift=2)
FIT_segregBlim <- eqsr_fit_shift(STK,nsamp=2000, models = "Segreg", rshift=2)
# eqsr_plot(FIT_segregBlim,n=2e4, ggPlot=FALSE)

blim <- round(FIT_segregBlim$sr.det$b/1e4)*1e4  # 390 kT instead of 220 kT estimated by plotMSY!!

# Now calculate the uncertainty in SSB in terminal year. 
# We need the sd that belongs to log(ssb) to calculate Bpa
logssb    <- subset(ssb(STK.sam),year==range(STK)["maxyear"])
sdmin     <- function(sdestim){
  return(abs(0.025 - dnorm(log(logssb$lbnd),log(logssb$value),sdestim)))}
sdSSB     <- optimize(sdmin,interval=c(1e-4,0.2))$minimum
Bpa       <- blim * exp(1.645*sdSSB)  # MP 878 kT
Bpa       <- ceiling(Bpa/1e4)*1e4  # rounding up


# 2. parameterize the segreg model with Blim breakpoint and (roughly) geomean rec above this
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= blim, 
                                            ab$a * blim, 
                                            ab$a * ssb))

# 3. truncate the STK object
# STKtrunc <- trim(STK, year=2002:2016)
STKtrunc <- STK

# 4. fit the stock recruitment model(s)
FIT <- eqsr_fit_shift(STKtrunc, nsamp = 2000, models = c("Ricker", "SegregBlim"), rshift=1)
# FIT <- eqsr_fit_shift(STK, nsamp = 2000, models = c("Ricker", "SegregBlim", "Bevholt"), rshift=1)
# FIT <- eqsr_fit_shift(STK, nsamp = 2000, models = c("Ricker", "Segreg", "Bevholt"), rshift=1)

eqsr_plot(FIT,n=2e4, ggPlot=TRUE)
eqsr_plot(FIT,n=2e4, ggPlot=FALSE)


# 5. Get Flim and thereby Fpa. Run EqSim with no MSY Btrigger (i.e. run EqSim with Btrigger=0), and Fcv=Fphi=0
SIM <- eqsim_run(FIT,
                 bio.years = c(2008:2017),
                 bio.const = FALSE,
                 sel.years = c(2008:2017),
                 sel.const = FALSE,
                 recruitment.trim = c(3, -3),
                 Fcv       = 0,
                 Fphi      = 0,
                 Blim      = blim,
                 Bpa       = Bpa,
                 Btrigger  = 0,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE,
                 extreme.trim=c(0.01,0.99))


Flim      <- SIM$Refs2["catF","F50"]   # MP: 0.341

# Now calculate the uncertainty in F in terminal year. We need the sd that belongs to log(F) to calculate Fpa
logfbar   <- subset(fbar(STK.sam),year==range(STK)["maxyear"])
sdmin     <- function(sdestim){
             return(abs(0.025 - dnorm(log(logfbar$lbnd),log(logfbar$value),sdestim)))}
sdF       <- optimize(sdmin,interval=c(1e-4,0.2))$minimum
Fpa       <- Flim * exp(-1.645*sdF) #0.294  MP: 0.298     

# 6. Run EqSim with no MSY Btrigger (i.e. run EqSim with Btrigger=0),
#    to get initial FMSY ; if this initial FMSY value is > Fpa, reduce it to Fpa
SIM <- eqsim_run(FIT,
                 bio.years = c(2008:2017),
                 bio.const = FALSE,
                 sel.years = c(2008:2017),
                 sel.const = FALSE,
                 recruitment.trim = c(3, -3),
                 Fcv       = 0.27,            #Need to check the derivation
                 Fphi      = 0.51,            #Need to check the derivation
                 Blim      = blim,
                 Bpa       = Bpa,
                 Btrigger  = 0,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE,
                 extreme.trim=c(0.01,0.99))

Fmsy      <- SIM$Refs2["lanF","medianMSY"] #0.275
Fmsy      <- ifelse(Fmsy>Fpa,Fpa,Fmsy)


# Select MSY Btrigger   (from schematic guidelines: yes, yes, no -> 5th percentile of MSYBtrigger
MSYBtrigger <- SIM$Refs2["catB","F05"]  # MP 1396 kT
MSYBtrigger <- round((2*MSYBtrigger)/1e5)*1e5/2 # rounding

# 7. Check if FMSY is precautionary, so do a scan
checkFmsy <- eqsim_run(FIT,
           bio.years = c(2007:2016),
           bio.const = FALSE,
           sel.years = c(2007:2016),
           sel.const = FALSE,
           recruitment.trim = c(3, -3),
           Fcv       = 0.27,
           Fphi      = 0.51,
           Blim      = blim,
           Bpa       = Bpa,
           Btrigger  = MSYBtrigger,
           Fscan     = seq(0.18,0.35,0.01),
#           Fscan     = seq(0.18,0.5,0.01),
           verbose   = TRUE,
           extreme.trim=c(0.01,0.99))

# If the precautionary criterion (FMSY < Fp.05) evaluated is not met, then FMSY should be reduced to  Fp.05. 
Fp05      <- checkFmsy$Refs2["catF","F05"] # MP: 0.256
propFmsy  <- subset(checkFmsy$pProfile,Ftarget==round(Fmsy,2) & variable=="Blim")$value
if (Fmsy > Fp05) {Fmsy <- Fp05}

Flim       <- round(Flim,2)
Fpa        <- round(Fpa,2)
Fmsy       <- round(Fmsy, 2)

# 8. final set of reference points
refpts <- data.frame(Flim       = Flim,
                     Fpa        = Fpa,
                     Fmsy       = Fmsy,
                     Blim       = blim,
                     Bpa        = Bpa,
                     MSYBtrigger= MSYBtrigger)

print(refpts)

# save(STK, STKtrunc, FIT, SIM, refpts, file="refpoints.RData")
