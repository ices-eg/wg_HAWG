# install package devtools first
rm(list=ls())

# ***follow the order of loading (1) FLCore and (2) msy
# as SR fuctions have same name but different formulation

#library(devtools)  ## install.packages("devtools")
library(FLCore)
library(FLSAM)
library(msy)       ## install_github("ices-tools-prod/msy")
library(tidyverse)

# setwd("D:/Repository/ICES_HAWG/wg_HAWG/NSAS/benchmark/")
try(setwd("D:/git/wg_HAWG/NSAS/benchmark/R/7_finalModel/refPoints"),silent=FALSE)

# source("../refpts/Refpoints functions.R")
# source("../../_Common/eqsr_fit_shift.R")
# load("./results/7_finalModel/NSH_final.RData")

source("Refpoints functions.R")
source("D:/GIT/wg_HAWG/_Common/eqsr_fit_shift.R")
load("D:/WKPELA/06. Data/NSAS/SAM/NSH_final.RData")

# Get estimate of Blim using the whole time series

FIT_segregBlim <- 
  eqsr_fit(NSH,nsamp=2000, models = "Segreg")

FIT_segregBlim$sr.det  # 796 kT


# 0. Get Flim and thereby Fpa. Run EqSim with no MSY Btrigger (i.e. run EqSim with Btrigger=0), and Fcv=Fphi=0
FIT <- eqsr_fit_shift(NSH, nsamp = 2000, models = c("Ricker"),
                      rshift=1,remove.years=c(1947:2001))
SIM <- eqsim_run(FIT,
                 bio.years = c(2007:2016),
                 bio.const = FALSE,
                 sel.years = c(2007:2016),
                 sel.const = FALSE,
                 recruitment.trim = c(3, -3),
                 Fcv       = 0,
                 Fphi      = 0,
                 Blim      = 800000,
                 Bpa       = 1000000,
                 Btrigger  = 0,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE,
                 extreme.trim=c(0.01,0.99))


Flim      <- SIM$Refs2["catF","F50"]   # MP: 0.320

#Now calculate the uncertainty in F in terminal year. We need the sd that belongs to log(F)
logfbar   <- subset(fbar(NSH.sam),year==2016)
sdmin     <- function(sdestim){
             return(abs(0.025 - dnorm(log(logfbar$lbnd),log(logfbar$value),sdestim)))}
sdF       <- optimize(sdmin,interval=c(1e-4,0.2))$minimum
Fpa       <- Flim * exp(-1.645*sdF) #0.294  MP: 0.280     #the 0.07878 value is obtained from a 10-year retro of sd on log(F) in terminal year of an assessment

# 1. Run EqSim with no MSY Btrigger (i.e. run EqSim with Btrigger=0),
#    to get initial FMSY ; if this initial FMSY value is > Fpa, reduce it to Fpa
SIM <- eqsim_run(FIT,
                 bio.years = c(2007:2016),
                 bio.const = FALSE,
                 sel.years = c(2007:2016),
                 sel.const = FALSE,
                 recruitment.trim = c(3, -3),
                 Fcv       = 0.27,            #From Martin 2018 WKPELA
                 Fphi      = 0.51,            #From Martin  2018 WKPELA
                 Blim      = 800000,
                 Bpa       = 1000000,
                 Btrigger  = 0,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE,
                 extreme.trim=c(0.01,0.99))

Fmsy      <- SIM$Refs2["lanF","medianMSY"] #0.256  MP: 0.249; run again: 0.256
Blim      <- 0.8e6
#Now calculate the uncertainty in SSB in terminal year. We need the sd that belongs to log(ssb)
logssb    <- subset(ssb(NSH.sam),year==2016)
sdmin     <- function(sdestim){
             return(abs(0.025 - dnorm(log(logssb$lbnd),log(logssb$value),sdestim)))}
sdSSB     <- optimize(sdmin,interval=c(1e-4,0.2))$minimum
Bpa       <- Blim * exp(1.645*sdSSB)  # MP 878 kT

# 2. Select MSY Btrigger   (from schematic guidelines: yes, yes, no -> 5th percentile of MSYBtrigger
MSYBtrigger <- SIM$Refs2["catB","F05"]  # MP 1446 kT

#- Check if FMSY is precautionary, so do a scan
checkFmsy <- eqsim_run(FIT,
           bio.years = c(2007:2016),
           bio.const = FALSE,
           sel.years = c(2007:2016),
           sel.const = FALSE,
           recruitment.trim = c(3, -3),
           Fcv       = 0.27,
           Fphi      = 0.51,
           Blim      = 800000,
           Bpa       = 1000000,
           Btrigger  = 1450000,
           Fscan     = seq(0.18,0.35,0.01),
           verbose   = TRUE,
           extreme.trim=c(0.01,0.99))


#Here do the check
if(subset(checkFmsy$pProfile,Ftarget==round(Fmsy,2) & variable=="Blim")$value<0.05)
  Fmsy <- checkFmsy$Refs2["catF","F05"]

Fp05      <- checkFmsy$Refs2["catF","F05"] # MP: 0.242
propFmsy  <- subset(checkFmsy$pProfile,Ftarget==round(Fmsy,2) & variable=="Blim")$value

refpts <- data.frame(Flim=round(Flim,2),
                     Fpa=round(Fpa,2),
                     Fmsy=ifelse(Fmsy>Fpa,round(Fpa,2),round(Fmsy,2)),
                     Blim=Blim,
                     Bpa=ceiling(Bpa/1e5)*1e5,
                     MSYBtrigger=ceiling(MSYBtrigger/1e4)*1e4)


checkFmsy$pProfile %>% 
  filter(Ftarget> 0.20 & Ftarget < 0.35 & variable =="Blim") %>% 
  ggplot(aes(Ftarget, value)) +
  geom_line()

