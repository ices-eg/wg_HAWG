# ==============================================================================
# Reference points estimation
# North Sea herring; final assessment at benchmark meeting 2018
# Sensititivity analysis with ricker and segreg
#
# Uses: R 3.4.3
#       FLCore 2.6.5
#       msy 0.1.18
#
# 22/02/2018 Martin Pastoors
# 17/03/2018 Checked and updated folders after cleanup of github
# 18/05/2019 Adapted for the new msy package (replaces eqsr_fit_shift)
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
try(setwd("benchmark/2018/refpts"),silent=FALSE)

# source("../refpts/Refpoints functions.R")
# source("../../_Common/eqsr_fit_shift.R")
# load("./results/7_finalModel/NSH_final.RData")

source("Refpoints functions.R")
# source("../../_Common/eqsr_fit_shift.R")

#load("D:/WKPELA/06. Data/NSAS/SAM/NSH_final.RData")
# NEED TO OPEN THE SHAREPOINT FOLDER IN EXPLORER FIRST!!
load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/benchmarks/2018/wkherring/2014 Meeting docs/06. Data/NSAS/SAM/NSH_final.RData")

# 1. Get estimate of Blim using the whole time series and calculate Bpa

# FIT_segregBlim <- eqsr_fit_shift(NSH,nsamp=2000, models = "Segreg", rshift=1)
FIT_segregBlim <- eqsr_fit(NSH,nsamp=2000, models = "Segreg", rshift=1)

blim <- round(FIT_segregBlim$sr.det$b/1e5)*1e5  # 796 kT = 800 kT

#Now calculate the uncertainty in SSB in terminal year. We need the sd that belongs to log(ssb) to calculate Bpa
logssb    <- subset(ssb(NSH.sam),year==2016)
sdmin     <- function(sdestim){
  return(abs(0.025 - dnorm(log(logssb$lbnd),log(logssb$value),sdestim)))}
sdSSB     <- optimize(sdmin,interval=c(1e-4,0.2))$minimum
Bpa       <- blim * exp(1.645*sdSSB)  # MP 878 kT
Bpa       <- ceiling(Bpa/1e5)*1e5  # rounding up


# 2. parameterize the segreg model with Blim breakpoint and (roughly) geomean rec above this
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= blim, 
                                            ab$a * blim, 
                                            ab$a * ssb))

# 3. truncate the NSH object
NSHtrunc <- trim(NSH, year=2002:2016)

# 4. fit the stock recruitment model(s)
# FIT <- eqsr_fit_shift(NSHtrunc, nsamp = 2000, models = c("Ricker", "SegregBlim"), rshift=1)
# FIT <- eqsr_fit_shift(NSH, nsamp = 2000, models = c("Ricker", "SegregBlim", "Bevholt"), rshift=1)
# FIT <- eqsr_fit(NSHtrunc, nsamp = 2000, models = c("Ricker", "SegregBlim"), rshift=1)
FIT <- eqsr_fit(NSHtrunc, nsamp = 100, models = c("Ricker", "SegregBlim"), rshift=1)

# eqsr_plot(FIT,n=2e4, ggPlot=TRUE)
eqsr_plot(FIT,n=2e4, ggPlot=FALSE)

# 5. Get Flim and thereby Fpa. Run EqSim with no MSY Btrigger (i.e. run EqSim with Btrigger=0), and Fcv=Fphi=0
SIM <- eqsim_run(FIT,
                 bio.years = c(2007,2016),
                 bio.const = FALSE,
                 sel.years = c(2007,2016),
                 sel.const = FALSE,
                 recruitment.trim = c(3, -3),
                 Fcv       = 0,
                 Fphi      = 0,
                 # Blim      = 800000, Bpa = 1000000,    # From WKPELA 2018
                 Blim      = blim,   Bpa = Bpa,        # Corrected 20/5/2019
                 Btrigger  = 0,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE,
                 extreme.trim=c(0.01,0.99))


Flim      <- SIM$Refs2["catF","F50"]   # MP: 0.341

# Now calculate the uncertainty in F in terminal year. We need the sd that belongs to log(F) to calculate Fpa
logfbar   <- subset(fbar(NSH.sam),year==2016)
sdmin     <- function(sdestim){
             return(abs(0.025 - dnorm(log(logfbar$lbnd),log(logfbar$value),sdestim)))}
sdF       <- optimize(sdmin,interval=c(1e-4,0.2))$minimum
Fpa       <- Flim * exp(-1.645*sdF) #0.294  MP: 0.298     

df <- data.frame(stringsAsFactors = FALSE)
cv <- 0.24; phi <- 0.51

for(cv in seq(0, 0.5, 0.05)) {
  for (phi in seq(0.1, 1.0, 0.2)) {
    
    # 6. Run EqSim with no MSY Btrigger (i.e. run EqSim with Btrigger=0),
    #    to get initial FMSY ; if this initial FMSY value is > Fpa, reduce it to Fpa
    SIM <- eqsim_run(FIT,
                     bio.years = c(2007,2016),
                     bio.const = FALSE,
                     sel.years = c(2007,2016),
                     sel.const = FALSE,
                     recruitment.trim = c(3, -3),
                     # Fcv       = 0.27,  Fphi  = 0.51,            #From 2018 WKPELA
                     Fcv       = cv,  
                     Fphi      = phi,            
                     Blim      = blim,
                     Bpa       = Bpa,
                     Btrigger  = 0,
                     Fscan     = seq(0,0.50,0.01),
                     verbose   = FALSE,
                     extreme.trim=c(0.01,0.99))
    
    Fmsy      <- SIM$Refs2["lanF","medianMSY"] #0.275
    Fmsy      <- ifelse(Fmsy>Fpa,Fpa,Fmsy)
    Fmsy1     <- Fmsy
    
    # Select MSY Btrigger   (from schematic guidelines: yes, yes, no -> 5th percentile of MSYBtrigger
    MSYBtrigger <- SIM$Refs2["catB","F05"]  # MP 1396 kT
    MSYBtrigger <- round((2*MSYBtrigger)/1e5)*1e5/2 # rounding
    
    # 7. Check if FMSY is precautionary, so do a scan
    checkFmsy <- eqsim_run(FIT,
                           bio.years = c(2007,2016),
                           bio.const = FALSE,
                           sel.years = c(2007,2016),
                           sel.const = FALSE,
                           recruitment.trim = c(3, -3),
                           # Fcv       = 0.27,  Fphi  = 0.51,            #From 2018 WKPELA
                           Fcv       = cv,  Fphi  = phi,            #Corrected 20/5/2019
                           Blim      = blim,
                           Bpa       = Bpa,
                           Btrigger  = MSYBtrigger,
                           Fscan     = seq(0.0,0.5,0.01),
                           #           Fscan     = seq(0.18,0.5,0.01),
                           verbose   = FALSE,
                           extreme.trim=c(0.01,0.99))
    
    # If the precautionary criterion (FMSY < Fp.05) evaluated is not met, then FMSY should be reduced to  Fp.05. 
    Fp05      <- checkFmsy$Refs2["catF","F05"] # MP: 0.256
    propFmsy  <- subset(checkFmsy$pProfile,Ftarget==round(Fmsy,2) & variable=="Blim")$value
    if (Fmsy > Fp05) {Fmsy <- Fp05}
    
    print(paste(cv, phi, round(Fmsy1,2), round(Fp05,2), round(Fmsy,2)))
    
    df <- 
      bind_rows(
        df,
        data.frame(
          cv   = cv,
          phi  = phi,
          fp05 = Fp05,
          fmsy1= Fmsy1,
          fmsy = Fmsy,
          fpa  = Fpa,
          flim = Flim,
          bpa  = Bpa,
          blim = blim,
          run  = "WKPELA")
      )
  }
}

wkpela <- df %>% filter(run=="WKPELA")

df %>% 
  filter(is.na(run)) %>% 
  ggplot(aes(x=cv, y=fmsy, group=phi)) + 
  geom_line(aes(colour=as.character(phi))) +
  geom_point(aes(colour=as.character(phi))) +
  geom_point(data=wkpela, colour="red", size=3) +
  expand_limits(y=0)

save(df, file="refpoints_sim_df.RData")
