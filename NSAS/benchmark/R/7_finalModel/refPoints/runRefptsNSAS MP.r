# ==============================================================================
# Reference points estimation
# North Sea herring; final assessment at benchmark meeting 2018
# 
# Uses: R 3.4.3
#       FLCore 2.6.5
#       msy 0.1.18
#
# Changed: 16/2/2018 Martin Pastoors
# ==============================================================================

rm(list=ls())

# ***follow the order of loading (1) FLCore and (2) msy
# as SR fuctions have same name but different formulation

# library(devtools)
# install_github("ices-tools-prod/msy")

library(FLCore)
library(FLSAM)
library(msy)       
library(tidyverse)

try(setwd("D:/git/wg_HAWG/NSAS/benchmark/R/7_finalModel/refPoints"),silent=FALSE)


source("Refpoints functions.R")
source("D:/GIT/wg_HAWG/_Common/eqsr_fit_shift.R")

load("D:/WKPELA/06. Data/NSAS/SAM/NSH_final.RData")
NSHbench <- NSH

# -------------------------------------------------------------------------------
# This is for estimating Blim
# -------------------------------------------------------------------------------

# Not carried out

# -------------------------------------------------------------------------------
# This is for estimating Flim
# -------------------------------------------------------------------------------

runsFlim <- list(
  run1=list(srryears=c(2002:2016),  bioyears=c(2007:2016), models=c("Bevholt","Ricker")),
  run2=list(srryears=c(2002:2016),  bioyears=c(2007:2016), models=c("Ricker")),
  run3=list(srryears=c(1947:2016),  bioyears=c(2007:2016), models=c("Bevholt", "Ricker")),
  run3=list(srryears=c(1994:2016),  bioyears=c(2007:2016), models=c("Ricker"))
)

FITLIM <- list()
SIMLIM <- list()

i <- 4

for(i in 1:length(runsFlim)){
  print(i)
  title   <- names(runsFlim)[i]; 
  run     <- title
  OBJ     <- trim(NSH, year=runsFlim[[i]]$srryears)
  
  FITLIM[[i]]  <- eqsr_fit_shift(OBJ, 
                            nsamp        = 2000,   
                            rshift       = 1, 
                            models       = runsFlim[[i]]$models
                            )
  
  # write out the SR data to text file
  sink(paste(run," SRR ",title,".txt",sep="")); FITLIM[i]$sr.det; sink()

  # write out SRR plot
  # png(paste(run," SRR ",title," gg.png",sep=""), width=1500, height=1500, res=200, bg="white"); 
  eqsr_plot(FITLIM[[i]],n=2e4, ggPlot=TRUE); 
  # dev.off()
  
  # Do the simulation for Flim (no Btrigger, no uncertainty)
  SIMLIM[[i]] <- eqsim_run(FITLIM[[i]],
                           bio.years = runsFlim[[i]]$bioyears,
                           bio.const = FALSE,
                           sel.years = runsFlim[[i]]$bioyears,
                           sel.const = FALSE,
                           recruitment.trim = c(3, -3),
                           Fcv       = 0,
                           Fphi      = 0,
                           Blim      = 800000,
                           Bpa       = 1000000,
                           Btrigger  = 0,
                           Fscan     = seq(0,0.80,len=40),
                           verbose   = TRUE,
                           extreme.trim = c(0.01,0.99))

  
  flim <- round(t(SIMLIM[[i]]$Refs2)["F50","catF"],2)
  
  print(paste0(i,
               ", SRR years:", min(runsFlim[[i]]$srryears), "-", max(runsFlim[[i]]$srryears), 
               ", Bio years:", min(runsFlim[[i]]$bioyears), "-", max(runsFlim[[i]]$bioyears),
               ", Models:", paste(runsFlim[[i]]$models, collapse=" "),
               ", Flim = ", flim,
               sep="") )
        
  #png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
  eqsim_plot(SIMLIM[[i]],catch=TRUE); 
  #dev.off()
}

# save(FITLIM,SIMLIM,file=paste("./simulation_",title,".RData",sep=""))



# -----------------------------------------------------------------------------------
# This is for estimating the Fmsy
# -----------------------------------------------------------------------------------

runsFmsy <- list( 
  run5=list(srryears=c(1994:2015),  bioyears=c(2007:2016), models=c("Bevholt","Ricker")),
  run6=list(srryears=c(1994:2015),  bioyears=c(2007:2016), models=c("Ricker")),
  run7=list(srryears=c(1947:2016),  bioyears=c(2007:2016), models=c("Bevholt", "Ricker")),
  run8=list(srryears=c(1994:2016),  bioyears=c(2007:2016), models=c("Ricker"))
)

FITMSY   <- list()
SIMMSY   <- list()

i <- 4

for(i in 1:length(runsFmsy)){
  
  # print(i)
  # title       <- names(runsFmsy)[i]; 
  # run         <- title
  # FITMSY[[i]] <- eqsr_fit_shift(NSH, 
  #                               nsamp  = 2000, 
  #                               rshift = 1, 
  #                               models = runsFmsy[[i]]$models
  #                               )
  
  # FITMSY[[i]]$rby <- subset(FITMSY[[i]]$rby, year %in% runsFmsy[[i]]$bioyears)
  
  # save text file
  # sink(paste(run," SRR ",title,".txt",sep="")); FITMSY$sr.det; sink()
  
  # save SRR plot
  # png(paste(run," SRR ",title," gg.png",sep=""), width=1500, height=1500, res=200, bg="white")
  # eqsr_plot(FITMSY[[i]],n=2e4, ggPlot=TRUE); dev.off()
  
  # now run MSY simulation, with uncertainty but without Btrigger
  SIMMSY[[i]] <- eqsim_run(
                     FITLIM[[i]],
#                    FITMSY[[i]],
                     bio.years = runsFmsy[[i]]$bioyears,
                     bio.const = FALSE,
                     sel.years = runsFmsy[[i]]$bioyears,
                     sel.const = FALSE,
                     recruitment.trim = c(3, -3),
                     Fcv       = 0.27,
                     Fphi      = 0.50,
                     Blim      = 800000,
                     Bpa       = 1000000,
#                    Btrigger  = 1500000,
                     Fscan     = seq(0,0.80,len=40),
                     verbose   = TRUE,
                     extreme.trim=c(0.01,0.99))

  # Save the MSY intervals as text file
  # sink(paste(run," MSY ",title,".txt",sep="")); MSY_Intervals(SIMMSY[[i]]); sink()
  MSY_Intervals(SIMMSY[[i]]); 
  
  # Save the MSY simulation results
  # sink(paste(run," SIM ",title,".txt",sep="")); SIMMSY[[i]]; sink()
  SIMMSY[[i]];
  
  # png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
  eqsim_plot(SIMMSY[[i]],catch=TRUE); 
  # dev.off()
  
  fmsy <- round(t(SIMMSY[[i]]$Refs2)["medianMSY","lanF"],3)
  
  print(paste0(i,
               ", SRR years:", min(runsFmsy[[i]]$srryears), "-", max(runsFmsy[[i]]$srryears), 
               ", Bio years:", min(runsFmsy[[i]]$bioyears), "-", max(runsFmsy[[i]]$bioyears),
               ", Models:", paste(runsFmsy[[i]]$models, collapse=" "),
               ", Fmsy = ", fmsy,
               sep="") )
}


# save(FITMSY,SIMMSY,file=paste("./simulation_",title,".RData",sep=""))

# fp05 <- round(t(SIMMSY[[i]]$Refs2)["F05","catF"],3)

# load("D:/HAWG/North Sea Herring 2014.RData")
# NSH2014 <- NSH
# 
# load("D:/GIT/wg_HAWG/NSAS/benchmark/R/7_finalModel/refPoints_nocorMIK/NSHnocorMIK.RData")
# NSHnoCorr <- NSH
# 
# load("D:/HAWG/2017/06. Data/NSAS/SAM/North Sea Herring 2017.RData")
# NSH2017 <- NSH
# 
# ssbbench <- as.data.frame(ssb(NSHbench)) %>%  mutate(run="Bench")
# ssbnoCorr <- as.data.frame(ssb(NSHnoCorr)) %>%  mutate(run="noCorr")
# ssb2017  <- as.data.frame(ssb(NSH2017)) %>% mutate(run="HAWG2017")
# ssb2014  <- as.data.frame(ssb(NSH2014)) %>% mutate(run="HAWG2014")
# 
# rbench <- as.data.frame(trim(NSHbench@stock.n, age=0)) %>%  mutate(run="Bench")
# rnoCorr <- as.data.frame(trim(NSHnoCorr@stock.n, age=0)) %>%  mutate(run="noCorr")
# 
# ssb2017 %>% 
#   bind_rows(ssbbench) %>% 
#   bind_rows(ssb2014) %>% 
#   bind_rows(ssbnoCorr) %>% 
#   
#   ggplot(aes(year, data, group=run)) +
#   theme_bw() +
#   geom_line(aes(colour=run))
# 
# ssbnoCorr %>% 
#   bind_rows(ssbbench) %>% 
# 
#   ggplot(aes(year, data, group=run)) +
#   theme_bw() +
#   geom_line(aes(colour=run))
# 
# rnoCorr %>% 
#   bind_rows(rbench) %>% 
#   
#   ggplot(aes(year, data, group=run)) +
#   theme_bw() +
#   geom_line(aes(colour=run))
