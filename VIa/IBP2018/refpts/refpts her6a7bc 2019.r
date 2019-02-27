# ==============================================================================
# Reference points estimation
# 6a7bc herring; final assessment at interbenchmark meeting 2019
#
# Uses: R 3.5
#       FLCore 2.6.12
#       msy 0.1.18:    
#
# 22/02/2018 Martin Pastoors
# 17/03/2018 Checked and updated folders after cleanup of github
# 21/02/2019 Adapted from North Sea herring code 2018
# ==============================================================================

# install package devtools first
rm(list=ls())

# ***follow the order of loading (1) FLCore and (2) msy
# as SR fuctions have same name but different formulation

#library(devtools)  # install.packages("devtools")
library(FLCore)     #  install.packages("FLCore", repos="http://flr-project.org/R")
library(FLSAM)
library(msy)        # devtools::install_github("ices-tools-prod/msy")
                    # devtools::install_github("ices-tools-prod/msy@development")
library(tidyverse)

# setwd("D:/Repository/ICES_HAWG/wg_HAWG/NSAS/benchmark/")
try(setwd("D:/GIT/wg_HAWG/VIa/IBP2018/refpts"),silent=FALSE)

source("../../../NSAS/refpts/Refpoints functions.R")
source("../../../_Common/eqsr_fit_shift.R")
source("../../../_Common/collapseAreas.R")
source("../../../../mptools/R/my_utils.r")

# Smooth hockey function adapted to Mesnil
Smooth_hockey <- function(a, ssb) smooth_hockey(a, ssb, gamma = sqrt(0.001 * 4))


# ----------------------------------------------------------------
# IBP 2019 data
# ----------------------------------------------------------------

load("D:/TEMP/IBP_VIaHerring_finalRun_MF.Rdata")
# save(MSH.tun, file="D:/TEMP/MSH.tun.Rdata")

# rename
STK       <- MSHm
STK.retro <- MSHm.retro
STK.sam   <- MSHm.sam
STK.ctrl  <- MSH.ctrl
STK.tun   <- MSH.tun
units(harvest(STK)) <- "f"


# add in results; for stck.n - split across areas
stock.n(STK)[] <- stock.n(window(MSHm.sam, end = 2017)) / dims(STK)$area
harvest(STK)   <- harvest(window(MSHm.sam, end = 2017))
stock(STK)     <- computeStock(STK)

# collapse areas (=fleets)
STK            <- collapseAreas(STK)
# plot(STK)
# STK@harvest[year=ac(2010:2017)] 

# set Fcv and Fphi
Fcv  <- 0.30            #see: HER 6a7bc Fcv and phi.xlsx
Fphi <- 0.37            #see: HER 6a7bc Fcv and phi.xlsx

# ----------------------------------------------------------------
# Now the reference points for the IBP assessment; first length of time series
# ----------------------------------------------------------------

STK <-  trim(STK, year=1975:2017)


# 1. Get estimate of Blim using the whole time series and calculate Bpa
FIT_segregBlim <- eqsr_fit(STK,nsamp=2000, models = "Segreg", rshift=1)
Blim           <- round(FIT_segregBlim$sr.det$b/1e4)*1e4  # 796 kT = 800 kT

eqsr_plot(FIT_segregBlim,n=2e4, ggPlot=FALSE)


# Now calculate the uncertainty in SSB in terminal year. 
# We need the sd that belongs to log(ssb) to calculate Bpa
logssb    <- subset(ssb(STK.sam),year==range(STK)["maxyear"])
sdmin     <- function(sdestim){
  return(abs(0.025 - dnorm(log(logssb$lbnd),log(logssb$value),sdestim)))}
sdSSB     <- optimize(sdmin,interval=c(1e-4,0.2))$minimum
Bpa       <- Blim * exp(1.645*sdSSB)  # MP 878 kT
Bpa       <- ceiling(Bpa/1e4)*1e4  # rounding up


# 2. parameterize the segreg model with Blim breakpoint and (roughly) geomean rec above this
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= Blim, ab$a * Blim, ab$a * ssb))

# 3. truncate the STK object
# STKtrunc <- trim(STK, year=2002:2016)

# 4. fit the stock recruitment model(s)
FIT <- eqsr_fit(STK, nsamp = 2000, models = c("Ricker", "SegregBlim"), rshift=1)
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
                 Blim      = Blim,
                 Bpa       = Bpa,
                 Btrigger  = 0,
                 # Fscan     = seq(0,0.30,len=400),
                 Fscan     = seq(0, 0.80,len=40),
                 verbose   = TRUE,
                 extreme.trim=c(0.01,0.99), 
                 Nrun      = 200)


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
                 Fcv       = Fcv,             
                 Fphi      = Fphi,            
                 Blim      = Blim,
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
           bio.years = c(2008:2017),
           bio.const = FALSE,
           sel.years = c(2008:2017),
           sel.const = FALSE,
           recruitment.trim = c(3, -3),
           Fcv       = Fcv,             
           Fphi      = Fphi,            
           Blim      = Blim,
           Bpa       = Bpa,
           Btrigger  = MSYBtrigger,
           Fscan     = seq(0.01,0.25,0.01),
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
                     Blim       = Blim,
                     Bpa        = Bpa,
                     MSYBtrigger= MSYBtrigger)

print(refpts)


# save(STK, STKtrunc, FIT, SIM, refpts, file="refpoints.RData")

# ----------------------------------------------------------------
# WKWEST 2015 data
# ----------------------------------------------------------------

load("D:/TEMP/MSH WKWEST2015.Rdata")
WKWEST      <- MSH
WKWEST.ctrl <- MSH.ctrl
WKWEST.tun  <- MSH.tun

# Fit smooth hockeystick similar to WKWEST
FIT_smoothhockey <- eqsr_fit(WKWEST, nsamp = 1000, models = "Smooth_hockey", rshift = 1)
eqsr_plot(FIT_smoothhockey,n=2e4, ggPlot=FALSE)
Blim_wkwest <- round(FIT_smoothhockey$sr.det$b/1e4)*1e4  # 796 kT = 800 kT

# ----------------------------------------------------------------
# Plottting from data frames
# ----------------------------------------------------------------

# Convert WKWEST to data.frames
WKWEST.df <- 
  bind_rows(mutate(as.data.frame(ssb(WKWEST)), slot="ssb"),
            mutate(as.data.frame(rec(WKWEST)), slot="rec", age=as.character(age)),
            mutate(as.data.frame(fbar(WKWEST)),slot="fbar")) %>% 
  mutate(assess="WKWEST2015")

# Convert STK to data.frames
STK.df <- 
  bind_rows(mutate(as.data.frame(ssb(STK)), slot="ssb"),
            mutate(as.data.frame(rec(STK)), slot="rec", age=as.character(age)),
            mutate(as.data.frame(fbar(STK)),slot="fbar")) %>% 
  mutate(assess="IBP2019")

# Several overview plotting routines

# plot SSB
bind_rows(WKWEST.df, STK.df) %>% 
  filter(slot=="ssb") %>% 
  ggplot(aes(x=year, y=data)) +
  theme_bw() +
  geom_point(aes(colour=assess)) + 
  geom_line(aes(colour=assess)) 

# plot recruitment
bind_rows(WKWEST.df, STK.df) %>% 
  filter(slot=="rec") %>% 
  ggplot(aes(x=year, y=data)) +
  theme_bw() +
  geom_point(aes(colour=assess)) + 
  geom_line(aes(colour=assess)) 

# plot ssb and recruitment and F
bind_rows(WKWEST.df, STK.df) %>% 
  ggplot(aes(x=year, y=data)) +
  theme_publication() +
  geom_point(aes(colour=assess)) + 
  geom_line(aes(colour=assess)) +
  labs(x="",y="") +
  facet_wrap(~slot, scales="free_y")

# Plot stock and recruitment in total or by decade
t <-
  bind_rows(WKWEST.df, STK.df) %>% 
  mutate(wr   = as.numeric(age),
         year = ifelse(is.na(wr), year, year - wr - 1)) %>% 
  dplyr::select(-age, -wr) %>% 
  spread(key=slot, value=data)

# in total
t %>% 
  ggplot(aes(x=ssb, y=rec, group=assess)) +
  theme_bw() +
  geom_point(aes(colour=assess)) + 
  geom_path(aes(colour=assess)) +
  expand_limits(x=0) +
  facet_wrap(~assess)

# by decade
t %>%
  mutate(decade = 10*floor(year/10)) %>%  
  filter( (assess=="IBP2019"    & year <= 2013)|
            (assess=="WKWEST2015" & year <= 2010) ) %>% 
  ggplot(aes(x=ssb, y=rec, group=assess)) +
  theme_bw() +
  geom_point(aes(colour=factor(decade))) + 
  geom_path(aes(colour=factor(decade))) +
  expand_limits(x=0) +
  facet_wrap(~assess)

# COmpare recruitment from model to recruitment from acoustic survey
HERAS.rec <-
  STK.tun[[1]] %>% 
  as.data.frame() %>% 
  bind_rows(as.data.frame(STK.tun[[2]])) %>% 
  filter(age==2, slot=="index") %>% 
  mutate(year = year - an(age) - 1, 
         assess="IBP2019")

STK.df %>% 
  filter(slot=="rec") %>% 
  mutate(wr   = as.numeric(age),
         year = ifelse(is.na(wr), year, year - wr - 1)) %>%
  bind_rows(HERAS.rec) %>% 
  filter(slot %in% c("rec","index")) %>% 
  filter(year >= 1988) %>%
  dplyr::select(-age, -wr) %>% 
  spread(key=slot, value=data) %>% 
  
  ggplot(aes(x=rec, y=index)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method=lm) 

