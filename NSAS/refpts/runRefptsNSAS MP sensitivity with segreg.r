# ==============================================================================
# Reference points estimation
# North Sea herring; final assessment at benchmark meeting 2018
# Sensititivity analysis with ricker and segreg
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
# This is for evaluting uncertainty in Fmsy depending on length of time series
# -------------------------------------------------------------------------------

runsFmsy <- list(
  run1=list(srryears=c(1994:2016),  bioyears=c(2007:2016), models=c("Ricker", "Segreg")),
  run2=list(srryears=c(1996:2016),  bioyears=c(2007:2016), models=c("Ricker","Segreg")),
  run3=list(srryears=c(1998:2016),  bioyears=c(2007:2016), models=c("Ricker","Segreg")),
  run4=list(srryears=c(2000:2016),  bioyears=c(2007:2016), models=c("Ricker","Segreg")),
  run5=list(srryears=c(2001:2016),  bioyears=c(2007:2016), models=c("Ricker","Segreg")),
  run6=list(srryears=c(2002:2016),  bioyears=c(2007:2016), models=c("Ricker","Segreg")),
  run7=list(srryears=c(2004:2016),  bioyears=c(2007:2016), models=c("Ricker","Segreg")),
  run8=list(srryears=c(2006:2016),  bioyears=c(2007:2016), models=c("Ricker","Segreg"))
)

FITMSY <- list()
SIMMSY <- list()

# i <- 1

for(i in 1:length(runsFmsy)){
  print(i)
  title   <- names(runsFmsy)[i]; 
  run     <- title
  OBJ     <- trim(NSH, year=runsFmsy[[i]]$srryears)
  
  FITMSY[[i]]  <- eqsr_fit_shift(
    OBJ, 
    nsamp        = 2000,   
    rshift       = 1, 
    models       = runsFmsy[[i]]$models
    )
  
  eqsr_plot(FITMSY[[i]],n=2e4, ggPlot=TRUE)
  
  SIMMSY[[i]] <- eqsim_run(
    FITMSY[[i]],
    bio.years = runsFmsy[[i]]$bioyears,
    bio.const = FALSE,
    sel.years = runsFmsy[[i]]$bioyears,
    sel.const = FALSE,
    recruitment.trim = c(3, -3),
    Fcv       = 0.27,
    Fphi      = 0.51,
    Blim      = 800000,
    Bpa       = 1000000,
    #  Btrigger  = 1500000,
    Fscan     = seq(0,0.80,len=40),
    verbose   = TRUE,
    extreme.trim=c(0.01,0.99))
  
  fmsy <- round(t(SIMMSY[[i]]$Refs2)["medianMSY","lanF"],3)
  
  print(paste0(i,
               ", SRR years:", min(runsFmsy[[i]]$srryears), "-", max(runsFmsy[[i]]$srryears), 
               ", Bio years:", min(runsFmsy[[i]]$bioyears), "-", max(runsFmsy[[i]]$bioyears),
               ", Models:", paste(runsFmsy[[i]]$models, collapse=" "),
               ", Fmsy = ", fmsy,
               sep="") )
  

}

eqsr_plot(FITMSY[[8]],n=2e4, ggPlot=TRUE)
round(t(SIMMSY[[1]]$Refs2)["medianMSY","lanF"],3)

# save(FITMSY, SIMMSY, file="refpoints.RData")

fitrby <-
  mutate   (FITMSY[[1]]$rby, sim=1) %>%
  bind_rows(mutate(FITMSY[[2]]$rby, sim=2)) %>% 
  bind_rows(mutate(FITMSY[[3]]$rby, sim=3)) %>% 
  bind_rows(mutate(FITMSY[[4]]$rby, sim=4)) %>% 
  bind_rows(mutate(FITMSY[[5]]$rby, sim=5)) %>% 
  bind_rows(mutate(FITMSY[[6]]$rby, sim=6)) %>% 
  bind_rows(mutate(FITMSY[[7]]$rby, sim=7)) %>% 
  bind_rows(mutate(FITMSY[[8]]$rby, sim=8))  

# x.mult <- y.mult <- 1
# out <- list()
# for (i in 1:8) {
#   modset   <- FITMSY[[1]]$sr.sto
#   data     <- FITMSY[[1]]$rby[, 1:3]
#   minSSB   <- min(data$ssb, max(data$ssb) * 0.0125)
#   maxSSB   <- max(data$ssb) * x.mult
#   maxrec   <- max(data$rec * y.mult)
#   out[[i]] <- do.call(rbind, lapply(sample(1:nrow(modset), 500), 
#                                function(i) {
#                                  fssb <- stats::runif(500, minSSB, maxSSB)
#                                  FUN  <- match.fun(modset$model[i])
#                                  frec <- exp(FUN(modset[i, ], fssb) + stats::rnorm(500, 
#                                                                                    sd = modset$cv[i]))
#                                  srModel <- modset$model[i]
#                                  data.frame(ssb = fssb, rec = frec, model = srModel)
#                                })) 
# }

# fitout <-
#   mutate   (out[[1]], sim=1) %>%
#   bind_rows(mutate(out[[2]], sim=2)) %>% 
#   bind_rows(mutate(out[[3]], sim=3)) %>% 
#   bind_rows(mutate(out[[4]], sim=4)) %>% 
#   bind_rows(mutate(out[[5]], sim=5)) %>% 
#   bind_rows(mutate(out[[6]], sim=6)) %>% 
#   bind_rows(mutate(out[[7]], sim=7)) %>% 
#   bind_rows(mutate(out[[8]], sim=8))  


# ggplot(fitrby, aes(x=ssb, y=rec)) +
#   theme_bw() +
#   # geom_point(data=fitout, aes(x=ssb,y=rec), colour="red", size=0.05, alpha=0.1) +
#   geom_point(aes(colour=as.factor(sim)), size=1) +
#   facet_wrap(~sim) +
#   expand_limits(x=0,y=0) +
#   guides(colour=FALSE)


