# Convert from stockassessment.org

library(FLCore)
library(FLSAM)
library(stockassessment) # stock assessment.org

# Load the tidyverse packages
library(tidyverse)
library(ggplot2)
library(readsam)       # tidy sam reader       # devtools::install_github("einarhjorleifsson/readsam")

# Load utils code
source("D:/GIT/mptools/r/my_utils.r")

list_all_objects_in_package("FLSAM")

rundir <- "https://www.stockassessment.org/datadisk/stockassessment/userdirs/user233/WBSS_HAWG_2018/run/"
fit    <- get(load("D:/HAWG/2018/05. Data/WBSS/model.RData"))

SAM2FLR(fit, ctrl)

res  <- new("FLSAM")

stk.ctrl <- new("FLSAM.control")

# set states
stk.ctrl@states[]["catch",] <- c(1:5,rep(6,4))

# correlated random walk
stk.ctrl@cor.F[] <- TRUE

# set catchabilities
stk.ctrl@catchabilities[]["HERAS",]   <- c(NA,1:7,7)
stk.ctrl@catchabilities[]["GerAS",]   <- c(8:15,15)
stk.ctrl@catchabilities[]["N20",]     <- c(16,rep(NA,8))
stk.ctrl@catchabilities[]["IBTS Q1",] <- c(NA,17:20,rep(NA,4))
stk.ctrl@catchabilities[]["IBTS Q3",] <- c(NA,21:24,rep(NA,4))


stk.ctrl@power.law.exps

# set fishing mortality RW variance
stk.ctrl@f.vars[]["catch",] <- c(1,rep(2,8))

# set log N RW variances
stk.ctrl@logN.vars[] <- rep(1,9)

# set observation variances
stk.ctrl@obs.vars[]["catch",] <- c(1,rep(2,3),rep(3,5))
stk.ctrl@obs.vars[]["HERAS",] <- c(NA,4,5,rep(6,4),7,7)
stk.ctrl@obs.vars[]["GerAS",] <- c(rep(8,4),9,9,rep(10,3))
stk.ctrl@obs.vars[]["N20",] <- c(11,rep(NA,8))
stk.ctrl@obs.vars[]["IBTS Q1",] <- c(NA,rep(12,4),rep(NA,4))
stk.ctrl@obs.vars[]["IBTS Q3",] <- c(NA,13,13,14,14,rep(NA,4))

#Finalise
stk.ctrl@name <- "WBSSher"
stk.ctrl <- update(stk.ctrl)

