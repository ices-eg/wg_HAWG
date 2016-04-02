# ==============================================================================
# Reference points estimation
# North Sea herring
#
# Uses: R 3.2.3
#       FLCore 1.0
#       msy from github
#
# Changed: 1/4/2016 Martin Pastoors
# ==============================================================================

# install package devtools first
rm(list=ls())

# ***follow the order of loading (1) FLCore and (2) msy
# as SR fuctions have same name but different formulation

library(devtools)  ## install.packages("devtools")
library(FLCore)
library(msy)       ## install_github("msy", "einarhjorleifsson", ref = "master")

source("C:/DATA/GIT/HAWG/NSAS/Refpoints functions.R")
source("C:/DATA/GIT/HAWG/_Common/eqsr_fit_shift.R")

load("C:/DATA/HAWG/NSAS 2016.RData")

# dimnames(NSH@stock.n)$age <- as.character(as.numeric(dimnames(NSH@stock.n)$age)+1)


# models used   .... FIT changed to make ring to age shift by defining age of R as FLR age+1
# Special case for NS herring to take account of the stock object expressed in terms of 
# winter rings not ages

## plot(NSH)

##  Stk.srr <- as.FLSR(wbss, model='segreg')
##  Stk.srr <- fmle(Stk.srr)
## segreg3  <- function(ab, ssb){ log(ifelse(ssb >= 149000, ab$a * 149000, ab$a * ssb)) }
## FIT <- eqsr_fit(wbss, nsamp = 1000, models = "segreg3")
## FIT <- eqsr_fit(wbss, nsamp = 1000, models = c('bevholt','ricker'))
FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Bevholt", "Ricker"),
                      rshift=1, remove.years=c(1947:2001,2014,2015))
eqsr_plot(FIT,n=2e4)

## windows(width = 6.5, height = 7)
# png("msy_output_01.png", width=1500, height=1500, res=200, bg="white")
# eqsr_plot(FIT,n=2e4)
# dev.off()

SIM <- eqsim_run(FIT,  
                 bio.years = c(2004, 2015), 
                 bio.const = FALSE, 
                 sel.years = c(2004, 2015), 
                 sel.const = FALSE, 
                 recruitment.trim = c(3, -3), 
                 Fcv       = fbar(NSH.sam)[fbar(NSH.sam)$year==2015,"CV"], 
                 Fphi      = 0.74, 
#                 Btrigger  = 1500000, 
                 Blim      =  800000,
                 Bpa       = 1000000,
                 Fscan     = seq(0,1.2,len=61),
                 verbose   = TRUE)

# png("msy_output_02.png", width=1500, height=1500, res=200, bg="white")
eqsim_plot(SIM,catch=TRUE)
# dev.off()

#png("msy_output_03.png", width=1500, height=1500, res=200, bg="white")
sink("Fmsy_table")
MSY_Intervals(SIM)
sink()
#dev.off()

# including segmRegr3
FIT <- eqsr_fit_My(wbss, nsamp = 1000, models = c('bevholt','segreg','ricker'))

png("msy_output_01_withSegReg3.png", width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4)
dev.off()

SIM <- eqsim_run(FIT,  bio.years = c(2004, 2015), bio.const = FALSE, sel.years = c(2004, 2015), sel.const = FALSE, Fcv=fbar(wbss.sam)[fbar(wbss.sam)$year==2015,"CV"], Fphi=0.30, Btrigger = 110000, Blim=90000,Bpa=110000,Fscan = seq(0,1.2,len=61),verbose=FALSE)

png("msy_output_02_withSegReg3.png", width=1500, height=1500, res=200, bg="white")
eqsim_plot(SIM,catch=TRUE)
dev.off()

png("msy_output_03_withSegReg3.png", width=1500, height=1500, res=200, bg="white")
sink("Fmsy_table_withSegReg")
MSY_Intervals(SIM)
sink()
dev.off()

png("msy_output_04_withSegReg3.png", width=1500, height=1500, res=200, bg="white")
eqsim_ggplot(SIM,1000)
dev.off()

