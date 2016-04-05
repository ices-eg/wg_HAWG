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
#load("C:/DATA/HAWG/NSAS 2016 OldM.RData")

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
# FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Segreg"),
#                       rshift=1, remove.years=c(1947:2001,2015))
# FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Segreg"),
#                       rshift=1, remove.years=c())
# eqsr_plot(FIT,n=2e4)
# 
# FIT$sr.det

## windows(width = 6.5, height = 7)
# png("msy_output_01.png", width=1500, height=1500, res=200, bg="white")
# eqsr_plot(FIT,n=2e4)
# dev.off()

# SIM <- eqsim_run(FIT,  
#                  bio.years = c(2004, 2014), 
#                  bio.const = FALSE, 
#                  sel.years = c(2004, 2014), 
#                  sel.const = FALSE, 
#                  recruitment.trim = c(3, -3), 
#                  Fcv       = fbar(NSH.sam)[fbar(NSH.sam)$year==2015,"CV"], 
#                  Fphi      = 0.74, 
# #                 Btrigger  = 1500000, 
#                  Blim      =  800000,
#                  Bpa       = 1000000,
#                  Fscan     = seq(0,1.2,len=61),
#                  verbose   = TRUE)

# png("msy_output_02.png", width=1500, height=1500, res=200, bg="white")
# eqsim_plot(SIM,catch=TRUE)
# dev.off()

#png("msy_output_03.png", width=1500, height=1500, res=200, bg="white")
# sink("Fmsy_table")
# MSY_Intervals(SIM)
# sink()
#dev.off()


# including segmRegr3
# FIT <- eqsr_fit_My(wbss, nsamp = 1000, models = c('bevholt','segreg','ricker'))
# 
# png("msy_output_01_withSegReg3.png", width=1500, height=1500, res=200, bg="white")
# eqsr_plot(FIT,n=2e4)
# dev.off()
# 
# SIM <- eqsim_run(FIT,  bio.years = c(2004, 2015), bio.const = FALSE, sel.years = c(2004, 2015), sel.const = FALSE, Fcv=fbar(wbss.sam)[fbar(wbss.sam)$year==2015,"CV"], Fphi=0.30, Btrigger = 110000, Blim=90000,Bpa=110000,Fscan = seq(0,1.2,len=61),verbose=FALSE)
# 
# png("msy_output_02_withSegReg3.png", width=1500, height=1500, res=200, bg="white")
# eqsim_plot(SIM,catch=TRUE)
# dev.off()
# 
# png("msy_output_03_withSegReg3.png", width=1500, height=1500, res=200, bg="white")
# sink("Fmsy_table_withSegReg")
# MSY_Intervals(SIM)
# sink()
# dev.off()
# 
# png("msy_output_04_withSegReg3.png", width=1500, height=1500, res=200, bg="white")
# eqsim_ggplot(SIM,1000)
# dev.off()
# 
# setwd("C:/DATA/GIT/HAWG/NSAS/eqsim/")




# Scenario 01 newM 1947-2015, B/R

load("C:/DATA/HAWG/NSAS 2016.RData")

run   <- "01"
title <- "Bev_Rick newM 1947-2015"

FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Bevholt", "Ricker"),
                      rshift=1, remove.years=c())

SIM <- eqsim_run(FIT,  
                 bio.years = c(2004, 2014), 
                 bio.const = FALSE, 
                 sel.years = c(2004, 2014), 
                 sel.const = FALSE, 
                 recruitment.trim = c(3, -3), 
                 Fcv       = fbar(NSH.sam)[fbar(NSH.sam)$year==2015,"CV"], 
                 Fphi      = 0.74, 
                 Blim      =  800000,
                 Bpa       = 1000000,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE)

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," MSY ",title,".txt",sep="")); MSY_Intervals(SIM); sink()
sink(paste(run," SIM ",title,".txt",sep="")); SIM; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()

png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsim_plot(SIM,catch=TRUE); dev.off()



# Scenario 02 newM 2002-2014, B/R

load("C:/DATA/HAWG/NSAS 2016.RData")

run   <- "02"
title <- "Bev_Rick newM 2002-2014"

FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Bevholt", "Ricker"),
                      rshift=1, remove.years=c(1947:2001,2015))

SIM <- eqsim_run(FIT,  
                 bio.years = c(2004, 2014), 
                 bio.const = FALSE, 
                 sel.years = c(2004, 2014), 
                 sel.const = FALSE, 
                 recruitment.trim = c(3, -3), 
                 Fcv       = fbar(NSH.sam)[fbar(NSH.sam)$year==2015,"CV"], 
                 Fphi      = 0.74, 
                 Blim      =  800000,
                 Bpa       = 1000000,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE)

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," MSY ",title,".txt",sep="")); MSY_Intervals(SIM); sink()
sink(paste(run," SIM ",title,".txt",sep="")); SIM; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()

png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsim_plot(SIM,catch=TRUE); dev.off()


# Scenario 03 newM 2002-2014, B

load("C:/DATA/HAWG/NSAS 2016.RData")

run   <- "03"
title <- "Bev newM 2002-2014"

FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Bevholt"),
                      rshift=1, remove.years=c(1947:2001,2015))

SIM <- eqsim_run(FIT,  
                 bio.years = c(2004, 2014), 
                 bio.const = FALSE, 
                 sel.years = c(2004, 2014), 
                 sel.const = FALSE, 
                 recruitment.trim = c(3, -3), 
                 Fcv       = fbar(NSH.sam)[fbar(NSH.sam)$year==2015,"CV"], 
                 Fphi      = 0.74, 
                 Blim      =  800000,
                 Bpa       = 1000000,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE)

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," MSY ",title,".txt",sep="")); MSY_Intervals(SIM); sink()
sink(paste(run," SIM ",title,".txt",sep="")); SIM; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()

png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsim_plot(SIM,catch=TRUE); dev.off()


# Scenario 04 oldM 1947-2015, B/R

load("C:/DATA/HAWG/NSAS 2016 oldM.RData")

run   <- "04"
title <- "Bev_Rick oldM 1947-2015"

FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Bevholt", "Ricker"),
                      rshift=1, remove.years=c())

SIM <- eqsim_run(FIT,  
                 bio.years = c(2004, 2014), 
                 bio.const = FALSE, 
                 sel.years = c(2004, 2014), 
                 sel.const = FALSE, 
                 recruitment.trim = c(3, -3), 
                 Fcv       = fbar(NSH.sam)[fbar(NSH.sam)$year==2015,"CV"], 
                 Fphi      = 0.74, 
                 Blim      =  800000,
                 Bpa       = 1000000,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE)

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," MSY ",title,".txt",sep="")); MSY_Intervals(SIM); sink()
sink(paste(run," SIM ",title,".txt",sep="")); SIM; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()

png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsim_plot(SIM,catch=TRUE); dev.off()



# Scenario 05 oldM 2002-2014, B/R

load("C:/DATA/HAWG/NSAS 2016 oldM.RData")

run   <- "05"
title <- "Bev_Rick oldM 2002-2014"

FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Bevholt", "Ricker"),
                      rshift=1, remove.years=c(1947:2001,2015))

SIM <- eqsim_run(FIT,  
                 bio.years = c(2004, 2014), 
                 bio.const = FALSE, 
                 sel.years = c(2004, 2014), 
                 sel.const = FALSE, 
                 recruitment.trim = c(3, -3), 
                 Fcv       = fbar(NSH.sam)[fbar(NSH.sam)$year==2015,"CV"], 
                 Fphi      = 0.74, 
                 Blim      =  800000,
                 Bpa       = 1000000,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE)

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," MSY ",title,".txt",sep="")); MSY_Intervals(SIM); sink()
sink(paste(run," SIM ",title,".txt",sep="")); SIM; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()

png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsim_plot(SIM,catch=TRUE); dev.off()


# Scenario 06 oldM 2002-2014, B

load("C:/DATA/HAWG/NSAS 2016 oldM.RData")

run   <- "06"
title <- "Bev oldM 2002-2014"

FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Bevholt"),
                      rshift=1, remove.years=c(1947:2001,2015))

SIM <- eqsim_run(FIT,  
                 bio.years = c(2004, 2014), 
                 bio.const = FALSE, 
                 sel.years = c(2004, 2014), 
                 sel.const = FALSE, 
                 recruitment.trim = c(3, -3), 
                 Fcv       = fbar(NSH.sam)[fbar(NSH.sam)$year==2015,"CV"], 
                 Fphi      = 0.74, 
                 Blim      =  800000,
                 Bpa       = 1000000,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE)

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," MSY ",title,".txt",sep="")); MSY_Intervals(SIM); sink()
sink(paste(run," SIM ",title,".txt",sep="")); SIM; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()

png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsim_plot(SIM,catch=TRUE); dev.off()


# scenario 07 segmented regression, 1947-2015, new M

load("C:/DATA/HAWG/NSAS 2016.RData")

run   <- "07"
title <- "Segreg newM 1947-2015"

FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Segreg"),
                      rshift=1, remove.years=c())

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," FIT ",title,".txt",sep="")); FIT; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()


# scenario 08 segmented regression, 1947-2015, old M

load("C:/DATA/HAWG/NSAS 2016 oldM.RData")

run   <- "08"
title <- "Segreg oldM 1947-2015"

FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Segreg"),
                      rshift=1, remove.years=c())

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," FIT ",title,".txt",sep="")); FIT; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()


# scenario 09 segmented regression, 1985-2015, new M

load("C:/DATA/HAWG/NSAS 2016.RData")

run   <- "09"
title <- "Segreg newM 1985-2015"

FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Segreg"),
                      rshift=1, remove.years=c(1947:1984))

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," FIT ",title,".txt",sep="")); FIT; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()


# scenario 10 segmented regression, 1985-2015, old M

load("C:/DATA/HAWG/NSAS 2016 oldM.RData")

run   <- "10"
title <- "Segreg newM 1985-2015"

FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Segreg"),
                      rshift=1, remove.years=c(1947:1984))

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," FIT ",title,".txt",sep="")); FIT; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()

# scenario 13 2015 assess, 1985-2015, old M

load("C:/DATA/HAWG/NSAS 2015.RData")

run   <- "13"
title <- "Bev 2015 assess 2002-2014"

FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Bevholt"),
                      rshift=1, remove.years=c(1947:2001,2015))

SIM <- eqsim_run(FIT,  
                 bio.years = c(2004, 2014), 
                 bio.const = FALSE, 
                 sel.years = c(2004, 2014), 
                 sel.const = FALSE, 
                 recruitment.trim = c(3, -3), 
                 Fcv       = fbar(NSH.sam)[fbar(NSH.sam)$year==2015,"CV"], 
                 Fphi      = 0.74, 
                 Blim      =  800000,
                 Bpa       = 1000000,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE)

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," MSY ",title,".txt",sep="")); MSY_Intervals(SIM); sink()
sink(paste(run," SIM ",title,".txt",sep="")); SIM; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()

png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsim_plot(SIM,catch=TRUE); dev.off()



# Scenario 14 newM 2002-2014, B with harvest rule

load("C:/DATA/HAWG/NSAS 2016.RData")

run   <- "14"
title <- "Bev newM AR 2002-2014"

FIT <- eqsr_fit_shift(NSH, nsamp = 1000, models = c("Bevholt"),
                      rshift=1, remove.years=c(1947:2001,2015))

SIM <- eqsim_run(FIT,  
                 bio.years = c(2004, 2014), 
                 bio.const = FALSE, 
                 sel.years = c(2004, 2014), 
                 sel.const = FALSE, 
                 recruitment.trim = c(3, -3), 
                 Fcv       = fbar(NSH.sam)[fbar(NSH.sam)$year==2015,"CV"], 
                 Fphi      = 0.74, 
                 Btrigger  = 1500000,
                 Blim      =  800000,
                 Bpa       = 1000000,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE)

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," MSY ",title,".txt",sep="")); MSY_Intervals(SIM); sink()
sink(paste(run," SIM ",title,".txt",sep="")); SIM; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()

png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsim_plot(SIM,catch=TRUE); dev.off()


# Scenario 15 newM 2002-2015, without harvest rule, settings from plenary

load("C:/DATA/HAWG/NSAS 2016.RData")

run   <- "15"
title <- "Bev_Rick newM -AR 2002-2015"

FIT <- eqsr_fit_shift(NSH, nsamp = 500, models = c("Bevholt","Ricker"),
                      rshift=1, remove.years=c(1947:2001, 2016))

SIM <- eqsim_run(FIT,  
                 bio.years = c(2002, 2014), 
                 bio.const = FALSE, 
                 sel.years = c(2002, 2014), 
                 sel.const = FALSE, 
                 recruitment.trim = c(3, -3), 
                 Fcv       = 0.26, 
                 Fphi      = 0.49, 
                 Blim      =  800000,
                 Bpa       = 1000000,
                 Fscan     = seq(0,0.80,len=40),
                 verbose   = TRUE)

sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
sink(paste(run," MSY ",title,".txt",sep="")); MSY_Intervals(SIM); sink()
sink(paste(run," SIM ",title,".txt",sep="")); SIM; sink()

png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsr_plot(FIT,n=2e4); dev.off()

png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
eqsim_plot(SIM,catch=TRUE); dev.off()
