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

#library(devtools)  ## install.packages("devtools")
library(FLCore)
library(FLSAM)
library(msy)       ## install_github("msy", "einarhjorleifsson", ref = "master")
library(ggplot2)
library(dplyr)

source("./Refpoints functions.R")
source("../../_Common/eqsr_fit_shift.R")

load("../results/North Sea Herring.RData")


runsFpa <- list(  run1=list(minyears=c(1947:2001),          bioyears=c(2002:2015)),
                  run2=list(minyears=c(1947:2001),          bioyears=c(2002:2014)),
                  run3=list(minyears=c(1947:2001,2014),     bioyears=c(2002:2013)),
                  run4=list(minyears=c(1947:2001,2013:2014),bioyears=c(2002:2012)))
runsFMSY <- list( run5=list(minyears=c(1947:2001),          bioyears=c(2002:2015)),
                  run6=list(minyears=c(1947:2001),          bioyears=c(2002:2014)),
                  run7=list(minyears=c(1947:2001,2014),     bioyears=c(2002:2013)),
                  run8=list(minyears=c(1947:2001,2013:2014),bioyears=c(2002:2012)))

for(i in 1:length(runsFpa)){
  print(i)
  title <- names(runsFpa)[i]; run <- title
  FIT <- eqsr_fit_shift(NSH, nsamp = 2000, models = c("Bevholt","Ricker"),
                        rshift=1, remove.years=runsFpa[[i]]$minyears)
  FIT$rby <- subset(FIT$rby,year %in% runsFpa[[i]]$bioyears)
  #sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
  #png(paste(run," SRR ",title," gg.png",sep=""), width=1500, height=1500, res=200, bg="white")
  #eqsr_plot(FIT,n=2e4, ggPlot=TRUE); dev.off()
  #png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
  #eqsr_plot(FIT,n=2e4, ggPlot=FALSE); dev.off()
  SIM <- eqsim_run(FIT,
                   bio.years = runsFpa[[i]]$bioyears,
                   bio.const = FALSE,
                   sel.years = runsFpa[[i]]$bioyears,
                   sel.const = FALSE,
                   recruitment.trim = c(3, -3),
                   Fcv       = 0.27,
                   Fphi      = 0.50,
                   Blim      = 800000,
                   Bpa       = 1000000,
                   Btrigger  = 1500000,
                   Fscan     = seq(0,0.80,len=41),
                   verbose   = TRUE,
                   extreme.trim=c(0.01,0.99))

  #sink(paste(run," MSY ",title,".txt",sep="")); MSY_Intervals(SIM); sink()
  #sink(paste(run," SIM ",title,".txt",sep="")); SIM; sink()

  #png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
  #eqsim_plot(SIM,catch=TRUE); dev.off()
  save(FIT,SIM,file=paste("./simulation_",title,".RData",sep=""))
}

for(i in 1:length(runsMSY)){
  print(i)
  title <- names(runsFMSY)[i]; run <- title
  FIT <- eqsr_fit_shift(NSH, nsamp = 2000, models = c("Bevholt","Ricker"),
                        rshift=1, remove.years=runsFMSY[[i]]$minyears)
  FIT$rby <- subset(FIT$rby,year %in% runsFMSY[[i]]$bioyears)
  sink(paste(run," SRR ",title,".txt",sep="")); FIT$sr.det; sink()
  #png(paste(run," SRR ",title," gg.png",sep=""), width=1500, height=1500, res=200, bg="white")
  #eqsr_plot(FIT,n=2e4, ggPlot=TRUE); dev.off()
  #png(paste(run," SRR ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
  #eqsr_plot(FIT,n=2e4, ggPlot=FALSE); dev.off()
  SIM <- eqsim_run(FIT,
                   bio.years = runsFMSY[[i]]$bioyears,
                   bio.const = FALSE,
                   sel.years = runsFMSY[[i]]$bioyears,
                   sel.const = FALSE,
                   recruitment.trim = c(3, -3),
                   Fcv       = 0.27,
                   Fphi      = 0.50,
                   Blim      = 800000,
                   Bpa       = 1000000,
#                   Btrigger  = 1500000,
                   Fscan     = seq(0,0.80,len=40),
                   verbose   = TRUE,
                   extreme.trim=c(0.01,0.99))

  sink(paste(run," MSY ",title,".txt",sep="")); MSY_Intervals(SIM); sink()
  sink(paste(run," SIM ",title,".txt",sep="")); SIM; sink()

  #png(paste(run," MSY ",title,".png",sep=""), width=1500, height=1500, res=200, bg="white")
  #eqsim_plot(SIM,catch=TRUE); dev.off()
  save(FIT,SIM,file=paste("./simulation_",title,".RData",sep=""))
}

load(paste("./simulation_",5,".Rdata",sep=""))
  SIM <- eqsim_run(FIT,
                   bio.years = runsFMSY[[i]]$bioyears,
                   bio.const = FALSE,
                   sel.years = runsFMSY[[i]]$bioyears,
                   sel.const = FALSE,
                   recruitment.trim = c(3, -3),
                   Fcv       = 0,
                   Fphi      = 0,
                   Blim      = 800000,
                   Bpa       = 1000000,
#                   Btrigger  = 1500000,
                   Fscan     = seq(0,0.80,len=40),
                   verbose   = TRUE,
                   extreme.trim=c(0.01,0.99))
save(FIT,SIM,file=paste("./simulation_",title,"Flim.RData",sep=""))



Fmsy <- c(0.33,0.328,0.296,0.336)
Fup  <- c(0.236,0.233,0.214,0.243)
Flow <- c(0.436,0.434,0.388,0.44)
Fpa <- c(0.339,0.344,0.354,0.363)

plot(y=Fmsy,x=c(2015.1,2014.9,2014,2013),col=1:4,pch=19,xlab="Recruitment till year",ylab="Fmsy",ylim=c(0,0.5),xaxt="n")
axis(1,at=c(2013:2015),labels=c(2013:2015))
segments(c(2015.1,2014.9,2014,2013),Flow,c(2015.1,2014.9,2014,2013),Fup,col=1:4)
points(y=Fpa,x=c(2015.1,2014.9,2014,2013),col=1:4,pch=15)

