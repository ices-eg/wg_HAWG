#compare runs 

rm(list=ls())
gc()

library(ggplot2)
library(dplyr)

setwd("C:/ICES/HAWG2016")

source("Supporting.R")

#three plots - SSB, recruits, mean F

run1 <- "2016_prelim"
run2 <- "2016_inc_2015_survey"

run1 <- "2015_assessment"
run2 <- "2016_prelim"

plots.dir <- "./Assessment/plots"

#read the ADMB report files
dfOut1 <- fReadReport(reportFile = paste0("./Assessment/",run1,".REP"))
dfOut2 <- fReadReport(reportFile = paste0("./Assessment/",run2,".REP"))

#stock summary plot
png(filename = paste0(plots.dir,"/RunComp_",run1,"_vs_",run2,".png"),
    width = 840, height = 840)

layout(matrix(c(1,2,3),nrow=3,ncol=1,byrow=TRUE))
plot(dfOut1$Year,dfOut1$SSB/1000,type="l",lty=1,lwd=2,col="black")
lines(dfOut2$Year,dfOut2$SSB/1000,lty=1,lwd=2,col="red")
plot(dfOut1$Year,dfOut1$ObsRec/1000,type="l",lty=1,lwd=2,col="black")
lines(dfOut2$Year,dfOut2$ObsRec/1000,lty=1,lwd=2,col="red")
plot(dfOut1$Year,dfOut1$FBarUnweighted,type="l",lty=1,lwd=2,col="black")
lines(dfOut2$Year,dfOut2$FBarUnweighted,lty=1,lwd=2,col="red")

dev.off()



with(dfOut, {
  plot(Year,PredCatch/1000,type="l",main="Landings",ylab="Kt",ylim=c(0,45))
  plot(Year,SSB/1000,type="l",main="Spawning Stock Biomass",ylab="Kt",ylim=c(0,160))
  plot(Year,FBarUnweighted,type="l",main="FBar 2-5",ylab="F",ylim=c(0,0.85))
  plot(Year,ObsRec/1000,type="l",main="Recruits (age 1)",ylab="Millions",ylim=c(0,1600))}
)

dev.off()