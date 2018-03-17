install.packages("FLSAM", repos = "http://flr-project.org/R")

library(FLCore)
library(FLSAM)
library(FLEDA)

rm(list=ls())

#- Set paths
my.path<-file.path("C:","Work","HAWG2018","SAM")
output.dir              <- file.path(my.path,"results")
data.source         <- file.path(my.path,"data")    #Data source, not code or package source!!!

#- Setup stock, tun and control
scenario<- list(); scenario$trimTS   <- T
#source(file.path(my.path,"benchmark/setupStockTun.r"))
source(file.path(my.path,"setupStockTunIII.r"))
source(file.path(my.path,"setupControl.r"))
ISH.ctrl@logN.vars[] <- 1
ISH.ctrl@f.vars["catch",] <- c(1,1,2,2,2,3,4,4)
#ISH_bench.ctrl@catchabilities["AC(VIIaN)",] <- c(1,1,2,3,3,4,4,4)
ISH.ctrl@obs.vars["catch",] <- c(1,2,2,2,3,3,3,3)
ISH.ctrl@obs.vars["AC(VIIaN)",] <- c(4,rep(5,4),rep(6,3))
#- Run final model
#only need the run dir setup below if sam.exe name is longer than 4 characters
ISH.ctrl@sam.binary <- file.path(my.path,"sam.exe")
ISH.sam       <- FLSAM(ISH,ISH.tun,ISH.ctrl,run.dir="C:/Work/HAWG2018/rundir")
ISH.sam       <- SAM2FLR(ctrl=ISH.ctrl,admb.stem="sam",run.dir="C:/Work/HAWG2018/rundir")
ISH.retro       <- FLSAM(ISH,ISH.tun,ISH.ctrl,run.dir="C:/Work/HAWG2018/rundir",5)
ISH.retro     <- SAM2FLR(ctrl=ISH.ctrl,admb.stem="sam",run.dir="C:/Work/HAWG2018/rundir/")
ISH.retro      <- retro(ISH,ISH.tun,ISH.ctrl,0)

#save results
name(ISH.sam) <- "ISH_assessment 2018"
save(ISH,ISH.tun,ISH.ctrl,ISH.sam,file=file.path(output.dir,paste(name(ISH.sam),".RData",sep="")))
save(ISH.retro,file=file.path(output.dir,paste(name(ISH.sam),"_retro.RData",sep="")))

#quick checks
obs.var(ISH.sam)
plot(ISH.sam)
ISH.sam@nlogl
ssb(ISH.sam)$value
fbar(ISH.sam)$value

##binding runs
saveList <- list()
saveList[["benchmark"]] <- ISH_bench.sam
saveList[["2018 assessment"]] <- ISH.sam
#etc
saveList <- as(saveList,"FLSAMs")
plot(saveList)
