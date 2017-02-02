library(FLCore)
library(FLSAM)

rm(list=ls())

#- Set paths
my.path           <- "D:/Repository/ICES_HAWG/wg_HAWG/IrishSea/"
output.dir        <- file.path(my.path,"results")
res.dir           <- file.path(my.path,"benchmark")
data.source       <- file.path(my.path,"data")

#- Setup stock, tun and control
scenario          <- list(); scenario$trimTS   <- T
source(file.path(my.path,"benchmark/setupStockTunIII.r"))
source(file.path(my.path,"benchmark/setupControl.r"))

#- Run default assessment
saveRes         <- list()
counter         <- 1
newS.ctrl       <- ISH.ctrl
newS.tun        <- ISH.tun
newS.stck       <- ISH
newS.ctrl@logN.vars[] <- 1
newS.ctrl@f.vars["catch",] <- c(1,1,2,2,2,3,4,4)
newS.ctrl@catchabilities["AC(VIIaN)",] <- c(1,1,2,3,3,4,4,4)
newS.ctrl@obs.vars["catch",] <- c(1,2,2,2,3,3,3,3)
newS.ctrl@obs.vars["AC(VIIaN)",] <- c(4,rep(5,4),rep(6,3))

newS.ctrl@sam.binary  <- file.path(res.dir,"sam/samISH.exe")
try(unlink(file.path(res.dir,"sam/samISH/"),recursive=TRUE))
dir.create(file.path(res.dir,"sam/samISH/"))
run.dir         <- file.path(res.dir,"sam/samISH/")
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=run.dir)
runSAM(newS.ctrl,run.dir=run.dir,use.pin=FALSE)
newS.sam      <- SAM2FLR(newS.ctrl,admb.stem="samISH",run.dir=run.dir)
newS.stck     <- newS.stck + newS.sam
save(newS.sam,newS.stck,newS.ctrl,newS.tun,file=file.path(res.dir,"newDefaultTS1980.RData"))

#- Free logNvars
newS.ctrl       <- ISH.ctrl
newS.tun        <- ISH.tun
newS.stck       <- ISH
newS.ctrl@logN.vars[] <- 1
newS.ctrl@sam.binary  <- file.path(res.dir,"sam/samISH.exe")
newS.ctrl@logN.vars[6:8] <- c(2,3,3)
try(unlink(file.path(res.dir,"sam/samISH3/"),recursive=TRUE))
dir.create(file.path(res.dir,"sam/samISH3/"))
run.dir         <- file.path(res.dir,"sam/samISH3/")
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=run.dir)
runSAM(newS.ctrl,run.dir=run.dir,use.pin=FALSE)
newS.sam      <- SAM2FLR(newS.ctrl,admb.stem="samISH",run.dir=run.dir)

#- Free f.vars
newS.ctrl       <- ISH.ctrl
newS.tun        <- ISH.tun
newS.stck       <- ISH
newS.ctrl@logN.vars[] <- 1
newS.ctrl@sam.binary  <- file.path(res.dir,"sam/samISH.exe")
newS.ctrl@logN.vars[6:8] <- c(2,3,3)
newS.ctrl@f.vars["catch",] <- c(1,1,2,2,2,3,4,4)
try(unlink(file.path(res.dir,"sam/samISH4/"),recursive=TRUE))
dir.create(file.path(res.dir,"sam/samISH4/"))
run.dir         <- file.path(res.dir,"sam/samISH4/")
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=run.dir)
runSAM(newS.ctrl,run.dir=run.dir,use.pin=FALSE)
newS.sam      <- SAM2FLR(newS.ctrl,admb.stem="samISH",run.dir=run.dir)

#- Free catchabilities
newS.ctrl       <- ISH.ctrl
newS.tun        <- ISH.tun
newS.stck       <- ISH
newS.ctrl@logN.vars[] <- 1
newS.ctrl@sam.binary  <- file.path(res.dir,"sam/samISH.exe")
newS.ctrl@logN.vars[6:8] <- c(2,3,3)
newS.ctrl@f.vars["catch",] <- c(1,1,2,2,2,3,4,4)
newS.ctrl@catchabilities["AC(VIIaN)",] <- c(1,1,2,3,4,5,6,6)
try(unlink(file.path(res.dir,"sam/samISH4/"),recursive=TRUE))
dir.create(file.path(res.dir,"sam/samISH4/"))
run.dir         <- file.path(res.dir,"sam/samISH4/")
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=run.dir)
runSAM(newS.ctrl,run.dir=run.dir,use.pin=FALSE)
newS.sam      <- SAM2FLR(newS.ctrl,admb.stem="samISH",run.dir=run.dir)

#- Free obs.vars
newS.ctrl       <- ISH.ctrl
newS.tun        <- ISH.tun
newS.stck       <- ISH
newS.ctrl@logN.vars[] <- 1
newS.ctrl@sam.binary  <- file.path(res.dir,"sam/samISH.exe")
newS.ctrl@f.vars["catch",] <- c(1,1,2,2,2,3,4,4)
newS.ctrl@catchabilities["AC(VIIaN)",] <- c(1,1,2,3,3,4,4,4)

newS.ctrl@obs.vars["catch",] <- c(1,2,2,2,3,3,3,3)
newS.ctrl@obs.vars["AC(VIIaN)",] <- c(4,rep(5,4),rep(6,3))

try(unlink(file.path(res.dir,"sam/samISH7/"),recursive=TRUE))
dir.create(file.path(res.dir,"sam/samISH7/"))
run.dir         <- file.path(res.dir,"sam/samISH7/")
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=run.dir)
runSAM(newS.ctrl,run.dir=run.dir,use.pin=FALSE)
newS.sam      <- SAM2FLR(newS.ctrl,admb.stem="samISH005",run.dir=run.dir)


head(params(newS.sam),30)
obsvar.plot(newS.sam)
cor.plot(newS.sam)
catchabilities(newS.sam)


#- Run retro
scenario          <- list(); scenario$trimTS   <- T
source(file.path(my.path,"benchmark/setupStockTunIII.r"))
source(file.path(my.path,"benchmark/setupControl.r"))
newS.ctrl       <- ISH.ctrl
newS.tun        <- ISH.tun
newS.stck       <- ISH
newS.ctrl@logN.vars[] <- 1
newS.ctrl@f.vars["catch",] <- c(1,1,2,2,2,3,4,4)
newS.ctrl@catchabilities["AC(VIIaN)",] <- c(1,1,2,3,3,4,4,4)
newS.ctrl@obs.vars["catch",] <- c(1,2,2,2,3,3,3,3)
newS.ctrl@obs.vars["AC(VIIaN)",] <- c(4,rep(5,4),rep(6,3))
newS.ctrl@sam.binary <- file.path(res.dir,"sam/samISH.exe")
dir.create(file.path(res.dir,"sam/samISH9/"))
run.dir         <- file.path(res.dir,"sam/samISH9/")
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=run.dir)
runSAM(newS.ctrl,run.dir=run.dir,use.pin=FALSE)
newS.sam      <- SAM2FLR(newS.ctrl,admb.stem="samISH",run.dir=run.dir)

save(newS.sam,file=file.path(res.dir,"samISH_RW_rec.RData"))
saveRes <- list()
saveRes[["RW"]] <- newS.sam
newS.ctrl@sam.binary <- file.path(res.dir,"sam/samISH.exe")
newS.sam        <- FLSAM(newS.stck,newS.tun,newS.ctrl)
saveRes[["noRW"]] <- newS.sam

saveRes <- as(saveRes,"FLSAMs")


newS.retro      <- retro(newS.stck,newS.tun,newS.ctrl,5)

retroParams(newS.retro)

retros <- list()
retros[["005"]] <- newS.retro
retros[["006"]] <- newS.retro
retros[["007"]] <- newS.retro
retros[["008"]] <- newS.retro
retros[["009"]] <- newS.retro
retros[["01"]]  <- newS.retro










#- Drop 2001 Acoustic survey
try(unlink(file.path(res.dir,"sam/samISH3/"),recursive=TRUE))
dir.create(file.path(res.dir,"sam/samISH3/"))
run.dir         <- file.path(res.dir,"sam/samISH3/")
newS.ctrl       <- ISH.ctrl
newS.tun        <- ISH.tun
newS.stck       <- ISH
newS.ctrl@logN.vars[] <- 1
newS.tun[[1]]@index[ac(1:6),ac(2001)] <- -1
newS.ctrl@sam.binary  <- file.path(res.dir,"sam/samISH.exe")
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=run.dir)
runSAM(newS.ctrl,run.dir=run.dir,use.pin=FALSE)
newS.sam      <- SAM2FLR(newS.ctrl,admb.stem="samISH",run.dir=run.dir)
newS.stck     <- newS.stck + newS.sam
saveRes[[counter]]  <- newS.sam



saveRes         <- list()
scenario        <- list(); scenario$trimTS   <- T
source(file.path(my.path,"benchmark/setupStockTunIII.r"))
source(file.path(my.path,"benchmark/setupControl.r"))
newS.ctrl       <- ISH.ctrl
newS.tun        <- ISH.tun
newS.stck       <- ISH
newS.ctrl@logN.vars[] <- 1
newS.ctrl@f.vars["catch",] <- c(1,1,2,2,2,3,4,4)
newS.ctrl@catchabilities["AC(VIIaN)",] <- c(1,1,2,3,3,4,4,4)
newS.ctrl@obs.vars["catch",] <- c(1,2,2,2,3,3,3,3)
newS.ctrl@obs.vars["AC(VIIaN)",] <- c(4,rep(5,4),rep(6,3))

#q range
admb.exec             <- "samISH"
newS.ctrl@sam.binary  <- file.path(res.dir,paste("sam/",admb.exec,".exe",sep=""))
run.dir <- file.path(res.dir,admb.exec)
try(unlink(run.dir)); dir.create(run.dir)
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=run.dir)
runSAM( newS.ctrl,run.dir=run.dir,use.pin=FALSE)
newS.sam      <- SAM2FLR(newS.ctrl,admb.stem="samISH",run.dir=run.dir)

saveRes[["default"]] <- newS.sam
newS.ctrl@sam.binary <- file.path(res.dir,"sam/samq08.exe")
newS.sam <- FLSAM(newS.stck,newS.tun,newS.ctrl)




saveRes[["RW"]] <- newS.sam
newS.ctrl@sam.binary <- file.path(res.dir,"sam/samISH.exe")
newS.sam        <- FLSAM(newS.stck,newS.tun,newS.ctrl)
saveRes[["noRW"]] <- newS.sam

saveRes <- as(saveRes,"FLSAMs")

