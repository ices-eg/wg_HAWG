#-------------------------------------------------------------------------------
#- Run scenario
#-------------------------------------------------------------------------------
source(file.path(my.path,"benchmark/setupStockTunII.r"))
source(file.path(my.path,"benchmark/setupControl.r"))

newS.ctrl       <- ISH.ctrl
newS.tun        <- ISH.tun
newS.stck       <- ISH

#-------------------------------------------------------------------------------
#- Setup control
#-------------------------------------------------------------------------------
source(file.path(my.path,"benchmark/setupStockTunIII.r"))
source(file.path(my.path,"benchmark/setupControl.r"))

#pdf(file=file.path(res.dir,paste(scenario$saveName,".pdf",sep="")))

saveRes         <- list()
counter         <- 1
newS.ctrl     <- ISH.ctrl
newS.tun      <- ISH.tun
newS.stck     <- ISH
newS.ctrl@sam.binary <- file.path(res.dir,"sam/freeR.exe")
try(unlink(file.path(res.dir,"sam/freeR"),recursive=TRUE))
dir.create(file.path(res.dir,"sam/freeR"))
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=file.path(res.dir,"sam/freeR"))
runSAM(newS.ctrl,run.dir=file.path(res.dir,"sam/freeR"))
newS.sam      <- SAM2FLR(newS.ctrl,admb.stem="freeR",run.dir=file.path(res.dir,"sam/freeR"))
newS.stck     <- newS.stck + newS.sam
saveRes[[1]]  <- newS.sam

load(file.path(res.dir,"defaultTS1980.RData"))
saveRes[[2]]  <- ISH.sam

stcks         <- FLStocks(noProcError=newS.stck,default=ISH+ISH.sam)
plot(stcks)
