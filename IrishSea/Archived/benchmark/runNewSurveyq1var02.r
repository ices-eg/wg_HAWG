#-------------------------------------------------------------------------------
#- Setup control
#-------------------------------------------------------------------------------
source(file.path(my.path,"benchmark/setupStockTunIII.r"))
source(file.path(my.path,"benchmark/setupControl.r"))

##pdf(file=file.path(res.dir,paste(scenario$saveName,".pdf",sep="")))

saveRes         <- list()
counter         <- 1
newS.ctrl       <- ISH.ctrl
newS.tun        <- ISH.tun
newS.stck       <- ISH
newS.ctrl@logN.vars[] <- 1
newS.ctrl@sam.binary <- file.path(res.dir,"sam/samISH.exe")
try(unlink(file.path(res.dir,"sam/samISH/"),recursive=TRUE))
dir.create(file.path(res.dir,"sam/samISH/"))
run.dir         <- file.path(res.dir,"sam/samISH/")
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=run.dir)
runSAM(newS.ctrl,run.dir=run.dir,use.pin=FALSE)
newS.sam      <- SAM2FLR(newS.ctrl,admb.stem="samISH",run.dir=run.dir)
newS.stck     <- newS.stck + newS.sam
saveRes[[counter]]  <- newS.sam
counter       <- counter + 1
load(file.path(res.dir,"defaultTS1980.RData"))
saveRes[[counter]]  <- ISH.sam

save(newS.sam,newS.stck,newS.tun,newS.ctrl,file=file.path(res.dir,paste(scenario$saveName,".RData")))

