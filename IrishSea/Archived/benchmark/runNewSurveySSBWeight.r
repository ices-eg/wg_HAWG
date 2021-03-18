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

##pdf(file=file.path(res.dir,paste(scenario$saveName,".pdf",sep="")))

saveRes         <- list()
counter         <- 1
for(iVar in c("0025","005","01","02","03","04")){
  newS.ctrl     <- ISH.ctrl
  newS.tun      <- ISH.tun
  newS.stck     <- ISH
  newS.ctrl@logN.vars[] <- 1
  newS.ctrl@sam.binary <- file.path(res.dir,paste("sam/samssbvar",iVar,".exe",sep=""))
  try(unlink(file.path(res.dir,paste("sam/var",iVar,sep="")),recursive=TRUE))
  dir.create(file.path(res.dir,paste("sam/var",iVar,sep="")))
  FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=file.path(res.dir,"sam",paste("var",iVar,sep="")))
  runSAM(newS.ctrl,run.dir=file.path(res.dir,"sam",paste("var",iVar,sep="")))
  newS.sam      <- SAM2FLR(newS.ctrl,admb.stem=paste("samssbvar",iVar,sep=""),run.dir=file.path(res.dir,"sam",paste("var",iVar,sep="")))
  newS.stck     <- newS.stck + newS.sam
  saveRes[[counter]]  <- newS.sam
  counter       <- counter + 1
}
load(file.path(res.dir,"defaultTS1980.RData"))
saveRes[[counter]]  <- ISH.sam

a <- FLStocks("var0.025"=saveRes[[1]]+ISH,"var0.4"=saveRes[[2]]+ISH,"var0.3"=saveRes[[3]]+ISH,"var0.2"=saveRes[[4]]+ISH,"var0.1"=saveRes[[5]]+ISH,"default"=ISH.sam+ISH)
plot(a)