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

pdf(file=file.path(res.dir,paste(scenario$saveName,".pdf",sep="")))

saveRes       <- list()
newS.ctrl     <- ISH.ctrl
newS.tun      <- ISH.tun
newS.stck     <- ISH
newS.sam      <- FLSAM(newS.stck,newS.tun,newS.ctrl)
newS.stck     <- newS.stck + newS.sam
saveRes[[1]]  <- newS.sam
newS.ctrl@sam.binary <- file.path(res.dir,"sam/samssbq1.exe")
newS.sam      <- FLSAM(newS.stck,newS.tun,newS.ctrl)
newS.stck     <- newS.stck + newS.sam
saveRes[[2]]  <- newS.sam
load(file.path(res.dir,"default.RData"))
saveRes[[3]]  <- ISH.sam

sams            <- as(saveRes,"FLSAMs")
names(sams)     <- c("newSSBsurveyQFree","newSSBsurveyQ1","default")
print(plot(y=AIC(sams),x=af(names(sams))))
print(plot(sams))
save(sams,file=file.path(res.dir,paste(scenario$saveName,".RData",sep="")))

par(mfrow=c(1,length(sams)))
for(iRun in 1:(length(sams))){
  obv <- obs.var(sams[[iRun]])
  obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
  obv <- obv[order(obv$value),]
  bp <- barplot(obv$value,ylab="Observation Variance",
         main="Observation variances by data source",col=factor(obv$fleet),ylim=c(0,1.3))
  axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
  legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)
}
dev.off()

