#-------------------------------------------------------------------------------
#- Run scenario
#-------------------------------------------------------------------------------
source(file.path(my.path,"benchmark/setupStockTun.r"))
source(file.path(my.path,"benchmark/setupControl.r"))
ISH.tun[[1]]@index[,ac(2001)] <- NA

newS.ctrl       <- ISH.ctrl
newS.tun        <- ISH.tun
newS.stck       <- ISH

#-------------------------------------------------------------------------------
#- Setup control
#-------------------------------------------------------------------------------
pdf(file=file.path(res.dir,paste(scenario$saveName,".pdf",sep="")))
print(plot(ISH.tun[[1]],main="Survey",type="internal"))

newS.sam        <- FLSAM(newS.stck,newS.tun,newS.ctrl)

saveRes       <- list()
saveRes[[1]]  <- newS.sam
load(file.path(res.dir,"default.RData"))
saveRes[[2]]  <- ISH.sam

sams            <- as(saveRes,"FLSAMs")
names(sams)     <- c("remove2001Survey","default")
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