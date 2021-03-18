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
pdf(file=file.path(res.dir,paste(scenario$saveName,".pdf",sep="")))
print(plot(ISH.tun[["AC(VIIaNSpawn)"]],main="Survey",type="internal"))


settings        <- list(minFreedom=list(catchabilities=c(NA,rep(5,7)),
                                        obs.vars=c(NA,rep(9,7))),
                        maxFreedom=list(catchabilities=c(NA,5:11),
                                        obs.vars=c(NA,9:15)),
                        sameAcoust=list(catchabilities=c(NA,rep(5,5),6,6),
                                        obs.vars=c(NA,rep(5,5),6,6)),
                        sensAcoust=list(catchabilities=c(NA,5,rep(6,6)),
                                        obs.vars=c(NA,9,9,10,10,10,11,12)))

saveRes         <- list()
for(iRun in 1:length(settings)){
  newS.ctrl     <- ISH.ctrl
  newS.tun      <- ISH.tun
  newS.stck     <- ISH
  iSet          <- settings[[iRun]]
  for(iSlot in names(iSet))
    slot(newS.ctrl,iSlot)["AC(VIIaNSpawn)",] <- iSet[[iSlot]]
  newS.sam      <- FLSAM(newS.stck,newS.tun,newS.ctrl)
  newS.stck     <- newS.stck + newS.sam
  saveRes[[iRun]] <- newS.sam
}
saveRes[[iRun+1]] <- ISH.sam

sams            <- as(saveRes,"FLSAMs")
names(sams)     <- c("min","max","same","sensible","default")

respref         <- "newSurvey"
resfile         <- file.path(res.dir,paste(respref,".RData",sep=""))

save(sams,file=file.path(res.dir,paste(scenario$saveName,".RData")))
print(plot(y=AIC(sams),x=af(names(sams))))
print(plot(sams))

par(mfrow=c(1,length(sams)))
for(iRun in 1:(length(settings)+1)){
  obv <- obs.var(sams[[iRun]])
  obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
  obv <- obv[order(obv$value),]
  bp <- barplot(obv$value,ylab="Observation Variance",
         main="Observation variances by data source",col=factor(obv$fleet))
  axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
  legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)
}

catch <- catchabilities(sams[["sensible"]])
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
          scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
          type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
          subset=fleet %in% c("AC(VIIaNSpawn)"),
          main="Survey catchability parameters",ylab="Catchability",xlab="Age"))
dev.off()

