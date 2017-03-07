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
newS.tun        <- ISH.tun[-2]
newS.stck       <- ISH
newS.ctrl       <- FLSAM.control(newS.stck,newS.tun,scaleYears=1980:2015)
newS.ctrl@states[1,]<- ISH.ctrl@states[1,]
newS.ctrl@logN.vars[] <- 1
newS.ctrl@f.vars["catch",] <- c(1,1,2,2,2,3,4,4)
newS.ctrl@catchabilities["AC(VIIaN)",] <- c(1,1,2,3,3,4,4,4)
newS.ctrl@obs.vars["catch",] <- c(1,2,2,2,3,3,3,3)
newS.ctrl@obs.vars["AC(VIIaN)",] <- c(4,rep(5,4),rep(6,3))
newS.ctrl@scalePars[] <- NA
newS.ctrl <- update(newS.ctrl)

newS.ctrl@sam.binary  <- file.path(res.dir,"sam/samVersions/samOriginalNoRW.exe")
try(unlink(file.path(res.dir,"sam/samISH4/"),recursive=TRUE))
dir.create(file.path(res.dir,"sam/samISH4/"))
run.dir         <- file.path(res.dir,"sam/samISH4/")
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=run.dir)
runSAM(newS.ctrl,run.dir=run.dir,use.pin=T)
SAM2FLR(newS.ctrl,admb.stem="samOriginalNoRWNoCaA",run.dir=run.dir)

short <- pin.sam
withoutQ   <- SAM2FLR(newS.ctrl,admb.stem="samNoQSSBVar04NoRW",run.dir=run.dir)
original     <- SAM2FLR(newS.ctrl,admb.stem="samOriginalNoRW",run.dir=run.dir)

saveRes[["withoutQ"]] <- withoutQ
saveRes[["withQ"]]    <- withQ

a <- subset(params(original),!name %in% c("U","ssb","logssb","logCatch","fbar","logfbar","tsb","logtsb"))
b <- subset(params(withoutQ),!name %in% c("U","ssb","logssb","logCatch","fbar","logfbar","tsb","logtsb"))
par(oma=c(6,2,2,2))
plot(round((exp(b$value) - exp(a$value))/exp(a$value) * 100),xlab="",xaxt="n",ylab="Percentage difference",pch=19)
abline(h=0,col=2,lty=2)
labnames <- c(paste("catchability age",c("1:2","3","4:5","6:8")),paste("RW-F age",c("1:2","3:5","6","7:8")),"RW-N age 1:8",paste("Obs var Catch age",c("1","2:4","5:8")),paste("Obs var Acoustic age",c("1","2:5","6:8")))
axis(1,at=1:15,las=2,labels=labnames)

SAMS <- FLSAMs(original=original,noQ=withoutQ)
plot(SAMS)

resOrig <- subset(residuals(original),fleet == "Fleet 2")
resNoQ  <- subset(residuals(withoutQ),fleet == "Fleet 2")
res     <- rbind(cbind(resOrig,cat="Original"),cbind(resNoQ,cat="Q=1"))

xyplot(std.res ~ year | as.factor(age),groups=cat,data=res,type="l",auto.key=T)

sumOrig <- aggregate(resOrig$std.res,by=list(resOrig$age),FUN=sum)
sumNoQ <- aggregate(resNoQ$std.res,by=list(resNoQ$age),FUN=sum)
i <- barplot(height=t(as.matrix(cbind(sumOrig,sumNoQ)[,c(2,4)])),beside=T)
axis(1,at=colMeans(i),labels=paste("age",1:8),las=2)
legend("topleft",legend=c("Original","noQ"),fill=c(1,"grey"))

ratioEarly <- subset(ssb(withoutQ),year %in% 1994:2006)$value / subset(ssb(original),year %in% 1994:2006)$value
ratioLate  <- subset(ssb(withoutQ),year %in% 2007:2015)$value / subset(ssb(original),year %in% 2007:2015)$value
plot(x=1994:2015,y=c(ratioEarly,ratioLate),type="l",ylim=c(0,2),xlab="Years",ylab="Ratio noQ/Original",las=1)
abline(h=1,col=2)

mean(subset(ssb(withoutQ),year %in% 2007:2015)$value) / mean(subset(ssb(withoutQ),year %in% 1994:2006)$value)
mean(subset(ssb(original),year %in% 2007:2015)$value) / mean(subset(ssb(original),year %in% 1994:2006)$value)

ssbTun <- as.data.frame(quantSums(ISH.tun[[1]]@index*ISH@stock.wt[,ac(1994:2015)]*ISH@mat[,ac(1994:2015)]))

mean(subset(ssbTun,year %in% 2007:2015)$data) / mean(subset(ssbTun,year %in% 1994:2006)$data)




saveRes <- as(saveRes,"FLSAMs")

newS.ctrl@sam.binary  <- file.path(res.dir,"sam/samVersions/samNoQSSBVar008NoRW.exe")
try(unlink(file.path(res.dir,"sam/samISH2/"),recursive=TRUE))
dir.create(file.path(res.dir,"sam/samISH2/"))
run.dir         <- file.path(res.dir,"sam/samISH2/")
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=run.dir,pin.sam=pin.sam)
runSAM(newS.ctrl,run.dir=run.dir,use.pin=TRUE)
pin.sam      <- SAM2FLR(newS.ctrl,admb.stem="samNoQSSBVar01NoRW",run.dir=run.dir)


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
newS.ctrl@sam.binary  <- file.path(res.dir,"sam/samVersions/samPriorSSBVar04NoRW.exe")
newS.ctrl@f.vars["catch",] <- c(1,1,2,2,2,3,4,4)
newS.ctrl@catchabilities["AC(VIIaN)",] <- c(1,1,2,3,3,4,4,4)

newS.ctrl@obs.vars["catch",] <- c(1,2,2,2,3,3,3,3)
newS.ctrl@obs.vars["AC(VIIaN)",] <- c(4,rep(5,4),rep(6,3))

try(unlink(file.path(res.dir,"sam/samISH15/"),recursive=TRUE))
dir.create(file.path(res.dir,"sam/samISH15/"))
run.dir         <- file.path(res.dir,"sam/samISH15/")
FLR2SAM(newS.stck,newS.tun,newS.ctrl,run.dir=run.dir)
runSAM(newS.ctrl,run.dir=run.dir,use.pin=FALSE)
newS.sam      <- SAM2FLR(newS.ctrl,admb.stem="samPriorSSBVarNoRW",run.dir=run.dir)


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

