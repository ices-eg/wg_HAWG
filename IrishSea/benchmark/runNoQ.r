source(file.path(my.path,"benchmark/setupStockTun.r"))
source(file.path(my.path,"benchmark/setupControl.r"))

#- Run catchability set to 1 for ages 4>

#- Run set all catchabilities for ages greater than 3 to 1
noq.ctrl          <- ISH.ctrl
noq.tun           <- ISH.tun
noq.stck          <- ISH

noq.ctrl@catchabilities["AC(VIIaN)",]           <- c(1:3,rep(NA,5))

noq.sam           <- FLSAM(noq.stck,noq.tun,noq.ctrl)
noq.stck          <- noq.stck + noq.sam
save(noq.stck,noq.tun,noq.ctrl,noq.sam,file=file.path(res.dir,paste(scenario$saveName,"noqage48.RData",sep="")))

#- Run remove ages > 3 from acoustic survey to see if it makes a difference
max3.ctrl         <- ISH.ctrl
max3.tun          <- ISH.tun
max3.stck         <- ISH

max3.tun[["AC(VIIaN)"]]<- trim(max3.tun[["AC(VIIaN)"]],age=1:3)
max3.ctrl@catchabilities["AC(VIIaN)",] <- c(1:3,rep(NA,5))
max3.ctrl@obs.vars["AC(VIIaN)",]  <- c(ISH.ctrl@obs.vars["AC(VIIaN)",1:3],rep(NA,5))

max3.sam          <- FLSAM(max3.stck,max3.tun,max3.ctrl)
max3.stck         <- max3.stck + max3.sam
save(max3.stck,max3.tun,max3.ctrl,max3.sam,file=file.path(res.dir,paste(scenario$saveName,"max3age.RData")))

load(file.path(res.dir,"defaultTS1980.RData"))
noQ.sams         <- as(list(orig=ISH.sam,noq=noq.sam,max3=max3.sam),"FLSAMs")


