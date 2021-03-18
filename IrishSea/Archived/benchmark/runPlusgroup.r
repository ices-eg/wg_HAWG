source(file.path(my.path,"benchmark/setupStockTun.r"))
source(file.path(my.path,"benchmark/setupControl.r"))

#-------------------------------------------------------------------------------
#- Run scenario
#-------------------------------------------------------------------------------
respref <- scenario$saveName
resfile <- file.path(res.dir,paste(respref,".RData",sep=""))
pg.sams <- list()
for(pg in sort(scenario$setting)){
  pg.stck     <- setPlusGroup(ISH,pg)
  pg.stck@stock.wt[pg,ac(2009)]          <- yearMeans(pg.stck@stock.wt[pg,ac(2008,2010)])
  pg.stck@catch.wt[pg,ac(2009)]          <- yearMeans(pg.stck@catch.wt[pg,ac(2008,2010)])
  pg.stck@landings.wt[pg,ac(2009)]       <- yearMeans(pg.stck@landings.wt[pg,ac(2008,2010)])
  pg.stck@stock.wt[,ac(2014)]            <- yearMeans(pg.stck@stock.wt[,ac(2011:2013)])
  pg.stck@catch.wt[,ac(2014)]            <- yearMeans(pg.stck@catch.wt[,ac(2011:2013)])
  pg.stck@landings.wt[,ac(2014)]         <- yearMeans(pg.stck@landings.wt[,ac(2011:2013)])
  pg.tun      <- ISH.tun
  AgeSur      <- do.call(cbind,lapply(ISH.tun,function(x){return(range(x)[c("min","max")])}))
  idx         <- which(AgeSur["max",]>1)
  if(length(idx)>0){
    for(iSur in idx){
      pg.tun[[iSur]]@index[ac(pg),] <- quantSums(pg.tun[[iSur]]@index[ac(pg:AgeSur["max",iSur]),])
      pg.tun[[iSur]]                <- trim(pg.tun[[iSur]],age=AgeSur["min",iSur]:pg)
      pg.tun[[iSur]]@range["plusgroup"] <- pg
    }
  }
  pg.ctrl     <- drop.from.control(ISH.ctrl,ages=pg:ISH.ctrl@range["max"]+1)
  pg.ctrl@states["catch",ac((pg-1):pg)] <- 101
  pg.ctrl@logN.vars                     <- c(1,rep(2,length(2:pg)))
  pg.ctrl@obs.vars["AC(VIIaN)",ac(4:pg)]<- 202
  pg.ctrl@range[c("max","plusgroup")]   <- pg
  pg.ctrl@range["maxfbar"]              <- pg.stck@range["maxfbar"]
  pg.ctrl     <- update(pg.ctrl)

  #Perform assessment
  pg.sam      <- FLSAM(pg.stck,pg.tun,pg.ctrl)

  #Store results
  pg.sam@name <- sprintf("%i+",pg)
  pg.sams[pg.sam@name] <- pg.sam
}
save(pg.sams,file=resfile)
