source(file.path(my.path,"benchmark/setupStockTun.r"))
source(file.path(my.path,"benchmark/setupControl.r"))

#-------------------------------------------------------------------------------
#- Run scenario
#-------------------------------------------------------------------------------
respref <- scenario$saveName
resfile <- file.path(res.dir,paste(respref,".RData",sep=""))

  r.stck    <- ISH
  r.tun     <- ISH.tun
  r.ctrl    <- ISH.ctrl
  #Perform assessment
  r.sams    <- retro(r.stck,r.tun,r.ctrl,retro=scenario$nyears)

save(r.sams,file=resfile)
