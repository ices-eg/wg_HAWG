source(file.path(my.path,"benchmark/setupStockTun.r"))
source(file.path(my.path,"benchmark/setupControl.r"))

#-------------------------------------------------------------------------------
#- Run scenario
#-------------------------------------------------------------------------------
respref <- scenario$saveName
resfile <- file.path(res.dir,paste(respref,".RData",sep=""))
m.sams <- list()

for(m in c("old","new","NS2015")){
  if(m == "new")
    newM    <- c(0.647,0.364,0.325,0.300,0.281,0.271,0.246,0.246)
  if(m=="NS2015")
    NSM     <- c(0.77428,0.63205,0.35916,0.31971,0.29378,0.27557,0.26493,0.24049,0.24049)[1:8]#[2:9]
  m.stck    <- ISH
  if(m == "new")
    m.stck@m[]<- newM
  if(m == "NS2015")
    m.stck@m[]<- NSM
  m.tun     <- ISH.tun
  m.ctrl    <- ISH.ctrl
  #Perform assessment
  m.sam       <- FLSAM(m.stck,m.tun,m.ctrl)

  #Store results
  m.sam@name  <- paste("Natmort",m)
  m.sams[m.sam@name] <- m.sam
}
save(m.sams,file=resfile)
