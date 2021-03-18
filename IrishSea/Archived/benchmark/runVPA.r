#- Run VPA

source(file.path(my.path,"benchmark/setupStockTun.r"))
load(file.path(res.dir,"default.RData"))
ISH@catch.n[,ac(2009)] <- (ISH@harvest / (ISH@harvest + ISH@m) * (1-exp(-ISH@harvest - ISH@m)) * ISH@stock.n)[,ac(2009)]

ISH.vpa <- VPA(ISH, fratio = 1, fit.plusgroup = T)
ISHvpa  <- ISH + ISH.vpa
save(ISH,ISH.vpa,file=file.path(res.dir,"VPA.RData"))
stcks   <- FLStocks(VPA=ISHvpa,SAM=ISH)
