nf_fun <- function(stf,
                   RECS,
                   FuY){
  
  ImY <- FuY[1]
  FcY <- FuY[2]
  CtY <- FuY[3]
  
  dms <- dimnames(stf@m)
  
  # F = 0 for FcY and CtY
  stf@harvest[,FcY] <- 0
  stf@harvest[,CtY] <- stf@harvest[,FcY]
  
  stkZ      <- unitSums(stf@harvest[,FcY]) + stf@m[,FcY,1]
  stf@catch <- apply(sweep(stf@stock.n[,FcY] * stf@catch.wt[,FcY] * sweep(stf@harvest[,FcY],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)

  ssb.FcY <- quantSums( stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                          exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                          stf@mat[,FcY,1])
  
  # propagate stock to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  # calculate SSB
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,CtY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY
  
  return(list(ssb.FcY=ssb.FcY,
              ssb.CtY=ssb.CtY,
              stf=stf))
}