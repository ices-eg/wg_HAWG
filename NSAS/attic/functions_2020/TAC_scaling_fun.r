TAC_scaling_fun <- function(  stf,
                                FuY,
                                TACS,
                                CATCH,
                                RECS,
                                TAC_var,
                                rat){
  ImY <- FuY[1]
  FcY <- FuY[2]
  CtY <- FuY[3]
  
  dms         <- dimnames(stf@m)

  #reset harvest for all fleets to F in ImY
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  
  # 15% increase only for the A fleet
  CATCH[,FcY,"A"]       <- TACS[,ImY,"A"]*rat + TAC_var$Ctransfer*TACS[,FcY,'C']
  #CATCH[,FcY,"B"]       <- CATCH[,ImY,"B"]
  
  stf@harvest[,FcY]         <- fleet.harvest2(stk=stf,
                                              iYr=FcY,
                                              CATCH=CATCH[,FcY])
  
    # compute catches
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }
  

  # compute SSB
  ssb.FcY           <- quantSums( stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                    exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                    stf@mat[,FcY,1])
  
  # propagate stock to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  
  CATCH[,CtY,"A"]            <- TACS[,ImY,"A"]*rat*rat + TAC_var$Ctransfer*TACS[,FcY,'C']
  CATCH[,CtY,"B"]            <- CATCH[,ImY,"B"]

  stf@harvest[,CtY]                                           <- fleet.harvest2(stk=stf,
                                                                               iYr=CtY,
                                                                               CATCH=CATCH[,CtY])
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*
                                                                             exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY
  
  return(list(ssb.FcY=ssb.FcY,
              ssb.CtY=ssb.CtY,
              stf=stf))
}