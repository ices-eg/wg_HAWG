MP_fun <- function( stf,
                    FuY,
                    CATCH,
                    RECS,
                    referencePoints,
                    managementRule,
                    TAC_var,
                    TACS,
                    f01,
                    f26){
  
  ImY <- FuY[1]
  FcY <- FuY[2]
  CtY <- FuY[3]
  
  dms <- dimnames(stf@m)
  
  # set selectivity in FcY to ImY
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  
  res <- matrix(NA,
                nrow=2,
                ncol=dims(stf)$iter,
                dimnames=list(c("A","B"),
                              dimnames(stf@stock.n)$iter))
  
  
  # if case for HCR A and B - the function to optimize is different
  if(managementRule$HCR == "A"){
    for(iTer in 1:dims(stf)$iter){
      res[,iTer]              <- nls.lm(par=rep(1,2),
                                        find.FAB_HCRA,
                                        stk=stf[,FcY,c("A","B"),,,iTer],
                                        mpPoints=referencePoints,
                                        f26=f26,
                                        f01=f01,
                                        jac=NULL,
                                        lower=rep(1e-8,2),
                                        upper=NULL,
                                        control=nls.lm.control(maxiter=1000))$par
    }
  }
  if(managementRule$HCR == "B"){
    for(iTer in 1:dims(stf)$iter){
      res[,iTer]              <- nls.lm(par=rep(1,2),
                                        find.FAB_HCRB,
                                        stk=stf[,FcY,c("A","B"),,,iTer],
                                        mpPoints=referencePoints,
                                        f26=f26,
                                        f01=f01,
                                        jac=NULL,
                                        lower=rep(1e-8,2),
                                        upper=NULL,
                                        control=nls.lm.control(maxiter=1000))$par
    }
  }
  
  
  
  # resulting F for A and B fleets
  stf@harvest[,FcY,c("A","B")]         <- sweep(stf@harvest[,FcY,c("A","B")],c(3,6),res,"*")
  
  # case of 0 catch, set F of C and D fleet to 0
  if(CATCH[,FcY,'C'] < 1){
    stf@harvest[,FcY,'C'] <- 0
  }
  if(CATCH[,FcY,'D'] < 1){
    stf@harvest[,FcY,'D'] <- 0
  }
  
  #- Calculate TACs in FcY
  # assume F from ImY for fleet C and D
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }
  TACS[,FcY,c("A","B")]   <- stf@catch[,FcY,c("A","B")]
  
  # compute alternative SSB (only accounting for fleet A and B)
  totF                      <- unitSums(stf@harvest[,FcY,c("A","B")])
  SSBHCR                    <- quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] * exp(-totF*stf@harvest.spwn[,FcY,1] - stf@m[,FcY,1]) * stf@mat[,FcY,1])
  
  # IAV
  if(is.null(managementRule$TACIAV) == F){
    mrF                             <- managementRule$TACIAV
    
    idx                             <- which(SSBHCR > referencePoints$Btrigger)
    for(imrF in mrF){
      bidx                                    <- which(iter(TACS[,FcY,imrF],idx) > (1.25 * iter(TACS[,ImY,imrF],idx)))
      iter(TACS[,FcY,imrF],idx[bidx])    <- 1.25 * iter(TACS[,ImY,imrF,],idx[bidx])
      sidx                                    <- which(iter(TACS[,FcY,imrF],idx) < (0.8 * iter(TACS[,ImY,imrF],idx)))
      iter(TACS[,FcY,imrF],idx[sidx])    <- 0.8 * iter(TACS[,ImY,imrF,],idx[sidx])
    }
  }
  
  # realised catch for the forecast year
  # Note 1: assuming same Ctransfer as for the ImY
  # Note 2: assuming same B uptake as for the ImY
  CATCH[,FcY,"A"]       <- TACS[,FcY,'A'] + TAC_var$Ctransfer * TACS[,FcY,'C']
  CATCH[,FcY,'B']       <- TACS[,FcY,'B'] * TAC_var$Buptake
  CATCH[,FcY,'C']       <- stf@catch[,FcY,'C']
  CATCH[,FcY,'D']       <- stf@catch[,FcY,'D']
  
  # calculate realised F by the fishing fleet
  res <- matrix(NA,
                nrow=4,
                ncol=dims(stf)$iter,
                dimnames=list(c("A","B",'C','D'),
                              dimnames(stf@stock.n)$iter))
  
  
  for(iTer in 1:dims(stf)$iter){
    res[,iTer] <- nls.lm(par=rep(1,4),
                         CATCH2sel,
                         stk=stf[,FcY],
                         CATCH=CATCH[,FcY],
                         iTer=iTer,
                         jac=NULL,
                         lower=rep(1e-15,4),
                         upper=NULL,
                         control=nls.lm.control(maxiter=1000))$par
  }
  
  
  # calculate true F and true SSB for forecast year
  stf@harvest[,FcY] <- sweep(stf@harvest[,FcY,],c(3,6),res,"*")
  ssb.FcY <- quantSums( stf@stock.n[,FcY,1] * 
                          stf@stock.wt[,FcY,1] *
                          stf@mat[,FcY,1] *
                          exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  
  #  Update to continuation year
  stf@harvest[,CtY]                                           <- stf@harvest[,FcY]
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums( stf@stock.n[,CtY,1] * 
                                                                              stf@stock.wt[,CtY,1]*
                                                                              stf@mat[,CtY,1]*
                                                                              exp(-unitSums(stf@harvest[,CtY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY
  
  return(list(ssb.FcY=ssb.FcY,
              ssb.CtY=ssb.CtY,
              stf=stf))
}