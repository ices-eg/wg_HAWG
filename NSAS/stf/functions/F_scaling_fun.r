F_scaling_fun <- function(  stf,
                            FuY,
                            CATCH,
                            RECS,
                            referencePoints,
                            Ftar){
  
  ImY <- FuY[1]
  FcY <- FuY[2]
  CtY <- FuY[3]
  
  #reset harvest for all fleets
  stf@harvest[,FcY]           <- stf@harvest[,ImY]
  
  dms <- dimnames(stf@m)
  
  # optimize F scalors against FA and true TAC C and D
  res <- matrix(NA,
                nrow=4,
                ncol=dims(stf)$iter,
                dimnames=list(dimnames(stf@stock.n)$unit,
                              dimnames(stf@stock.n)$iter))
  
  # assume same F in FcY for the B fleet
  for(iTer in 1:dims(stf)$iter)
    res[,iTer]                  <- nls.lm(par=rep(1,4),
                                          lower=rep(1e-08,4),
                                          upper=NULL,
                                          find.FC,
                                          stk=iter(stf[,FcY],iTer),
                                          F26tar=Ftar,
                                          CATCH=iter(CATCH[,FcY],iTer),
                                          jac=NULL,
                                          nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000))$par
  
  # create 4 element vector. Scalor A fleet = scalor B fleet
  #res <- cbind(res[1,],res[1,],res[2,],res[3,])
  
  # update F with scalors
  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
  
  # update catch and landings for each fleet in Forecast year
  for(i in dms$unit){
    stf@catch.n[,FcY,i]         <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]           <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]      <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]        <- computeLandings(stf[,FcY,i])
  }
  
  ssb.FcY <- quantSums( stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                          exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1] *
                                stf@m.spwn[,FcY,1]) * stf@mat[,FcY,1])
  
  # propagate stock to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  # calculate SSB
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY
  
  return(list(ssb.FcY=ssb.FcY,
              ssb.CtY=ssb.CtY,
              stf=stf))
}