fmsyAR_fun_no_transfer_Btarget <- function( stf,
                                            FuY,
                                            TACS,
                                            RECS,
                                            referencePoints,
                                            TAC_var,
                                            f01,
                                            f26){
  
  ImY <- FuY[1]
  FcY <- FuY[2]
  CtY <- FuY[3]
  
  dms <- dimnames(stf@m)
  
  # Set selectivity as in ImY
  stf@harvest[,FcY]   <- stf@harvest[,ImY]
  
  # optimize F scalors against FA and true TAC C and D
  res <- matrix(NA,
                nrow=4,
                ncol=dims(stf)$iter,
                dimnames=list(c('A','B','C','D'),
                              dimnames(stf@stock.n)$iter))
  
  CATCH <- TACS
  
  # assume no transfer of the C fleet TAC, i.e. catches only in IIIa
  CATCH[,c(FcY,CtY),'C'] <- TACS[,FcY,'C']*TAC_var$Csplit
  CATCH[,c(FcY,CtY),'D'] <- TACS[,FcY,'D']*TAC_var$Dsplit*TAC_var$Duptake
  
  for(iTer in 1:dims(stf)$iter)      #stf.=stf,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,4), # A scalor = B scalor, therefore 3 scalors
                                          lower=rep(1e-8,4),
                                          upper=NULL,
                                          find.FCAR_Btarget,
                                          stk=iter(stf[,FcY],iTer),
                                          CATCH=iter(CATCH[,FcY],iTer),
                                          refs.=referencePoints,
                                          f01=f01,
                                          f26=f26,
                                          jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),
                                                                  maxiter = 1000))$par
  
  
  # update F with scalors
  stf@harvest[,FcY,c('A','B','C','D')]  <- sweep(stf@harvest[,FcY,c('A','B','C','D')],
                                                 c(3,6),res,"*")
  
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