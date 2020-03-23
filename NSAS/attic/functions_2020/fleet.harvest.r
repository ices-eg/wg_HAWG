#-------------------------------------------------------------------------------
#- Calculate the harvest by fleet given a TAC for a stock
#-------------------------------------------------------------------------------
fleet.harvest <- function(stk,
                          iYr,
                          CATCH){
  nUnits                          <- dims(stk)$unit
  #- prepare for iterations
  nIter                           <- dims(CATCH)$iter
  res                             <- matrix(NA,
                                            ncol=nIter,
                                            nrow=nUnits,
                                            dimnames=list(units=dimnames(stk@stock.n)$unit,
                                                          iter=1:nIter))
  for(iTer in 1:nIter) 
    res[,iTer] <- nls.lm(par=runif(nUnits),
                         lower=rep(1e-8,nUnits),
                         upper=NULL,
                         rescaleF,
                         stk=iter(stk,iTer),
                         iYr=iYr,
                         TACS=c(iter(CATCH,iTer)),
                         nls.lm.control(ftol = (.Machine$double.eps),
                                        maxiter = 1000),
                         jac=NULL)$par
  
  stk@harvest[,iYr]               <- sweep(stk@harvest[,iYr],c(3,6),res,"*")
  return(stk@harvest[,iYr])}