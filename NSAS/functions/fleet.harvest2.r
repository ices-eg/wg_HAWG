#-------------------------------------------------------------------------------
#- Calculate the harvest by fleet given a TAC for a stock
#-------------------------------------------------------------------------------
fleet.harvest2 <- function(stk,
                          iYr,
                          CATCH){
  nUnits                          <- dims(stk)$unit
  #- prepare for iterations
  nIter                           <- dims(CATCH)$iter
  res                             <- matrix(NA,
                                            ncol=nIter,
                                            nrow=3,
                                            dimnames=list(units=c('AB','C','D'),
                                                          iter=1:nIter))
  
  for(iTer in 1:nIter) 
    res[,iTer] <- nls.lm(par=rep(1,3),
                         lower=rep(1e-8,3),
                         upper=NULL,
                         rescaleF2,
                         stk=iter(stk,iTer),
                         iYr=iYr,
                         TACS=c(iter(CATCH,iTer)),
                         nls.lm.control(ftol = (.Machine$double.eps),
                                        maxiter = 1000),
                         jac=NULL)$par
  
  
  res <- c(res[1],res[1],res[2],res[3])
  stk@harvest[,iYr,c('A','B','C','D')]  <- sweep(stk@harvest[,iYr,c('A','B','C','D')],
                                             c(3,6),res,"*")
  
  #print(stf@harvest[,iYr,c('A','C','D')])
  #print(res)
  
  
  return(stk@harvest[,iYr])}