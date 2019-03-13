#-------------------------------------------------------------------------------
#- Calculate the harvest by fleet given a TAC for a stock
#-------------------------------------------------------------------------------
fleet.harvest <- function(stk,iYr,TACS){
  nUnits                          <- dims(stk)$unit
  #- prepare for iterations
  nIter                           <- dims(TACS)$iter
  res                             <- matrix(NA,ncol=nIter,nrow=nUnits,dimnames=list(units=dimnames(stk@stock.n)$unit,iter=1:nIter))
  for(iTer in 1:nIter) res[,iTer] <- nls.lm(par=runif(nUnits),lower=rep(1e-8,nUnits), upper=NULL,rescaleF,stk=iter(stk,iTer),iYr=iYr,TACS=c(iter(TACS,iTer)),nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000),jac=NULL)$par
  #optim(par=runif(nUnits),rescaleF,lower=rep(1e-8,nUnits),stk=iter(stk,iTer),iYr=iYr,TACS=c(iter(TACS,iTer)),method="L-BFGS-B",control=list(maxit=1000))$par
  stk@harvest[,iYr]               <- sweep(stk@harvest[,iYr],c(3,6),res,"*")
  return(stk@harvest[,iYr])}