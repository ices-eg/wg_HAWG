#-------------------------------------------------------------------------------
#- Function to scale the F pattern for stock
#-------------------------------------------------------------------------------
rescaleF      <- function(mult,stk.=stk,iYr.=iYr,TACS.=TACS){
  stk.@harvest[,iYr.] <- sweep(stk.@harvest[,iYr.],3,mult,"*")
  stkZ                <- unitSums(stk.@harvest[,iYr.]) + stk.@m[,iYr.,1]
  ctch                <- c(apply(sweep(stk.@stock.n[,iYr.] * stk.@catch.wt[,iYr.] * sweep(stk.@harvest[,iYr.],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T))

  res                 <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n[,iYr.] * stk.@catch.wt[,iYr.] * sweep(stk.@harvest[,iYr.],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))
  return(res)}