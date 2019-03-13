#-------------------------------------------------------------------------------
#- Function to scale the F pattern for stock
#-------------------------------------------------------------------------------
rescaleF      <- function(mult,stk.=stk,iYr.=iYr,TACS.=TACS){
  stk.@harvest[,iYr.] <- sweep(stk.@harvest[,iYr.],3,mult,"*")
  stkZ                <- unitSums(stk.@harvest[,iYr.]) + stk.@m[,iYr.,1]
  ctch                <- c(apply(sweep(stk.@stock.n[,iYr.] * stk.@catch.wt[,iYr.] * sweep(stk.@harvest[,iYr.],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T))
  
  #res                 <- -1*c(dnorm(log(c(TACS.)[1]),log(ctch[1]),sd=0.15,log=T),
  #                                                dnorm(log(c(TACS.)[2]),log(ctch[2]),sd=0.3,log=T),
  #                                                dnorm(log(c(TACS.)[3]),log(ctch[3]),sd=0.15,log=T),
  #                                                dnorm(log(c(TACS.)[4]),log(ctch[4]),sd=0.3,log=T))
  #                    print(res)
  res                 <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n[,iYr.] * stk.@catch.wt[,iYr.] * sweep(stk.@harvest[,iYr.],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))
  return(res)}