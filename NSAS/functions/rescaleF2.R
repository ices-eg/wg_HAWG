 #-------------------------------------------------------------------------------
#- Function to scale the F pattern for stock
#-------------------------------------------------------------------------------
rescaleF      <- function(mult,
                               stk.=stk,
                               iYr.=iYr,
                               TACS.=TACS){
  
  mult                <- c(mult[1],mult[1],mult[2],mult[3])
  
  stk.@harvest[,,c('A','B','C','D')]        <- sweep(stk.@harvest[,,c('A','B','C','D')],3,mult,"*")
  stkZ                <- unitSums(stk.@harvest[,iYr.]) + stk.@m[,iYr.,1]
  ctch                <- c(apply(sweep(stk.@stock.n[,iYr.] * stk.@catch.wt[,iYr.] * sweep(stk.@harvest[,iYr.],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T))
  
  resA <- sqrt(((ctch[1]-TACS.[1])/TACS.[1])^2)
  resC <- sqrt(((ctch[3]-TACS.[3])/TACS.[3])^2)
  resD <- sqrt(((ctch[4]-TACS.[4])/TACS.[4])^2)
  #resB <- sqrt(((stk.@catch[,,'B']-CATCH[,,'B'])/CATCH[,,'B'])^2)
  #resC <- sqrt(((stk.@catch[,,'C']-CATCH[,,'C'])/CATCH[,,'C'])^2)
  #resD <- sqrt(((stk.@catch[,,'D']-CATCH[,,'D'])/CATCH[,,'D'])^2)
  #TACS.[,,'C']
  #res                 <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n[,iYr.] * stk.@catch.wt[,iYr.] * sweep(stk.@harvest[,iYr.],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))
  #res                 <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n[,iYr.] * stk.@catch.wt[,iYr.] * sweep(stk.@harvest[,iYr.],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))
  res <- c(resA,resC,resD)
  return(res)}