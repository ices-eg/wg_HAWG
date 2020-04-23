#-------------------------------------------------------------------------------
#- Calculate the catch by fleet
#-------------------------------------------------------------------------------
harvestCatch  <-  function(stk.,iYr){
  stkZ      <- unitSums(stk.@harvest[,iYr]) + stk.@m[,iYr,1]
  res       <- apply(sweep(stk.@stock.n[,iYr] * stk.@catch.wt[,iYr] * sweep(stk.@harvest[,iYr],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)
  return(res)}
