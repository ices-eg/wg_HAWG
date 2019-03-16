#-------------------------------------------------------------------------------
#- Fmsy: calculate A TAC under a given biomass value for the combination of the fleets
#-------------------------------------------------------------------------------
find.BC <- function(mult,
                    stk.=stk,
                    Btarget=Btarget,
                    CATCH=CATCH){
  
  mult                <- c(mult[1],mult[1],mult[2],mult[3])
  
  # scale F
  stk.@harvest        <- sweep(stk.@harvest,3,mult,"*")
  
  # calculate Z
  stkZ                <- unitSums(stk.@harvest) + stk.@m[,,1]
  
  # compute subsequent catches
  for(i in 1:dims(stk.)$unit){
    stk.@catch.n[,,i]         <- stk.@stock.n[,,i]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,i]))*(stk.@harvest[,,i]/(unitSums(stk.@harvest)+stk.@m[,,i]))
    stk.@catch[,,i]           <- computeCatch(stk.[,,i])
  }
  
  # compute SSB
  ssb                 <- quantSums(stk.@stock.n[,,1] * stk.@stock.wt[,,1] *
                                     exp(-unitSums(stk.@harvest)*stk.@harvest.spwn[,,1]-stk.@m[,,1] *
                                           stk.@m.spwn[,,1]) * stk.@mat[,,1])
  
  # compute residual between ftar and F26 (on total F)
  resA <- sqrt(((ssb-Btarget)/Btarget)^2)
  resC <- sqrt(((stk.@catch[,,'C']-CATCH[,,'C'])/CATCH[,,'C'])^2)
  resD <- sqrt(((stk.@catch[,,'D']-CATCH[,,'D'])/CATCH[,,'D'])^2)
  
  ret                 <- c(resA,resC,resD)
  
  return(ret)}