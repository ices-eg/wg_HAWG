find.FC <- function(mult,
                    stk.=stk,
                    F26tar=f.,
                    CATCH=CATCH,
                    f26=f26,
                    f01=f01){
  
  #f26 <- ac(2:6)
  #f01 <- ac(0:1)
  
  #mult                <- c(mult[1],mult[1],mult[2],mult[3])
  
  # scale F
  stk.@harvest        <- sweep(stk.@harvest,3,mult,"*")
  # calculate Z
  stkZ                <- unitSums(stk.@harvest) + stk.@m[,,1]
  
  # compute subsequent catches
  for(i in 1:dims(stk.)$unit){
    stk.@catch.n[,,i]         <- stk.@stock.n[,,i]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,i]))*(stk.@harvest[,,i]/(unitSums(stk.@harvest)+stk.@m[,,i]))
    stk.@catch[,,i]           <- computeCatch(stk.[,,i])
  }
  
  # compute F26
  F26_bar <- quantMeans(unitSums(stk.@harvest[f26,]))
  
  
  # compute residual between ftar and F26 (on total F)
  resA <- sqrt(((F26_bar-F26tar)/F26tar)^2)
  #resB <- sqrt(((stk.@catch[,,'B']-CATCH[,,'B'])/CATCH[,,'B'])^2)
  resB <- sqrt(((stk.@catch[,,'B']-CATCH[,,'B'])/CATCH[,,'B'])^2)
  resC <- sqrt(((stk.@catch[,,'C']-CATCH[,,'C'])/CATCH[,,'C'])^2)
  resD <- sqrt(((stk.@catch[,,'D']-CATCH[,,'D'])/CATCH[,,'D'])^2)
  
  ret <- c(resA,resB,resC,resD)
  
  return(ret)}