#-------------------------------------------------------------------------------
#- Fmsy: calculate A TAC under a given Fmsy Advice Rule value for the 
# combination of the fleets
#
# Fmsy Advise rule: use MP HCR A with MSYBtrigger and Fmsy.
#
# The function Computes differences between TAC and catches computed with 
# given F scalors
#
# Note: Only Fmsy is used here corresponding to F26. There is therefore no F01
# constraint. One only calculate scalors for fleets AB, C and D.
#-------------------------------------------------------------------------------
find.FCAR <- function(mult,
                      stk.=stk,
                      CATCH=CATCH,
                      refs.,
                      f26=f26,
                      f01=f01){
  
  #f26 <- ac(2:6)
  
  # same multiplier for A and B fleet
  #mult                <- c(mult[1],mult[1],mult[2],mult[3])
  
  # compute harvest
  stk.@harvest[,,c('A','C','D')]        <- sweep(stk.@harvest[,,c('A','C','D')],3,mult,"*")
  
  # compute total mortality
  stkZ                <- unitSums(stk.@harvest) + stk.@m[,,1]
  
  # compute corresponding catches
  for(i in 1:dims(stk.)$unit){
    stk.@catch.n[,,i]         <- stk.@stock.n[,,i]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,i]))*(stk.@harvest[,,i]/(unitSums(stk.@harvest)+stk.@m[,,i]))
    stk.@catch[,,i]           <- computeCatch(stk.[,,i])
  }
  
  # compute SSB
  ssb                 <- quantSums(  stk.@stock.n[,,1] * stk.@stock.wt[,,1] *
                                     exp(-unitSums(stk.@harvest)*stk.@harvest.spwn[,,1]-stk.@m[,,1] *
                                      stk.@m.spwn[,,1]) * stk.@mat[,,1])
  
  # compute F based on HCR A, downward slope after MSYBtrigger
  if(ssb<refs.$MSYBtrigger){
    F26tar <- ssb/refs.$MSYBtrigger * refs.$Fmsy
  }else{
    F26tar <- refs.$Fmsy
  }

  F26_bar <- quantMeans(unitSums(stk.@harvest[f26,]))
  
  # compute residual between ftar and F26 (on total F)
  resA <- sqrt(((F26_bar-F26tar)/F26tar)^2)
  #resB <- sqrt(((stk.@catch[,,'B']-CATCH[,,'B'])/CATCH[,,'B'])^2)
  resC <- sqrt(((stk.@catch[,,'C']-CATCH[,,'C'])/CATCH[,,'C'])^2)
  resD <- sqrt(((stk.@catch[,,'D']-CATCH[,,'D'])/CATCH[,,'D'])^2)

  res                 <- c(resA,resC,resD)
  return(res)}