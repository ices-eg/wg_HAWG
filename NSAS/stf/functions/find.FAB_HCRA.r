#-------------------------------------------------------------------------------
#- compute difference between Ftar and F01 and selectivities with scalor
#-------------------------------------------------------------------------------
find.FAB_HCRA  <- function(mult,stk.=stk,f01.=f01,f26.=f26,mpPoints.=mpPoints){
  Fs      <- sweep(stk.@harvest@.Data,3,mult,"*")
  Ns      <- stk.@stock.n@.Data[,,1,,,]
  Ms      <- stk.@m@.Data[,,1,,,]
  Hspwns  <- stk.@harvest.spwn@.Data[,,1,,,]
  Mspwns  <- stk.@m.spwn@.Data[,,1,,,]
  Mats    <- stk.@mat@.Data[,,1,,,]
  Swghts  <- stk.@stock.wt@.Data[,,1,,,]
  Cwghts  <- stk.@catch.wt@.Data
  
  bigF              <- apply(Fs,1,sum)
  ssb               <- sum(Ns * Swghts * exp(-bigF*Hspwns - Ms*Mspwns) * Mats)
  if(ssb <= mpPoints.$Btrigger){
    resA <- mpPoints.$Ftarget*ssb/mpPoints.$Btrigger
    resB <- mpPoints.$F01*ssb/mpPoints.$Btrigger
  }
  if(ssb > mpPoints.$Btrigger){
    resA <- mpPoints.$Ftarget
    resB <- mpPoints.$F01
  }
  fbarB     <- mean(bigF[f01.])
  fbarA     <- mean(bigF[f26.])
  ret       <- -1*c(dnorm(log(fbarA),log(resA),log=T),
                    dnorm(log(fbarB),log(resB),log=T))
  
  return(ret)}