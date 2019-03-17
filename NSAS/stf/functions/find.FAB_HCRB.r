find.FAB_HCRB  <- function(mult,stk.=stk,f01.=f01,f26.=f26,TACS.=TACS,mpPoints.=mpPoints){
  
  f01 <- ac(0:1)
  f26 <- ac(2:6)
  
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
  
  if(ssb < mpPoints.$Btrigger & ssb > mpPoints.$Blim){
    Ftarget <- mpPoints.$Ftarget*ssb/mpPoints.$Btrigger
    F01Tar  <- 0.05#mpPoints.$F01*ssb/mpPoints.$Btrigger
  }
  if(ssb <= mpPoints.$Blim){
    Ftarget <- 0.1
    F01Tar  <- 0.04
  }
  if(ssb >= mpPoints.$Btrigger){
    Ftarget <- mpPoints.$Ftarget
    F01Tar  <- mpPoints.$F01
  }
  fbarB     <- mean(bigF[f01])
  fbarA     <- mean(bigF[f26])
  
  #print(Ftarget)
  #print(F01Tar)
  
  resA <- sqrt(((fbarA - Ftarget)/Ftarget)^2)
  resB <- sqrt(((fbarB - F01Tar)/F01Tar)^2)
  
  ret <- c(resA,resB)
  
  return(ret)}