#-------------------------------------------------------------------------------
#- Calculate the harvest by fleet given a TAC for a stock
#-------------------------------------------------------------------------------
fleet.harvestF <- function(Fs,Ns,Wts,CWts,Ms,Tcs){
  res <- nls.lm(par=rep(1,ncol(Fs)),rescaleFF,Fs=Fs,Ns=Ns,Wts=Wts,CWts=CWts,Ms=Ms,Tcs=Tcs,jac=NULL)$par
  Fs  <- sweep(Fs,2,res,"*")
  return(Fs)}

rescaleFF      <- function(mult,Fs,Ns,Wts,CWts,Ms,Tcs){
  Fs  <- sweep(Fs,2,mult,"*")
  Z   <- rowSums(Fs) + Ms
  res <- sqrt(c(Tcs - colSums(sweep(Ns * CWts * sweep(Fs,1,Z,"/"),1,(1-exp(-rowSums(Fs)-Ms)),"*")))^2)
  return(res)}


find.FABC <- function(mult,Fs,Ns,Wts,CWts,fspwns,mspwns,mats,Ms,f01,f26,Tcs,mixprop){
  print(mult)
  Fs                <- sweep(Fs,2,mult,"*")
  #calculate CATCH of NSAS (A + B) based on stk.@harvest
  catchABCD         <- numeric(length(mult))#; catchABCD[] <- NA
  for(i in 1:length(mult))
    catchABCD[i]    <- sum(Ns*(1-exp(-rowSums(Fs)-Ms))*(Fs[,i]/(rowSums(Fs)+Ms))*CWts[,i])
  CtargetC          <- (0.057*sum(catchABCD[1]) + 0.41*advCatchWB) * mixprop
  bigF              <- rowSums(Fs)
  ssb               <- sum(Ns*Wts*exp(-bigF*fspwns-Ms*mspwns)*mats)
  if(ssb < 0.8e6){
    resA <- 0.1
    resB <- 0.04
  }
  if(ssb >= 0.8e6 & ssb <= 1.5e6){
    resA <- 0.16/0.7*((ssb-0.8e6)/1e6)+0.1
    resB <- 0.05
  }
  if(ssb > 1.5e6){
    resA <- 0.26
    resB <- 0.05
  }
  fbarA             <- mean(bigF[f26])
  fbarB             <- mean(bigF[f01])
  meanFsA           <- mean(fbarA,resA)
  meanFsB           <- mean(fbarB,resB)
  meanCC            <- mean(catchABCD[3],CtargetC)
  meanCD            <- mean(catchABCD[4],c(Tcs[,2,4]))
  ret               <- sqrt(c((fbarA - resA)/meanFsA,(fbarB-resB)/meanFsB,(catchABCD[3] - CtargetC)/meanCC,(catchABCD[4] - c(Tcs[,2,4]))/meanCD)^2)
  return(sum(ret))}


find.FABC_F <- function(mult,Fs,Ns,Wts,CWts,fspwns,mspwns,mats,Ms,f01,f26,Tcs,mixprop,FA,FB){
  #print(mult)
  Fs                <- sweep(Fs,2,mult,"*")
  #calculate CATCH of NSAS (A + B) based on stk.@harvest
  catchABCD         <- numeric(length(mult)) #; catchABCD[] <- NA
  for(i in 1:length(mult))
    catchABCD[i]    <- sum(Ns*(1-exp(-rowSums(Fs)-Ms))*(Fs[,i]/(rowSums(Fs)+Ms))*CWts[,i])
  CtargetC          <- (0.057*sum(catchABCD[1]) + 0.41*advCatchWB) * mixprop
  bigF              <- rowSums(Fs)
  fbarA             <- mean(bigF[f26])
  fbarB             <- mean(bigF[f01])
  meanFsA           <- mean(fbarA,FA)
  meanFsB           <- mean(fbarB,FB)
  meanCC            <- mean(catchABCD[3],CtargetC)
  meanCD            <- mean(catchABCD[4],c(Tcs[,2,4]))
  ret               <- sqrt(c((fbarA - FA)/meanFsA,(fbarB-FB)/meanFsB,(catchABCD[3] - CtargetC)/meanCC,(catchABCD[4] - c(Tcs[,2,4]))/meanCD)^2)
  return(sum(ret))}
