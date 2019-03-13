#-------------------------------------------------------------------------------
#- Convert TACs from forecast to realised F by the fishing fleet
#-------------------------------------------------------------------------------

TAC2sel <- function(mult,
                    stk,
                    TAC,
                    iTer){
  
  Ns  <- iter(stk@stock.n,iTer)
  # apply scalor to selectivities
  Fs  <- sweep(iter(stk@harvest,iTer),3:6,mult,"*")
  Ms  <- iter(stk@m,iTer)
  Wts <- iter(stk@catch.wt,iTer)
  
  # calculate catches for the different fleets
  Z           <- drop(Ms[,,1]) + rowSums(Fs)
  catchNFleet <- drop(Fs)/Z*drop(Wts)*drop(Ns[,,1]) * (1-exp(-Z))
  Cs          <- colSums(catchNFleet)

  # setup catch targets
  Atarget <- iter(TAC[,,"A"],iTer)
  Btarget <- iter(TAC[,,"B"],iTer)
  Ctarget <- iter(TAC[,,"C"],iTer)
  Dtarget <- iter(TAC[,,"D"],iTer)

  resA <- sqrt(((Atarget-Cs[1])/Atarget)^2)
  resB <- sqrt(((Btarget-Cs[2])/Btarget)^2)
  resC <- sqrt(((Ctarget-Cs[3])/Ctarget)^2)
  resD <- sqrt(((Dtarget-Cs[4])/Dtarget)^2)
  
  ret <- c(resA,resB,resC,resD)
  
  return(ret)}