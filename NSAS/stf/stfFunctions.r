#-------------------------------------------------------------------------------
#- Calculate the harvest by fleet given a TAC for a stock
#-------------------------------------------------------------------------------
fleet.harvest <- function(stk,iYr,TACS){
                    nUnits                          <- dims(stk)$unit
                    #- prepare for iterations
                    nIter                           <- dims(TACS)$iter
                    res                             <- matrix(NA,ncol=nIter,nrow=nUnits,dimnames=list(units=dimnames(stk@stock.n)$unit,iter=1:nIter))
                    for(iTer in 1:nIter) res[,iTer] <- nls.lm(par=rep(1,nUnits),lower=NULL, upper=NULL,rescaleF,stk=iter(stk,iTer),iYr=iYr,TACS=c(iter(TACS,iTer)),nls.lm.control(ftol = (.Machine$double.eps)),jac=NULL)$par
                    stk@harvest[,iYr]               <- sweep(stk@harvest[,iYr],c(3,6),res,"*")
                 return(stk@harvest[,iYr])}

#-------------------------------------------------------------------------------
#- Function to scale the F pattern for stock
#-------------------------------------------------------------------------------
rescaleF      <- function(mult,stk.=stk,iYr.=iYr,TACS.=TACS){
                    stk.@harvest[,iYr.] <- sweep(stk.@harvest[,iYr.],3,mult,"*")
                    stkZ                <- unitSums(stk.@harvest[,iYr.]) + stk.@m[,iYr.,1]
                    res                 <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n[,iYr.] * stk.@catch.wt[,iYr.] * sweep(stk.@harvest[,iYr.],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))
                 return(res)}

#-------------------------------------------------------------------------------
#- Management plan: calculate A and B TAC
#-------------------------------------------------------------------------------
find.FAB      <- function(mult,stk.=stk,f01.=f01,f26.=f26,mp.options.=mp.options,TACS.=TACS){
                    stk.@harvest[,,1]  <- stk.@harvest[,,1]*mult[1]
                    stk.@harvest[,,3]  <- stk.@harvest[,,3]*mult[3] #there is a TAC constraint for this fleet
                    stk.@harvest[,,4]  <- stk.@harvest[,,4]*mult[4] #there is a TAC constraint for this fleet
                    if(mp.options. == "i"){ stk.@harvest[,,2]  <- stk.@harvest[,,2]*mult[2]}
                    if(mp.options. == "fro"){ stk.@harvest[,,2] <- stk.@harvest[,,2]}
                    bigF              <- apply(stk.@harvest,1,sum)
                    ssb               <- sum(stk.@stock.n[,,1]*stk.@stock.wt[,,1]*exp(-bigF*stk.@harvest.spwn[,,1]-stk.@m[,,1]*stk.@m.spwn[,,1])*stk.@mat[,,1])
                    if(mp.options. == "i"){
                      if(ssb < 0.8e6){
                        resA <- 0.1
                        resB <- 0.04
                      }
                      if(ssb >= 0.8e6 & ssb <= 1.5e6){
                        resA <- 0.15/0.7*((ssb-0.8e6)/1e6)+0.1
                        resB <- 0.05
                      }
                      if(ssb > 1.5e6){
                        resA <- 0.25
                        resB <- 0.05
                      }
                      catchC    <- sum(stk.@stock.n[,,"C"]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,"C"]))*(stk.@harvest[,,"C"]/(unitSums(stk.@harvest)+stk.@m[,,"C"]))*stk.@catch.wt[,,"C"])
                      catchD    <- sum(stk.@stock.n[,,"D"]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,"D"]))*(stk.@harvest[,,"D"]/(unitSums(stk.@harvest)+stk.@m[,,"D"]))*stk.@catch.wt[,,"D"])
                      fbarB <- mean(bigF[f01.,])
                      fbarA <- mean(bigF[f26.,])
                      ret   <- c(sqrt(c((fbarA-resA)^2,(fbarB-resB)^2,(catchC - c(TACS.)[3])^2,(catchD - c(TACS.)[4])^2)))
                    } else {
                        if(ssb < 0.8e6) resA <- 0.1
                        if(ssb >= 0.8e6 & ssb <= 1.5e6) resA <- 0.15/0.7*((ssb-0.8e6)/1e6)+0.1
                        if(ssb > 1.5e6) resA <- 0.25
                        fbarA <- mean(bigF[f26.,])
                        catchC    <- sum(stk.@stock.n[,,"C"]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,"C"]))*(stk.@harvest[,,"C"]/(unitSums(stk.@harvest)+stk.@m[,,"C"]))*stk.@catch.wt[,,"C"])
                        catchD    <- sum(stk.@stock.n[,,"D"]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,"D"]))*(stk.@harvest[,,"D"]/(unitSums(stk.@harvest)+stk.@m[,,"D"]))*stk.@catch.wt[,,"D"])
                        ret   <- c(sqrt(c((fbarA-resA)^2,0,(catchC - c(TACS.)[3])^2,(catchD - c(TACS.)[4])^2)))
                      }
                    return(ret)}

#-------------------------------------------------------------------------------
#- Management plan: calculate A and B TAC
#-------------------------------------------------------------------------------
find.newFAB   <- function(mult,stk.=stk,f01.=f01,f26.=f26,mp.options.=mp.options,TACS.=TACS){
                    stk.@harvest[,,1]  <- stk.@harvest[,,1]*mult[1]
                    stk.@harvest[,,3]  <- stk.@harvest[,,3]*mult[3] #there is a TAC constraint for this fleet
                    stk.@harvest[,,4]  <- stk.@harvest[,,4]*mult[4] #there is a TAC constraint for this fleet
                    if(mp.options. == "i"){ stk.@harvest[,,2]  <- stk.@harvest[,,2]*mult[2]}
                    if(mp.options. == "fro"){ stk.@harvest[,,2] <- stk.@harvest[,,2]}
                    bigF              <- apply(stk.@harvest,1,sum)
                    ssb               <- sum(stk.@stock.n[,,1]*stk.@stock.wt[,,1]*exp(-bigF*stk.@harvest.spwn[,,1]-stk.@m[,,1]*stk.@m.spwn[,,1])*stk.@mat[,,1])
                    if(mp.options. == "i"){
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
                      catchC    <- sum(stk.@stock.n[,,"C"]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,"C"]))*(stk.@harvest[,,"C"]/(unitSums(stk.@harvest)+stk.@m[,,"C"]))*stk.@catch.wt[,,"C"])
                      catchD    <- sum(stk.@stock.n[,,"D"]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,"D"]))*(stk.@harvest[,,"D"]/(unitSums(stk.@harvest)+stk.@m[,,"D"]))*stk.@catch.wt[,,"D"])
                      fbarB <- mean(bigF[f01.,])
                      fbarA <- mean(bigF[f26.,])
                      ret   <- c(sqrt(c((fbarA-resA)^2,(fbarB-resB)^2,(catchC - c(TACS.)[3])^2,(catchD - c(TACS.)[4])^2)))
                    } else {
                        if(ssb < 0.8e6) resA <- 0.1
                        if(ssb >= 0.8e6 & ssb <= 1.5e6) resA <- 0.15/0.7*((ssb-0.8e6)/1e6)+0.1
                        if(ssb > 1.5e6) resA <- 0.25
                        fbarA <- mean(bigF[f26.,])
                        catchC    <- sum(stk.@stock.n[,,"C"]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,"C"]))*(stk.@harvest[,,"C"]/(unitSums(stk.@harvest)+stk.@m[,,"C"]))*stk.@catch.wt[,,"C"])
                        catchD    <- sum(stk.@stock.n[,,"D"]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,"D"]))*(stk.@harvest[,,"D"]/(unitSums(stk.@harvest)+stk.@m[,,"D"]))*stk.@catch.wt[,,"D"])
                        ret   <- c(sqrt(c((fbarA-resA)^2,0,(catchC - c(TACS.)[3])^2,(catchD - c(TACS.)[4])^2)))
                      }
                    return(ret)}


#-------------------------------------------------------------------------------
#- Bpa: calculate A and B TAC
#-------------------------------------------------------------------------------
find.Bpa <- function(mult,stk.=stk,rec.=rec,bpa.=bpa,fpa.=fpa,f26.=f26,f01.=f01){
              stk.@harvest[,2,1]  <- stk.@harvest[,1,1] * mult[1]
              stk.@harvest[,2,2]  <- stk.@harvest[,1,2] * mult[2]

              bigF1               <- unitSums(stk.@harvest[,1])
              bigF2               <- unitSums(stk.@harvest[,2])
              stk.@stock.n[,2]    <- c(rec,(stk.@stock.n[,1,1]*exp(-bigF1-stk.@m[,1,1]))[ac(range(stk.)["min"]:(range(stk.)["max"]-2)),1],
                                     sum((stk.@stock.n[,1,1]*exp(-bigF1-stk.@m[,1,1]))[ac((range(stk.)["max"]-1):range(stk.)["max"]),1]))
              ssb                 <- sum(stk.@stock.n[,2,1]*stk.@stock.wt[,2,1]*stk.@mat[,2,1]*exp(-bigF2*stk.@harvest.spwn[,2,1]-stk.@m[,2,1]*stk.@m.spwn[,2,1]))
              fbar26              <- mean(bigF2[f26.,1])
              fbar01              <- mean(bigF2[f01.,1])
              return(ifelse(fbar26>0.25|fbar01>0.05,(fbar26-fpa.)^2+(fbar01-0.05)^2,(bpa.-ssb)^2))}

#-------------------------------------------------------------------------------
#- Fmsy: calculate A TAC under a given Fmsy value for the combination of the fleets
#-------------------------------------------------------------------------------
find.F <- function(mult,stk.=stk,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS){
                    stk.@harvest        <- sweep(stk.@harvest,3,mult,"*")
                    stkZ                <- unitSums(stk.@harvest) + stk.@m[,,1]
                    resA                <- sqrt(c(f. - quantMeans(unitSums(stk.@harvest[f26.,])))^2)*100
                    resBCD              <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n * stk.@catch.wt * sweep(stk.@harvest,c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))[2:4]
                    res                 <- c(resA,resBCD)
                 return(res)}


#-------------------------------------------------------------------------------
#- Calculate the catch by fleet
#-------------------------------------------------------------------------------
harvestCatch  <-  function(stk.,iYr){
                    stkZ      <- unitSums(stk.@harvest[,iYr]) + stk.@m[,iYr,1]
                    res       <- apply(sweep(stk.@stock.n[,iYr] * stk.@catch.wt[,iYr] * sweep(stk.@harvest[,iYr],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)
                  return(res)}

#-------------------------------------------------------------------------------
#- Set generic method for quantiles of iterations
#-------------------------------------------------------------------------------
	setGeneric("iterQuantile", function(x,probs=c(0.05,0.5,0.95), ...) standardGeneric("iterQuantile"))
setMethod('iterQuantile', signature(x='FLQuant'), function(x,probs=c(0.05,0.5,0.95), na.rm=TRUE) {
	return(apply(x, c(1:5), quantile, na.rm=na.rm,probs=c(0.05,0.5,0.95)))
})

#-------------------------------------------------------------------------------
#- Take SAM vcov to generate new realisations
#-------------------------------------------------------------------------------

monteCarloStock <- function(stck,sam,realisations){

  require(MASS)
  ctrl              <- sam@control

  #-Create new stock object with nr of realisations in iter slot
  mcstck            <- propagate(stck,iter=realisations)
  mcstck            <- window(mcstck,start=range(sam)["minyear"],end=range(sam)["maxyear"])
  mcstck@stock.n[]  <- NA
  mcstck@harvest[]  <- NA

  #-Generate new parameter realisations from vcov
  random.param      <- mvrnorm(realisations,sam@params$value,sam@vcov)

  #-Control settings
  n.states          <- length(unique(ctrl@states[names(which(ctrl@fleets==0)),]))
  yrs               <- dims(sam)$minyear:dims(sam)$maxyear
  ages              <- dims(sam)$age

  #-Extract the state variables
  u                 <- random.param[,which(colnames(random.param)=="U")]
  idxNs             <- c(mapply(seq,from=seq(1,ncol(u),ages+n.states),
                                    to  =seq(1,ncol(u),ages+n.states)+ages-1,
                                    by  =1))
  idxFs             <- c(mapply(seq,from=seq(1,ncol(u),ages+n.states)+ages,
                                    to  =seq(1,ncol(u),ages+n.states)+n.states+ages-1,
                                    by  =1))
  mcstck@stock.n[]  <- exp(aperm(array(u[,idxNs],dim=c(realisations,ages,    length(yrs))),perm=c(2,3,1)))
  mcstck@harvest[]  <- exp(aperm(array(u[,idxFs],dim=c(realisations,n.states,length(yrs))),perm=c(2,3,1)))[ctrl@states[names(which(NSH.ctrl@fleets==0)),],,]

  return(mcstck)}

#-------------------------------------------------------------------------------
#- Function to draw truncated lognormally distributed random values
#-------------------------------------------------------------------------------

rtlnorm <- function (n, mean = 0, sd = 1, lower = -Inf, upper = Inf)
{
   ret <- numeric()
   if (length(n) > 1)
       n <- length(n)
   while (length(ret) < n) {
       y <- rlnorm(n - length(ret), mean, sd)
       y <- y[y >= lower & y <= upper]
       ret <- c(ret, y)
   }
   stopifnot(length(ret) == n)
   ret
}

#-------------------------------------------------------------------------------
#- Calculate the harvest by fleet given a TAC for a stock
#-------------------------------------------------------------------------------
fleet.harvestF <- function(Fs,Ns,Wts,CWts,Ms,Tcs){
  res <- nls.lm(par=rep(1,ncol(Fs)),rescaleFF,Fs=Fs,Ns=Ns,Wts=Wts,CWts=CWts,Ms=Ms,Tcs=Tcs,jac=NULL)$par
  Fs  <- sweep(Fs,2,res,"*")
  return(Fs)}

#-------------------------------------------------------------------------------
#- Function to scale the F pattern for stock
#-------------------------------------------------------------------------------
rescaleFF      <- function(mult,Fs,Ns,Wts,CWts,Ms,catches){
  Fs  <- sweep(Fs,2,mult,"*")
  Z   <- rowSums(Fs) + Ms
  res <- sqrt(c(catches - colSums(sweep(Ns * CWts * sweep(Fs,1,Z,"/"),1,(1-exp(-rowSums(Fs)-Ms)),"*")))^2)
  return(sum(res,na.rm=T))}

#-------------------------------------------------------------------------------
#- Management plan: calculate A, B and C-fleet TAC
#-------------------------------------------------------------------------------
find.FABC <- function(mult,Fs,Ns,Wts,CWts,fspwns,mspwns,mats,Ms,f01,f26,Tcs,mixprop,WBSScatch){
  Fs                <- sweep(Fs,2,mult,"*")
  #calculate CATCH of NSAS (A + B) based on stk.@harvest
  catchABCD         <- numeric(length(mult))#; catchABCD[] <- NA
  for(i in 1:length(mult))
    catchABCD[i]    <- sum(Ns*(1-exp(-rowSums(Fs)-Ms))*(Fs[,i]/(rowSums(Fs)+Ms))*CWts[,i])
  CtargetC          <- (0.057*sum(catchABCD[1]) + 0.41*WBSScatch) * mixprop
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

#-------------------------------------------------------------------------------
#- Management plan: calculate A, B and C-fleet TAC based on Fs
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
#- Management plan: calculate all TACs of all fleets
#-------------------------------------------------------------------------------

find.FABCD <- function(mult,Fs,Ns,Wts,CWts,fspwns,mspwns,mats,Ms,f01,f26,Tcs,Tcsorig,mixprop,WBSScatch){
  print(mult)
  invokeFIAV <- FALSE
  invokeCatchIAV <- FALSE
  Fs                <- sweep(Fs,2,mult,"*")
  #calculate CATCH of NSAS (A + B) based on stk.@harvest
  catchABCD         <- numeric(length(mult))#; catchABCD[] <- NA
  for(i in 1:length(mult))
    catchABCD[i]    <- sum(Ns*(1-exp(-rowSums(Fs)-Ms))*(Fs[,i]/(rowSums(Fs)+Ms))*CWts[,i])
  CtargetC          <- (0.057*sum(catchABCD[1]) + 0.41*WBSScatch) * mixprop
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

  cat("Set initial targets\n")
  if((catchABCD[1] - Tcsorig[[1]])/Tcsorig[[1]] < -0.15){
    targetA <- Tcsorig[[1]] * 0.85
    invokeCatchIAV <- TRUE
  }
  if((catchABCD[1] - Tcsorig[[1]])/Tcsorig[[1]] > 0.15){
    targetA <- Tcsorig[[1]] * 1.15
    invokeCatchIAV <- TRUE
  }
  cat("Set TAC IAV targets\n")
  if(invokeCatchIAV){
    res       <- optim(par=mult,fn=rescaleFF,Fs=Fs,Ns=Ns,Wts=Wts,CWts=CWts,Ms=Ms,catches=c(targetA,catchABCD[2:4]),lower=rep(1e-4,length(mult)),method="L-BFGS-B")
    Fsnew     <- sweep(Fs,2,res$par,"*")
    bigFnew   <- rowSums(Fsnew)
    ssbnew    <- sum(Ns*Wts*exp(-bigFnew*fspwns-Ms*mspwns)*mats)


    if(ssbnew >= 0.8e6){
      targetA <- resA
      if(mean(bigFnew[f26])>(1.1*resA)){
        targetA <- 1.1*resA
        invokeFIAV <- TRUE
      }
      if(mean(bigFnew[f26])<(0.9*resA)){
        targetA <- 0.9*resA
        invokeFAIV <- TRUE
      }
    }
  }
  cat("Set F IAV targets\n")

  fbarA             <- mean(bigF[f26])
  fbarB             <- mean(bigF[f01])
  meanFsA           <- mean(fbarA,resA)
  meanFsB           <- mean(fbarB,resB)
  meanCC            <- mean(catchABCD[3],CtargetC)
  meanCD            <- mean(catchABCD[4],c(Tcs[,2,4]))
  if(invokeFIAV == FALSE & invokeCatchIAV == FALSE)
    ret               <- sqrt(c((fbarA - resA)/resA,(fbarB-resB)/resB,(catchABCD[3] - CtargetC)/CtargetC,(catchABCD[4] - c(Tcs[,2,4]))/c(Tcs[,2,4]))^2)
  if(invokeFIAV == FALSE & invokeCatchIAV == TRUE)
    ret               <- sqrt(c((catchABCD[1] - targetA)/targetA,(fbarB-resB)/resB,(catchABCD[3] - CtargetC)/CtargetC,(catchABCD[4] - c(Tcs[,2,4]))/Tcs[,2,4])^2)
  if(invokeFIAV == TRUE)
    ret               <- sqrt(c((fbarA - targetA)/targetA,(fbarB-resB)/resB,(catchABCD[3] - CtargetC)/CtargetC,(catchABCD[4] - c(Tcs[,2,4]))/Tcs[,2,4])^2)
  cat(c(fbarA,fbarB,catchABCD,ssb,resA,resB,CtargetC))
  cat("\n")
  return(sum(ret))}


#-------------------------------------------------------------------------------
#- Fmsy: calculate A TAC under a given Fmsy value for the combination of the fleets
#-------------------------------------------------------------------------------
find.FC <- function(mult,stk.=stk,f.=f.,f26.=f26,f01.=f01,TACS.=TACS,WBSScatch=WBSScatch,Csplit=Csplit){
                    stk.@harvest        <- sweep(stk.@harvest,3,mult,"*")
                    stkZ                <- unitSums(stk.@harvest) + stk.@m[,,1]
                    
                    for(i in 1:dims(stk.)$unit){
                      stk.@catch.n[,,i]         <- stk.@stock.n[,,i]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,i]))*(stk.@harvest[,,i]/(unitSums(stk.@harvest)+stk.@m[,,i]))
                      stk.@catch[,,i]           <- computeCatch(stk.[,,i])
                    }
                    resA                <- sqrt(c(f. - quantMeans(unitSums(stk.@harvest[f26.,])))^2)*100
                    resB                <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n * stk.@catch.wt * sweep(stk.@harvest,c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))[2]
                    resC                <- sqrt(c(stk.@catch[,,"C"] - (0.41 * WBSScatch + stk.@catch[,,"A"] * 0.057) * Csplit)^2)
                    resD                <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n * stk.@catch.wt * sweep(stk.@harvest,c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))[4]
                    res                 <- c(resA,resB,resC,resD)
                 return(res)}
                 
#-------------------------------------------------------------------------------
#- Fmsy: calculate A TAC under a given Fmsy value for the combination of the fleets
#-------------------------------------------------------------------------------
find.BC <- function(mult,stk.=stk,b.=b.,f26.=f26,f01.=f01,TACS.=TACS,WBSScatch=WBSScatch,Csplit=Csplit){
                    stk.@harvest        <- sweep(stk.@harvest,3,mult,"*")
                    stkZ                <- unitSums(stk.@harvest) + stk.@m[,,1]

                    for(i in 1:dims(stk.)$unit){
                      stk.@catch.n[,,i]         <- stk.@stock.n[,,i]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,i]))*(stk.@harvest[,,i]/(unitSums(stk.@harvest)+stk.@m[,,i]))
                      stk.@catch[,,i]           <- computeCatch(stk.[,,i])
                    }
                    ssb                 <- quantSums(stk.@stock.n[,,1] * stk.@stock.wt[,,1] *
                                                                      exp(-unitSums(stk.@harvest)*stk.@harvest.spwn[,,1]-stk.@m[,,1] *
                                                                      stk.@m.spwn[,,1]) * stk.@mat[,,1])
                    
                    resA                <- sqrt(c(b. - ssb)^2)
                    resB                <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n * stk.@catch.wt * sweep(stk.@harvest,c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))[2]
                    resC                <- sqrt(c(stk.@catch[,,"C"] - (0.41 * WBSScatch + stk.@catch[,,"A"] * 0.057) * Csplit)^2)
                    resD                <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n * stk.@catch.wt * sweep(stk.@harvest,c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))[4]
                    res                 <- c(resA,resB,resC,resD)
                 return(res)}