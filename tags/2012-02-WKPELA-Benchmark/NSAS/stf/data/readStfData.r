#-------------------------------------------------------------------------------
#- TAC overfishing underfishing ratio's
#-------------------------------------------------------------------------------
OOF   <- read.csv(paste("./data/over_underfishing",DtY,".csv",sep=""),header=T); dims.OOF <- dim(OOF)

#-------------------------------------------------------------------------------
#- Partial Ns per fleet to partial Fs
#-------------------------------------------------------------------------------
Ns            <- read.csv(paste("./data/partial_ns",ac(an(ImY)-1),".csv",sep=""),header=T)
FA            <- Ns[,paste("A",DtY,sep="")]/apply(Ns,1,sum) * NSH@harvest[,DtY]
FB            <- Ns[,paste("B",DtY,sep="")]/apply(Ns,1,sum) * NSH@harvest[,DtY]
FC            <- Ns[,paste("C",DtY,sep="")]/apply(Ns,1,sum) * NSH@harvest[,DtY]
FD            <- Ns[,paste("D",DtY,sep="")]/apply(Ns,1,sum) * NSH@harvest[,DtY]

#-------------------------------------------------------------------------------
#- Partial Ws per fleet
#-------------------------------------------------------------------------------
Ws            <- read.csv(paste("./data/partial_ws",ac(an(ImY)-1),".csv",sep=""),header=T)
WA            <- apply(Ws[,paste("A",ac((an(DtY)-2):DtY),sep="")],1,mean,na.rm=T)
WB            <- apply(Ws[,paste("B",ac((an(DtY)-2):DtY),sep="")],1,mean,na.rm=T)
WC            <- apply(Ws[,paste("C",ac((an(DtY)-2):DtY),sep="")],1,mean,na.rm=T)
WD            <- apply(Ws[,paste("D",ac((an(DtY)-2):DtY),sep="")],1,mean,na.rm=T)

#-------------------------------------------------------------------------------
#- Calculate the harvest by fleet given a TAC for a stock
#-------------------------------------------------------------------------------
fleet.harvest <- function(stk,iYr,TACS){
                    nUnits                          <- dims(stk)$unit
                    #- prepare for iterations
                    nIter                           <- dims(TACS)$iter
                    res                             <- matrix(NA,ncol=nIter,nrow=nUnits,dimnames=list(units=dimnames(stk@stock.n)$unit,iter=1:nIter))
                    for(iTer in 1:nIter) res[,iTer] <- nls.lm(par=rep(1,nUnits),rescaleF,stk=iter(stk,iTer),iYr=iYr,TACS=c(iter(TACS,iTer)),nls.lm.control(ftol = (.Machine$double.eps)),jac=NULL)$par
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