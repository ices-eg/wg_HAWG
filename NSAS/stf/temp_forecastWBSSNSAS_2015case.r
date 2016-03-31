#- Find F multipliers for fleet A,B,C and D, based on Ftarget for A,B and
#   moving Ctarget for C and fixed Ctarget for D

source(file=file.path("./data/temp_forecast_readStfData.r"))

iTer <- 1
mixprop <- Csplit #updated value (HAWG 2016) #NSAS proportion in C-fleet: 3-year average
advCatchWB <- 56793 #updated value for FcY(2017) #advised WBSS catch in the forecast year

#stf@stock.n[,FcY] <- stf@stock.n[,FcY]*3
res <- optim(par=rep(1,dims(stf)$unit),find.FABC,
                 Fs=stf@harvest[,FcY,,,,iTer,drop=T],
                 Ns=stf@stock.n[,FcY,1,,,iTer,drop=T],
                 Wts=stf@stock.wt[,FcY,1,,,iTer,drop=T],
                 CWts=stf@catch.wt[,FcY,,,,iTer,drop=T],
                 fspwns=stf@harvest.spwn[,FcY,1,,,iTer,drop=T],
                 mspwns=stf@m.spwn[,FcY,1,,,iTer,drop=T],
                 mats=stf@mat[,FcY,1,,,iTer,drop=T],
                 Ms=stf@m[,FcY,1,,,iTer,drop=T],f01=f01,f26=f26,
                 Tcs=iter(TACS[,c(ImY,FcY)],iTer),
                 mixprop=mixprop,
                 lower=rep(0.01,4),method="L-BFGS-B",control=list(maxit=100))$par

#- Update harvest accordingly
stf@harvest[,FcY]         <- sweep(stf@harvest[,FcY],c(3,6),res,"*")

#- Calculate catches based on new harvest pattern
for(i in dms$unit){
  stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
  stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
  stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
  stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
}

stf@catch[,FcY,"A"] #forecasted A-fleet TAC in FcY
#test if IAV cap on TAC needs to be applied
abs((stf@catch[,FcY,"A"] - TACS.orig[,ImY,"A"])/TACS.orig[,ImY,"A"]) > 0.15 #forecasted TAC is not smaller than ImY TAC-15%

#########
### ONLY if forecasted A-fleet TAC falls outside MP bounds, run the following loop!!! otherwise, continue at the end ###
#########

for(i in 1:20){

  #- Check if catches are more than 15% from the year before. And if so, adjust
  idxup                     <- which(stf@catch[,FcY,"A"] > TACS.orig[,ImY,"A"]*1.15)
  idxdown                   <- which(stf@catch[,FcY,"A"] < TACS.orig[,ImY,"A"]*0.85)
  stf@catch[,FcY,"A",,,idxup]   <- TACS.orig[,ImY,"A",,,idxup]   *1.15
  stf@catch[,FcY,"A",,,idxdown] <- TACS.orig[,ImY,"A",,,idxdown] *0.85
  #- If adjusted, recalculate harvest pattern
  stf@catch[,FcY,"C"]       <- (0.057 * sum(stf@catch[,FcY,c("A")]) + 0.41 * advCatchWB) *mixprop #combined C-fleet TAC

  stf@harvest[,FcY]         <- fleet.harvest(stk=stf,iYr=FcY,TACS=stf@catch[,FcY])

  #- Check the F target according to adjusted TAC
  ssb                       <- quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  resA <- resB              <- numeric(dims(stf)$iter)
  idx                       <- which(ssb < 0.8e6,arr.ind=T)
  if(length(idx)>0){
    resA[idx[,6]]           <- 0.1
    resB[idx[,6]]           <- 0.04
  }
  idx                       <- which(ssb >= 0.8e6 & ssb <= 1.5e6,arr.ind=T)
  if(length(idx)>0){
    resA[idx[,6]]           <- 0.16/0.7*((ssb[,,,,,idx]-0.8e6)/1e6)+0.1
    resB[idx[,6]]           <- 0.05
  }
  idx                       <- which(ssb > 1.5e6,arr.ind=T)
  if(length(idx)>0){
    resA[idx[,6]]           <- 0.26
    resB[idx[,6]]           <- 0.05
  }


  #-if Ftarget is off by more than 10%, make an adjustment
  outidx                    <- which(apply(unitSums(stf@harvest[f26,FcY]),2:6,mean,na.rm=T) <  0.9 * resA |
                                     apply(unitSums(stf@harvest[f26,FcY]),2:6,mean,na.rm=T) >  1.1 * resA)
  inidx                     <- which(apply(unitSums(stf@harvest[f26,FcY]),2:6,mean,na.rm=T) >= 0.9 * resA &
                                     apply(unitSums(stf@harvest[f26,FcY]),2:6,mean,na.rm=T) <= 1.1 * resA)
  #- If outside this range, do not apply TAC constraint and use 15% cap on F
  if(length(outidx)>0){
    sidx                    <- which((apply(unitSums(stf@harvest[f26,FcY,,,,outidx]),2:6,mean,na.rm=T)) <  0.9 * resA[outidx])
    bidx                    <- which((apply(unitSums(stf@harvest[f26,FcY,,,,outidx]),2:6,mean,na.rm=T)) >  1.1 * resA[outidx])

    if(length(sidx)>0) stf@harvest[,FcY,"A",,,outidx[sidx]]       <- sweep(stf@harvest[,FcY,"A",,,outidx[sidx]],2:6,
                                                                      ((resA[outidx[sidx]] * 0.9) - apply(unitSums(stf@harvest[f26,FcY,c("B","C","D"),,,outidx[sidx]]),2:6,mean,na.rm=T)) /
                                                                        apply(unitSums(stf@harvest[f26,FcY,"A",,,outidx[sidx]]),2:6,mean,na.rm=T),"*")
    if(length(bidx)>0) stf@harvest[,FcY,"A",,,outidx[bidx]]       <- sweep(stf@harvest[,FcY,"A",,,outidx[bidx]],2:6,
                                                                      ((resA[outidx[bidx]] * 1.1) - apply(unitSums(stf@harvest[f26,FcY,c("B","C","D"),,,outidx[bidx]]),2:6,mean,na.rm=T)) /
                                                                        apply(unitSums(stf@harvest[f26,FcY,"A",,,outidx[bidx]]),2:6,mean,na.rm=T),"*")
  }
  #-if Ftarget is off by more than 10%, make an adjustment
  outidx                    <- which(apply(unitSums(stf@harvest[f01,FcY]),2:6,mean,na.rm=T) <  resB |
                                     apply(unitSums(stf@harvest[f01,FcY]),2:6,mean,na.rm=T) >  resB)
  inidx                     <- which(apply(unitSums(stf@harvest[f01,FcY]),2:6,mean,na.rm=T) >= resB &
                                     apply(unitSums(stf@harvest[f01,FcY]),2:6,mean,na.rm=T) <= resB)
  #- If outside this range, do not apply TAC constraint and use 15% cap on F
  if(length(outidx)>0){
    sidx                    <- which((apply(unitSums(stf@harvest[f01,FcY,,,,outidx]),2:6,mean,na.rm=T)) <  resB[outidx])
    bidx                    <- which((apply(unitSums(stf@harvest[f01,FcY,,,,outidx]),2:6,mean,na.rm=T)) >  resB[outidx])

    if(length(sidx)>0) stf@harvest[,FcY,"B",,,outidx[sidx]]       <- sweep(stf@harvest[,FcY,"B",,,outidx[sidx]],2:6,
                                                                      ((resB[outidx[sidx]]) - apply(unitSums(stf@harvest[f01,FcY,c("A","C","D"),,,outidx[sidx]]),2:6,mean,na.rm=T)) /
                                                                        apply(unitSums(stf@harvest[f01,FcY,"B",,,outidx[sidx]]),2:6,mean,na.rm=T),"*")
    if(length(bidx)>0) stf@harvest[,FcY,"B",,,outidx[bidx]]       <- sweep(stf@harvest[,FcY,"B",,,outidx[bidx]],2:6,
                                                                      ((resB[outidx[bidx]]) - apply(unitSums(stf@harvest[f01,FcY,c("A","C","D"),,,outidx[bidx]]),2:6,mean,na.rm=T)) /
                                                                        apply(unitSums(stf@harvest[f01,FcY,"B",,,outidx[bidx]]),2:6,mean,na.rm=T),"*")
  }
  #- Now find the Fs and catches that
  stf@catch[,FcY,"B"]       <- quantSums(stf@catch.wt[,FcY,"B",,,outidx] * stf@stock.n[,FcY,"B",,,outidx] *
                                        (1-exp(-unitSums(stf@harvest[,FcY,,,,outidx])-stf@m[,FcY,"B",,,outidx])) *
                                        (stf@harvest[,FcY,"B",,,outidx]/(unitSums(stf@harvest[,FcY,,,,outidx])+stf@m[,FcY,"B",,,outidx])))

  stf@catch[,FcY,"C"]       <- 0.057*unitSums(stf@catch[,FcY,c("A")])*mixprop + TACS[,FcY,"C"]

   res <- optim(par=rep(1,dims(stf)$unit),find.FABC_F,
                    Fs=stf@harvest[,FcY,,,,iTer,drop=T],
                    Ns=stf@stock.n[,FcY,1,,,iTer,drop=T],
                    Wts=stf@stock.wt[,FcY,1,,,iTer,drop=T],
                    CWts=stf@catch.wt[,FcY,,,,iTer,drop=T],
                    fspwns=stf@harvest.spwn[,FcY,1,,,iTer,drop=T],
                    mspwns=stf@m.spwn[,FcY,1,,,iTer,drop=T],
                    mats=stf@mat[,FcY,1,,,iTer,drop=T],
                    Ms=stf@m[,FcY,1,,,iTer,drop=T],f01=f01,f26=f26,
                    Tcs=iter(TACS[,c(ImY,FcY)],iTer),
                    mixprop=mixprop,
                    FA=resA*0.9,
                    FB=resB,
                    lower=rep(0.01,4),method="L-BFGS-B",control=list(maxit=100))$par

   stf@harvest[,FcY]         <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
   stf@catch[,FcY,"A"]       <- quantSums(stf@catch.wt[,FcY,"A",,,outidx] * stf@stock.n[,FcY,"A",,,outidx] *
                                         (1-exp(-unitSums(stf@harvest[,FcY,,,,outidx])-stf@m[,FcY,"A",,,outidx])) *
                                         (stf@harvest[,FcY,"A",,,outidx]/(unitSums(stf@harvest[,FcY,,,,outidx])+stf@m[,FcY,"A",,,outidx])))
   stf@catch[,FcY,"B"]       <- quantSums(stf@catch.wt[,FcY,"B",,,outidx] * stf@stock.n[,FcY,"B",,,outidx] *
                                         (1-exp(-unitSums(stf@harvest[,FcY,,,,outidx])-stf@m[,FcY,"B",,,outidx])) *
                                         (stf@harvest[,FcY,"B",,,outidx]/(unitSums(stf@harvest[,FcY,,,,outidx])+stf@m[,FcY,"B",,,outidx])))
   stf@catch[,FcY,"C"]       <- (0.057*unitSums(stf@catch[,FcY,c("A")]) + 0.41*advCatchWB) * mixprop



   print(c(apply(unitSums(stf@harvest[f26,FcY]),2:6,mean,na.rm=T),apply(unitSums(stf@harvest[f01,FcY]),2:6,mean,na.rm=T),stf@catch[,FcY,],(0.057*unitSums(stf@catch[,FcY,c("A")]) + 0.41*advCatchWB )* mixprop - stf@catch[,FcY,"C"]))
   stf@harvest[,FcY]         <- fleet.harvest(stk=stf,iYr=FcY,TACS=stf@catch[,FcY])
}

TAC.C <- 0.057 * sum(stf@catch[,FcY,c("A")]) + 0.41 * advCatchWB  #combined C-fleet TAC
TAC.C10 <- TAC.C *0.1 #10% thereof, add to A-fleet as the minimum transfer amount

stf@harvest[ac(2:6),,"A"] #fleet A f2-6 before 10% transfer
sum(stf@catch[,FcY,]) #total forecasted NSAS outtake before transfer business

### fill in NSAS management option in table
stf.table["mp","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
stf.table["mp",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
stf.table["mp","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
stf.table["mp","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
stf.table["mp",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
stf.table["mp",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                                           exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                                           stf@mat[,FcY,1]))
### different transfer options (e.g. 10%)
# 10% C-fleet transfer to the North Sea (C ---> A)
stf@catch[,FcY,"A"] <- stf@catch[,FcY,"A"] + TAC.C10 #- 2953 #add 10% C-fleet TAC to A-fleet (and don't transfer WBSS proportion in A-fleet back to C-fleet!!)
stf@catch[,FcY,"C"] <- stf@catch[,FcY,"C"] - (TAC.C10 * mixprop) #remove 10% from the C-fleet (this time, only NSAS using mixing proportion)
sum(stf@catch[,FcY,]) #new total NSAS outtake
stf@harvest[,FcY]         <- fleet.harvest(stk=stf,iYr=FcY,TACS=stf@catch[,FcY]) # recalculate new F's for all fleets after transferring
stf@harvest[ac(2:6),,"A"] #new partial F for the A-fleet

apply(unitSums(stf@harvest[f26,FcY]),2:6,mean,na.rm=T) # new F-bar (F2-6) of all fleets for NSAS

##fill stf.table
stf.table["mp","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
stf.table["mp",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
stf.table["mp","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
stf.table["mp","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
stf.table["mp",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
stf.table["mp",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                                           exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                                           stf@mat[,FcY,1]))
#plot SSB @ age in FcY
#plot(c(0:8),as.numeric(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] * exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1])* stf@mat[,FcY,1]),ylab="biomass [tonnes]",xlab="age",main="NSAS SSB at age in 2016",type="l",lwd=2,col="red")

# #-------------------------------------------------------------------------------
# #- Calculate the harvest by fleet given a TAC for a stock
# #-------------------------------------------------------------------------------
# fleet.harvestF <- function(Fs,Ns,Wts,CWts,Ms,Tcs){
#                     res <- nls.lm(par=rep(1,ncol(Fs)),rescaleFF,Fs=Fs,Ns=Ns,Wts=Wts,CWts=CWts,Ms=Ms,Tcs=Tcs,jac=NULL)$par
#                     Fs  <- sweep(Fs,2,res,"*")
#                     return(Fs)}
#                     
# rescaleFF      <- function(mult,Fs,Ns,Wts,CWts,Ms,Tcs){
#                     Fs  <- sweep(Fs,2,mult,"*")
#                     Z   <- rowSums(Fs) + Ms
#                     res <- sqrt(c(Tcs - colSums(sweep(Ns * CWts * sweep(Fs,1,Z,"/"),1,(1-exp(-rowSums(Fs)-Ms)),"*")))^2)
#                   return(res)}
# 
# 
# find.FABC <- function(mult,Fs,Ns,Wts,CWts,fspwns,mspwns,mats,Ms,f01,f26,Tcs,mixprop){
#                     print(mult)
#                     Fs                <- sweep(Fs,2,mult,"*")
#                     #calculate CATCH of NSAS (A + B) based on stk.@harvest
#                     catchABCD         <- numeric(length(mult))#; catchABCD[] <- NA
#                     for(i in 1:length(mult))
#                       catchABCD[i]    <- sum(Ns*(1-exp(-rowSums(Fs)-Ms))*(Fs[,i]/(rowSums(Fs)+Ms))*CWts[,i])
#                     CtargetC          <- (0.057*sum(catchABCD[1:2]) + 0.41*advCatchWB) * mixprop
#                     bigF              <- rowSums(Fs)
#                     ssb               <- sum(Ns*Wts*exp(-bigF*fspwns-Ms*mspwns)*mats)
#                     if(ssb < 0.8e6){
#                       resA <- 0.1
#                       resB <- 0.04
#                     }
#                     if(ssb >= 0.8e6 & ssb <= 1.5e6){
#                       resA <- 0.16/0.7*((ssb-0.8e6)/1e6)+0.1
#                       resB <- 0.05
#                     }
#                     if(ssb > 1.5e6){
#                       resA <- 0.26
#                       resB <- 0.05
#                     }
#                     fbarA             <- mean(bigF[f26])
#                     fbarB             <- mean(bigF[f01])
#                     meanFsA           <- mean(fbarA,resA)
#                     meanFsB           <- mean(fbarB,resB)
#                     meanCC            <- mean(catchABCD[3],CtargetC)
#                     meanCD            <- mean(catchABCD[4],c(Tcs[,2,4]))
#                     ret               <- sqrt(c((fbarA - resA)/meanFsA,(fbarB-resB)/meanFsB,(catchABCD[3] - CtargetC)/meanCC,(catchABCD[4] - c(Tcs[,2,4]))/meanCD)^2)
#              return(sum(ret))}
#              
#              
# find.FABC_F <- function(mult,Fs,Ns,Wts,CWts,fspwns,mspwns,mats,Ms,f01,f26,Tcs,mixprop,FA,FB){
#                     #print(mult)
#                     Fs                <- sweep(Fs,2,mult,"*")
#                     #calculate CATCH of NSAS (A + B) based on stk.@harvest
#                     catchABCD         <- numeric(length(mult)) #; catchABCD[] <- NA
#                     for(i in 1:length(mult))
#                       catchABCD[i]    <- sum(Ns*(1-exp(-rowSums(Fs)-Ms))*(Fs[,i]/(rowSums(Fs)+Ms))*CWts[,i])
#                     CtargetC          <- (0.057*sum(catchABCD[1:2]) + 0.41*advCatchWB) * mixprop
#                     bigF              <- rowSums(Fs)
#                     fbarA             <- mean(bigF[f26])
#                     fbarB             <- mean(bigF[f01])
#                     meanFsA           <- mean(fbarA,FA)
#                     meanFsB           <- mean(fbarB,FB)
#                     meanCC            <- mean(catchABCD[3],CtargetC)
#                     meanCD            <- mean(catchABCD[4],c(Tcs[,2,4]))
#                     ret               <- sqrt(c((fbarA - FA)/meanFsA,(fbarB-FB)/meanFsB,(catchABCD[3] - CtargetC)/meanCC,(catchABCD[4] - c(Tcs[,2,4]))/meanCD)^2)
#              return(sum(ret))}