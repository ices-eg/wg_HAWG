# load appropriate functions
source(file.path(functionPath,"find.FAB_HCRA.r"))
source(file.path(functionPath,"find.FAB_HCRB.r"))
source(file.path(functionPath,"TAC2sel.r"))

TACTemp <-  TACS.orig

# reset F
stf@harvest[,FcY] <- stf@harvest[,ImY]

res <- matrix(NA,
              nrow=2,
              ncol=dims(stf)$iter,
              dimnames=list(c("A","B"),
                            dimnames(stf@stock.n)$iter))

# if case for both HCR - the function to optimize is different
if(managementRule$HCR == "A"){
  for(iTer in 1:dims(stf)$iter){
    res[,iTer]              <- nls.lm(par=rep(1,2),
                                      find.FAB_HCRA,
                                      stk=stf[,FcY,c("A","B"),,,iTer],
                                      mpPoints=referencePoints,
                                      jac=NULL,
                                      lower=rep(1e-15,2),
                                      upper=NULL,
                                      control=nls.lm.control(maxiter=1000))$par
  }
}
if(managementRule$HCR == "B"){
  for(iTer in 1:dims(stf)$iter){
    res[,iTer]              <- nls.lm(par=rep(1,2),
                                      find.FAB_HCRB,
                                      stk=stf[,FcY,c("A","B"),,,iTer],
                                      mpPoints=referencePoints,
                                      jac=NULL,
                                      lower=rep(1e-15,2),
                                      upper=NULL,
                                      control=nls.lm.control(maxiter=1000))$par
  }
}


# resulting F for A and B fleets
stf@harvest[,FcY,c("A","B")]         <- sweep(stf@harvest[,FcY,c("A","B")],c(3,6),res,"*")

#- Calculate TACs in FcY
# Note: I don't understand why we use all fleets when calculating the catches and only fleet A and B when calculating the SSB
for(i in dms$unit){
  stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
  stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
  stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
  stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
}
TACS.orig[,FcY,c("A","B")]   <- stf@catch[,FcY,c("A","B")]

# calculate alternative total F and SSB
totF                      <- unitSums(stf@harvest[,FcY,c("A","B")])
SSBHCR                    <- quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] * exp(-totF*stf@harvest.spwn[,FcY,1] - stf@m[,FcY,1]) * stf@mat[,FcY,1])

# IAV
if(is.null(managementRule$TACIAV) == F){
  mrF                             <- managementRule$TACIAV
  
  idx                             <- which(SSBHCR > referencePoints$Btrigger)
  for(imrF in mrF){
    bidx                                    <- which(iter(TACS.orig[,FcY,imrF],idx) > (1.25 * iter(TACS.orig[,ImY,imrF],idx)))
    iter(TACS.orig[,FcY,imrF],idx[bidx])    <- 1.25 * iter(TACS.orig[,ImY,imrF,],idx[bidx])
    sidx                                    <- which(iter(TACS.orig[,FcY,imrF],idx) < (0.8 * iter(TACS.orig[,ImY,imrF],idx)))
    iter(TACS.orig[,FcY,imrF],idx[sidx])    <- 0.8 * iter(TACS.orig[,ImY,imrF,],idx[sidx])
  }
}

# realised catch for the forecast year
# Note 1: assuming same Ctransfer as for the ImY
# Note 2: assuming same B uptake as for the ImY
TACS[,FcY,"A"]        <- TACS.orig[,FcY,'A'] + Ctransfer * TACS.orig[,FcY,'C']
TACS[,FcY,'B']        <- TACS.orig[,FcY,'B'] * Buptake

# calculate realised F by the fishing fleet
res <- matrix(NA,
              nrow=4,
              ncol=dims(stf)$iter,
              dimnames=list(c("A","B",'C','D'),
                            dimnames(stf@stock.n)$iter))

for(iTer in 1:dims(stf)$iter){
  res[,iTer] <- nls.lm(par=rep(1,4),
                       TAC2sel,
                       stk=stf[,FcY],
                       TAC=TACS[,FcY],
                       iTer=iTer,
                       jac=NULL,
                       lower=rep(1e-15,4),
                       upper=NULL,
                       control=nls.lm.control(maxiter=1000))$par
}

# calculate true F and true SSB for forecast year
stf@harvest[,FcY] <- sweep(stf@harvest[,FcY,],c(3,6),res,"*")
ssb.FcY <- quantSums( stf@stock.n[,FcY,1] * 
                        stf@stock.wt[,FcY,1] *
                        stf@mat[,FcY,1] *
                        exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))

#  Update to continuation year
stf@harvest[,CtY]                                           <- stf@harvest[,FcY]
for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
ssb.CtY                                                     <- quantSums( stf@stock.n[,CtY,1] * 
                                                                            stf@stock.wt[,CtY,1]*
                                                                            stf@mat[,CtY,1]*
                                                                            exp(-unitSums(stf@harvest[,CtY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

TACS.orig <- TACTemp