# load appropriate function
source(file.path(functionPath,"find.FCAR.r"))

#reset FcY harvest for all fleets
stf@harvest[,FcY]   <- stf@harvest[,ImY]

# optimize F scalors against FA and true TAC C and D
res <- matrix(NA,
              nrow=3,
              ncol=dims(stf)$iter,
              dimnames=list(dimnames(stf@stock.n)$unit[c(1,3,4)],
                            dimnames(stf@stock.n)$iter))

for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
  res[,iTer]                  <- nls.lm(par=rep(1,3), # A scalor = B scalor, therefore 3 scalors
                                        lower=rep(1e-15,3),
                                        upper=NULL,
                                        find.FCAR,
                                        stk=iter(stf[,FcY],iTer),
                                        TACS=iter(TACS[,FcY],iTer),
                                        refs.=referencePoints,
                                        jac=NULL,nls.lm.control(ftol = (.Machine$double.eps),
                                                                maxiter = 1000))$par

# create 4 element vector. Scalor A fleet = scalor B fleet
res <- cbind(res[1,],res[1,],res[2,],res[3,])

# update F with scalors
stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],c(3,6),res,"*")

# update catch and landings for each fleet in Forecast year
for(i in dms$unit){
  stf@catch.n[,FcY,i]         <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
  stf@catch[,FcY,i]           <- computeCatch(stf[,FcY,i])
  stf@landings.n[,FcY,i]      <- stf@catch.n[,FcY,i]
  stf@landings[,FcY,i]        <- computeLandings(stf[,FcY,i])
}

ssb.FcY <- quantSums( stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                      exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1] *
                      stf@m.spwn[,FcY,1]) * stf@mat[,FcY,1])

# propagate stock to continuation year
for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
# calculate SSB
ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY
