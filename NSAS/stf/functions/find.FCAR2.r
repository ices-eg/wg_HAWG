#-------------------------------------------------------------------------------
#- Fmsy: calculate A TAC under a given Fmsy Advice Rule value for the combination of the fleets
#-------------------------------------------------------------------------------
find.FCAR2 <- function(mult,stk.=stk,f26.=f26,f01.=f01,TACS.=TACS,WBSScatch=WBSScatch,Csplit=Csplit,refs.){
  mult                <- c(mult[1],mult[1],mult[2],mult[3])
  stk.@harvest        <- sweep(stk.@harvest,3,mult,"*")
  stkZ                <- unitSums(stk.@harvest) + stk.@m[,,1]
  
  for(i in 1:dims(stk.)$unit){
    stk.@catch.n[,,i]         <- stk.@stock.n[,,i]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,i]))*(stk.@harvest[,,i]/(unitSums(stk.@harvest)+stk.@m[,,i]))
    stk.@catch[,,i]           <- computeCatch(stk.[,,i])
  }
  ssb                 <- quantSums(stk.@stock.n[,,1] * stk.@stock.wt[,,1] *
                                     exp(-unitSums(stk.@harvest)*stk.@harvest.spwn[,,1]-stk.@m[,,1] *
                                           stk.@m.spwn[,,1]) * stk.@mat[,,1])
  
  f.                  <- ifelse(ssb<refs.$MSYBtrigger,ssb/refs.$MSYBtrigger * refs.$Fmsy,refs.$Fmsy)
  resA                <- sqrt(c(f. - quantMeans(unitSums(stk.@harvest[f26.,])))^2)*100
  resB                <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n * stk.@catch.wt * sweep(stk.@harvest,c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))[2]
  resC                <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n * stk.@catch.wt * sweep(stk.@harvest,c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))[3]
  resD                <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n * stk.@catch.wt * sweep(stk.@harvest,c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))[4]
  res                 <- c(resA,resC,resD)
  return(res)}