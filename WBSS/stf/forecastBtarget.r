
wbss <- wbss.option[[1]]
wbss@harvest[,ac(2017)] <- wbss@harvest[,ac(2016)]
wbss@harvest[,ac(2018)] <- wbss@harvest[,ac(2016)]

mult <- optimize(f=btarget,interval=c(0,20),stk=wbss[,ac(2017:2018)],target=90000,rec=1855751)$minimum


btarget <- function(mult,stk,target,rec){
  stk@harvest <- stk@harvest * mult
  stock.n     <- stk@stock.n[,1] *exp(-stk@harvest[,1]-stk@m[,1])
  stk@stock.n[-c(1,dims(stk)$age),2] <- stock.n[1:(dims(stk)$age-2)]
  stk@stock.n[1,2] <- rec
  stk@stock.n[dims(stk)$age,2] <- sum(stock.n[(dims(stk)$age-1):(dims(stk)$age),])
  ssb         <- ssb(stk[,2])
  print(ssb); print(mult)
return(sqrt(c(ssb-target)^2))}

wbss@harvest[,ac(2017)] <- wbss@harvest[,ac(2016)] * mult
wbss@harvest[,ac(2018)] <- wbss@harvest[,ac(2016)] * mult

wbss@stock.n[-c(1,9),ac(2017)] <- (wbss@stock.n[,ac(2016)] * exp(-wbss@harvest[,ac(2016)]-wbss@m[,ac(2016)]))[1:7]
wbss@stock.n[1,ac(2017)] <- 1855751
wbss@stock.n[9,ac(2017)] <- sum((wbss@stock.n[,ac(2016)] * exp(-wbss@harvest[,ac(2016)]-wbss@m[,ac(2016)]))[8:9])

wbss@stock.n[-c(1,9),ac(2018)] <- (wbss@stock.n[,ac(2017)] * exp(-wbss@harvest[,ac(2017)]-wbss@m[,ac(2017)]))[1:7]
wbss@stock.n[1,ac(2018)] <- 1855751
wbss@stock.n[9,ac(2018)] <- sum((wbss@stock.n[,ac(2017)] * exp(-wbss@harvest[,ac(2017)]-wbss@m[,ac(2017)]))[8:9])

quantSums(wbss@harvest[,ac(2017:2018)] / (wbss@harvest[,ac(2017:2018)] + wbss@m[,ac(2017:2018)]) * wbss@stock.n[,ac(2017:2018)] * (1-exp(-wbss@harvest[,ac(2017:2018)] - wbss@m[,ac(2017:2018)])) * wbss@catch.wt[,ac(2017:2018)])
ssb(wbss[,ac(2017:2018)])
fbar(wbss[,ac(2017:2018)])

bpa: catch 2017 = 104592, catch 2018 = 80739, fbar 2017,2018 = 0.68, ssb 2017 = 148652, 2018 = 110000
blim: catch 2017 = 127302, catch 2018 = 85110, fbar 2017,2018 = 0.90, ssb 2017 = 145529, 2018 = 90000
