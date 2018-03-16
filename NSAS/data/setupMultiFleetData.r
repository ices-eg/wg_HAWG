caaF  <- read.table("./data/Historic_ABCD_Fleet_NSAS_HAWG2018.txt",stringsAsFactors=F,header=T)
caA   <- matrix(subset(caaF,Fleet=="A")[,"numbers"],nrow=length(0:9),ncol=length(1997:2017),dimnames=list(age=0:9,year=1997:2017))*1000
caB   <- matrix(subset(caaF,Fleet=="B")[,"numbers"],nrow=length(0:9),ncol=length(1997:2017),dimnames=list(age=0:9,year=1997:2017))*1000
caC   <- matrix(subset(caaF,Fleet=="C")[,"numbers"],nrow=length(0:9),ncol=length(1997:2017),dimnames=list(age=0:9,year=1997:2017))*1000
caD   <- matrix(subset(caaF,Fleet=="D")[,"numbers"],nrow=length(0:9),ncol=length(1997:2017),dimnames=list(age=0:9,year=1997:2017))*1000
cwA   <- matrix(subset(caaF,Fleet=="A")[,"weight"],nrow=length(0:9),ncol=length(1997:2017),dimnames=list(age=0:9,year=1997:2017))
cwB   <- matrix(subset(caaF,Fleet=="B")[,"weight"],nrow=length(0:9),ncol=length(1997:2017),dimnames=list(age=0:9,year=1997:2017))
cwC   <- matrix(subset(caaF,Fleet=="C")[,"weight"],nrow=length(0:9),ncol=length(1997:2017),dimnames=list(age=0:9,year=1997:2017))
cwD   <- matrix(subset(caaF,Fleet=="D")[,"weight"],nrow=length(0:9),ncol=length(1997:2017),dimnames=list(age=0:9,year=1997:2017))

source(file.path("./stf/multifleet_assessment/setupAssessmentObjects_mf.r"))

setMatrixPlusGroupN <- function(x){
  x[nrow(x)-1,] <- colSums(x[(nrow(x)-1):(nrow(x)),],na.rm=T)
  return(x[-nrow(x),])}
setMatrixPlusGroupWt <- function(x,y){
  y[nrow(x)-1,] <- colSums(x[(nrow(x)-1):(nrow(x)),]*y[(nrow(y)-1):(nrow(y)),],na.rm=T) / colSums(x[(nrow(x)-1):(nrow(x)),],na.rm=T)
  y[is.nan(y)] <- 0
  return(y[-nrow(y),])}

NSHmf <- expand(NSH,area=c("A","B","C","D"))
NSHmf@catch.n[,ac(1997:2017),,,"A"]   <- setMatrixPlusGroupN(caA)
NSHmf@catch.n[,ac(1997:2017),,,"B"]   <- setMatrixPlusGroupN(caB)
NSHmf@catch.n[,ac(1997:2017),,,"C"]   <- setMatrixPlusGroupN(caC)
NSHmf@catch.n[,ac(1997:2017),,,"D"]   <- setMatrixPlusGroupN(caD)
NSHmf@catch.wt[,ac(1997:2017),,,"A"]  <- setMatrixPlusGroupWt(caA,cwA)
NSHmf@catch.wt[,ac(1997:2017),,,"B"]  <- setMatrixPlusGroupWt(caB,cwB)
NSHmf@catch.wt[,ac(1997:2017),,,"C"]  <- setMatrixPlusGroupWt(caC,cwC)
NSHmf@catch.wt[,ac(1997:2017),,,"D"]  <- setMatrixPlusGroupWt(caD,cwD)
NSHmf@catch.n[,ac(1947:1996),,,-1]    <- NA
NSHmf@catch.wt[,ac(1947:1996),,,-1]   <- NA

NSH3f <- expand(NSH,area=c("A","BD","C"))
NSH3f@catch.n[,ac(1997:2017),,,"A"]   <- setMatrixPlusGroupN(caA)
NSH3f@catch.n[,ac(1997:2017),,,"BD"]   <- setMatrixPlusGroupN(caB+caD)
NSH3f@catch.n[,ac(1997:2017),,,"C"]   <- setMatrixPlusGroupN(caC)
NSH3f@catch.wt[,ac(1997:2017),,,"A"]  <- setMatrixPlusGroupWt(caA,cwA)
NSH3f@catch.wt[,ac(1997:2017),,,"BD"]  <- setMatrixPlusGroupWt(caB+caD,(cwB*caB + cwD*caD)/(caB+caD))
NSH3f@catch.wt[,ac(1997:2017),,,"C"]  <- setMatrixPlusGroupWt(caC,cwC)
NSH3f@catch.n[,ac(1947:1996),,,-1]    <- NA
NSH3f@catch.wt[,ac(1947:1996),,,-1]   <- NA

NSHsum <- NSHmf[,ac(1947:1996),,,1]
NSHres4 <- NSHmf[,ac(1997:2017)]; NSHres4@landings.n[] <- NSHres4@catch.n
NSHres3 <- NSH3f[,ac(1997:2017)]; NSHres3@landings.n[] <- NSHres3@catch.n
NSHs4        <- FLStocks(residual=NSHres4,sum=NSHsum)
NSHs3        <- FLStocks(residual=NSHres3,sum=NSHsum)
