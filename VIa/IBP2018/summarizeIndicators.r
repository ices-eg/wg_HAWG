library(FLSAM)
setwd("D:/Repository/ICES_HAWG/wg_HAWG/VIa/IBP2018/results")

fileNames   <-  dir()[grep("_MF",dir())]#[c(1,7,4,6,2,3,5)]
indicators  <- matrix(NA,nrow=length(fileNames),ncol=9,dimnames=list(run=unlist(lapply(strsplit(fileNames,"_"),function(x){x[3]})),indicator=c("AIC","MohnsRhoSSB","MohnsRhoF","MohnsRhoR","MohnsRhoSSBAbs","MohnsRhoFAbs","MohnsRhoRAbs","loglik","nparam")))
for(iFile in fileNames){
  load(iFile)
  print(iFile)
  indicators[strsplit(iFile,"_")[[1]][3],"AIC"]         <- AIC(MSHm.sam)
  if(length(MSHm.retro)==8){
    indicators[strsplit(iFile,"_")[[1]][3],"MohnsRhoSSB"]     <- mean(mohns.rho(MSHm.retro,ref.year=2017,span=7,type="ssb")[1:5,1])
    indicators[strsplit(iFile,"_")[[1]][3],"MohnsRhoSSBAbs"]  <- mean(abs(mohns.rho(MSHm.retro,ref.year=2017,span=7,type="ssb")[1:5,1]))
    indicators[strsplit(iFile,"_")[[1]][3],"MohnsRhoF"]   <- mean(mohns.rho(MSHm.retro,ref.year=2017,span=7,type="fbar")[1:5,1])
    indicators[strsplit(iFile,"_")[[1]][3],"MohnsRhoFAbs"]   <- mean(abs(mohns.rho(MSHm.retro,ref.year=2017,span=7,type="fbar")[1:5,1]))
    indicators[strsplit(iFile,"_")[[1]][3],"MohnsRhoR"]   <- mean(mohns.rho(MSHm.retro,ref.year=2017,span=7,type="rec")[1:5,1])
    indicators[strsplit(iFile,"_")[[1]][3],"MohnsRhoRAbs"]   <- mean(abs(mohns.rho(MSHm.retro,ref.year=2017,span=7,type="rec")[1:5,1]))
  }
  indicators[strsplit(iFile,"_")[[1]][3],"loglik"]      <- MSHm.sam@nlogl
  indicators[strsplit(iFile,"_")[[1]][3],"nparam"]      <- MSHm.sam@nopar
}
indicators <- cbind(indicators,sumRho=rowSums(abs(indicators[,2:4])),sumRhoAbs=rowSums(abs(indicators[,5:7])))
indicators <- cbind(indicators,sumRhos=indicators[,"sumRho"] + indicators[,"sumRhoAbs"])
indicators

