#get AICs
setwd("D:/Repository/ICES_HAWG/wg_HAWG/VIa/IBP2018/results/")
runs <- dir()[grep(".Rdata",dir())][c(1,14,2,16,4,6,15,3,7,8,9,12,13,10,11,17,5,18,19,20,21)]
runNames <- gsub(".Rdata","",gsub("IBP_VIaHerring_","",runs))

AICs <- matrix(NA,nrow=length(runs),ncol=1,dimnames=list(runNames,"AIC"))
for(i in 1:length(runs)){
  load(runs[i])
  AICs[runNames[i],"AIC"] <- AIC(MSH.sam)
  rm(MSH.sam)
}
