rm(list=ls())

library(lattice)
library(latticeExtra)
library(nlme)

path <- "D:/git/wg_HAWG/NSAS/"


                                                  
try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
output.dir          <- file.path(path,"data/")                #data directory
input.dir           <- file.path(path,"data/natural_mortality//")
SMSRun              <- 'SMS2016'
year_last           <- 2017

### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA); library(FLBRP)
source("vectorise.r")
load(paste(input.dir,"natMortDataFrame_",SMSRun,".RData",sep=""))


#-----------------------------------------------------------------------------
# 1b) create M matrix raw from 2016 data
#-----------------------------------------------------------------------------

SMSher              <- (her2016)
SMSher$M            <- SMSher$M1+SMSher$M2
agesM <- unique(SMSher$Age)

rawM                <- t(matrix(SMSher$M,ncol=length(sort(unique(SMSher$Age))),nrow=length(sort(unique(SMSher$Year))),
                                dimnames=list(sort(unique(SMSher$Year)),sort(unique(SMSher$Age)))))
years   <- 1960:2016
extryrs <- 1960:1973
ages    <- 0:9
finalM  <- matrix(NA,nrow=length(ages),ncol=length(1960:max(years)),dimnames=list(ages=ages,years=years))

#- Fill in values already known for finalM
finalM[ac(0:9),ac(1974:2016)] <- rawM
write.csv(finalM,file=paste(input.dir,"Raw_NotExtrapolated_NSAS_",SMSRun,".csv",sep=""))

#-----------------------------------------------------------------------------
# 2) compare different SMS key-runs - to be added
#-----------------------------------------------------------------------------

#windows()
#savePlot(paste(input.dir,"/SMS2016_TotalNaturalMortalityNSAS.png",sep=""),type="png")

#-----------------------------------------------------------------------------
# 3) Fit loess smoothers to each age and SMS year and predict new smoothed values
#-----------------------------------------------------------------------------

storeSmooth   <- array(NA,dim=c(length(sort(unique(SMSher$Age))),1,length(sort(unique(SMSher$Year))),3),
                          dimnames=list(Age=sort(unique(SMSher$Age)),SMS=c(2016),Year=sort(unique(SMSher$Year)),Fit=c("5%","50%","95")))
for(iAge in sort(unique(SMSher$Age))){
  #for(iSMS in sort(unique(SMSher$SMSyear))){
  for(iSMS in 2016){
    res         <- predict(loess((M)~Year,data=subset(SMSher, Age == iAge),span=0.5),
                           newdata=expand.grid(Year=sort(unique(subset(SMSher,Age == iAge)$Year))),
                           se=T)                           
    yrs         <- sort(unique(subset(SMSher, Age == iAge)$Year))
    storeSmooth[ac(iAge),ac(iSMS),ac(yrs),] <- matrix(c(res$fit-1.96*res$se.fit,res$fit,res$fit+1.96*res$se.fit),nrow=length(yrs),ncol=3)
  }
}

#-----------------------------------------------------------------------------
# 4) Plot averaged new predicted smoothed values
#-----------------------------------------------------------------------------
dtfSmooth           <- vectorise(storeSmooth[,"2016",,"50%"])
dtfRaw              <- SMSher[,c("M1","M2","Age","Year")]
dtfRaw$M            <- dtfRaw$M1+dtfRaw$M2
dtfRaw              <- dtfRaw[,c("M","Age","Year")]; colnames(dtfRaw) <- c("Value","Age","Year")
colnames(dtfSmooth) <- c("Value","Age","Year")
dtfSmooth$Age       <- as.factor(dtfSmooth$Age)
dtfSmooth$Year      <- as.numeric(dtfSmooth$Year)
dtftotal            <- rbind(cbind(dtfSmooth,type="Smooth"),cbind(dtfRaw,type="Raw"))

windows()
xyplot(Value~Year|Age,data=dtftotal,type="l",xlab="Years",ylab="Total Natural Mortality - Span = 0.5",
       groups="type",
       prepanel=function(...) {list(ylim=range(c(0,list(...)$y*1.05)))},
       main="North Sea herring M - span = 0.5",
       panel=function(...){
        dat <- list(...)
        idx1 <- 1:(length(dat$x)/2)
        idx2 <- (rev(idx1)[1]+1):length(dat$x)

        panel.grid(h=-1, v= -1)
        panel.xyplot(dat$x[idx1],dat$y[idx1],type="l",col=1)
        panel.xyplot(dat$x[idx2],dat$y[idx2],type="l",col=2)
       },
       scales=list(alternating=1,y=list(relation="free",rot=0)))
savePlot(paste(input.dir,"Raw_NotExtrapolated_NSAS_",SMSRun,".png",sep=""),type="png")

#-----------------------------------------------------------------------------
# 6) Extrapolate the M-at-age
#-----------------------------------------------------------------------------

years   <- 1974:2016
extryrs <- years[which(!years %in% unique(subset(dtfSmooth,Year %in% 1963:2016)$Year))]
ages    <- 0:9
finalM  <- matrix(NA,nrow=length(ages),ncol=length(1974:max(years)),dimnames=list(ages=ages,years=years))

  #- Fill in values already known for finalM
finalM[ac(0:9),ac(1974:2016)] <- storeSmooth[,"2016",,"50%"]
write.csv(finalM,file=paste(.dir,"Smoothed_span50_M_NotExtrapolated_NSAS",SMSRun,".csv",sep=""))
write.csv(finalM,file=paste(output.dir,"Smoothed_span50_M_NotExtrapolated_NSAS",SMSRun,".csv",sep=""))

