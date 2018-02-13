#-------------------------------------------------------------------------------
#- WKPELA 2012
#
# Looking at herring natural estimates in retrospect
#
# By: Niels Hintzen
# Date: 19 Dec 2011
#
# SMS key-run 2007 & SMS key-run 2010
# Fit loess smoother and use fitted values
# Use autocorrelation to estimate best period to use as extrapolation smoothing
#  period
#
#-------------------------------------------------------------------------------
rm(list=ls())

library(lattice)
library(latticeExtra)
library(nlme)
##library(visstatExtraction)
#source("D:/Sascha/Projects/HAWG/HAWG 2016/data/M/code/vectorise.r")
#ac                      <- function(x){return(as.character(x))}
#codePath                <- "D:/Sascha/Projects/HAWG/HAWG 2016/data/M/code/"
#dataPath                <- "D:/Sascha/Projects/HAWG/HAWG 2016/data/M/data/"
#outPath                 <- "D:/Sascha/Projects/HAWG/HAWG 2016/data/M/output/"
#
#  #-----------------------------------------------------------------------------
#  # 1) Read in the 2007 and 2010 SMS
#  #-----------------------------------------------------------------------------
#
#load(file=paste(dataPath,"SMS2007_her2007.RData",sep=""))
#load(file=paste(dataPath,"SMS2010_her2010.RData",sep=""))
#load(file=paste(dataPath,"SMS2007_her2007PM2.RData",sep=""))
#load(file=paste(dataPath,"SMS2010_her2010PM2.RData",sep=""))
#her2015 <- read.csv(file=paste(dataPath,"NS-keyRun-SMS-details-input-output-summary2.csv",sep="")) #read raw data file from SMS run
#her2015 <- her2015[which(her2015$Species=="Herring"),] #only pick out "Herring" values
#her2015 <- her2015[,c("Year","Quarter","Age","M1","M2","F","Z","N","Yield")] #select columns needed
#her2015 <- ddply(her2015,c("Year","Age"),summarise,M1=sum(M1),M2=sum(M2),F=sum(F),Z=sum(Z),N=sum(N),Yield=sum(Yield)) #summarise over quarters by year and age
#her2015 <- her2015[order(her2015$Age,her2015$Year),] #order by age and year
#her2015 <- her2015[which(her2015$Year<2014),] #only take years until 2013
#
#her2007$SMSyear     <- 2007
#her2010$SMSyear     <- 2010
##her2007PM2$SMSyear  <- 2007
##her2010PM2$SMSyear  <- 2010
#her2015$SMSyear     <- 2015

# local path

#path <- "C:/Users/brune001/my git files/wg_HAWG/NSAS/benchmark/"
path <- "D:/git/wg_HAWG/NSAS/benchmark/"


                                                  
try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
output.dir          <-  file.path(path,"data/M_scaling_litterature/")                #data directory
input.dir           <- file.path(path,"data/")
#.libPaths("C:/software/Rpackages")


### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA); library(FLBRP)
source("vectorise.r")
load(file.path(input.dir,"natMortDataFrame.RData"))


  #-----------------------------------------------------------------------------
  # 1b) create M matrix raw from 2016 data
  #-----------------------------------------------------------------------------

scalor <- c(1.873544565,1.260486787,1.639743892,1.905761982,2.093246989,2.2325339,2.383022972,2.472366602,2.52327097,2.595772687)
SMSher              <- (her2016)
SMSher$M            <- SMSher$M1+SMSher$M2
agesM <- unique(SMSher$Age)

for(idxAge in 1:length(agesM)){
  SMSher$M[SMSher$Age == agesM[idxAge]] <- SMSher$M[SMSher$Age == agesM[idxAge]]*scalor[idxAge]
}

#rawM                <- t(matrix(subset(SMSher,SMSyear==2010)$M,ncol=length(sort(unique(SMSher$Age))),nrow=length(sort(unique(SMSher$Year))),
#                                dimnames=list(sort(unique(SMSher$Year)),sort(unique(SMSher$Age)))))
rawM                <- t(matrix(SMSher$M,ncol=length(sort(unique(SMSher$Age))),nrow=length(sort(unique(SMSher$Year))),
                                dimnames=list(sort(unique(SMSher$Year)),sort(unique(SMSher$Age)))))
years   <- 1960:2016
extryrs <- 1960:1973
ages    <- 0:9
finalM  <- matrix(NA,nrow=length(ages),ncol=length(1960:max(years)),dimnames=list(ages=ages,years=years))

  #- Fill in values already known for finalM
finalM[ac(0:9),ac(1974:2016)] <- rawM
write.csv(finalM,file=paste(output.dir ,"/Raw_NotExtrapolated_NSAS2016.csv",sep=""))

  #-----------------------------------------------------------------------------
#  # 1c) create M matrix raw from 2015 data
#  #-----------------------------------------------------------------------------
#
#SMSher              <- rbind(her2007[which(her2007$Year>1973),],her2010[which(her2010$Year>1973),],her2015)
#SMSher$M            <- SMSher$M1+SMSher$M2
#rawM                <- t(matrix(subset(SMSher,SMSyear==2015)$M,ncol=length(sort(unique(SMSher$Age))),nrow=length(sort(unique(SMSher$Year))),
#                                dimnames=list(sort(unique(SMSher$Year)),sort(unique(SMSher$Age)))))
#years   <- 1960:2013
##extryrs <- 1960:1962
#ages    <- 0:9
#finalM  <- matrix(NA,nrow=length(ages),ncol=length(1960:max(years)),dimnames=list(ages=ages,years=years))
#
##- Fill in values already known for finalM
#finalM[ac(0:7),ac(1974:2013)] <- rawM
#write.csv(finalM,file=paste(outPath,"Raw_NotExtrapolated_NSAS_NEW.csv",sep=""))
#

  #-----------------------------------------------------------------------------
  # 2) Plot the natural mortality estimates of the three SMS key-runs
  #-----------------------------------------------------------------------------

windows()
xyplot((M1+M2)~Year|as.factor(Age),data=SMSher,type="l",xlab="Years",ylab="Total Natural Mortality",
       prepanel=function(...) {list(ylim=range(c(0,list(...)$y*1.05)))},main="North Sea herring M SMS2007",
       panel=function(...){
        dat <- list(...)
        panel.grid(h=-1, v= -1)
        panel.xyplot(...)
        panel.loess(...,col="red",lwd=2)
       },
       scales=list(alternating=1,y=list(relation="free",rot=0)))
savePlot(paste(output.dir ,"/SMS2016_TotalNaturalMortalityNSAS.png",sep=""),type="png")
#
#windows()
#xyplot((M1+M2)~Year|as.factor(Age),data=subset(SMSher,SMSyear==2010),type="l",xlab="Years",ylab="Total Natural Mortality",
#       prepanel=function(...) {list(ylim=range(c(0,list(...)$y*1.05)))},main="North Sea herring M SMS2010",
#       panel=function(...){
#        dat <- list(...)
#        panel.grid(h=-1, v= -1)
#        panel.xyplot(...)
#        panel.loess(...,col="red",lwd=2)
#       },
#       scales=list(alternating=1,y=list(relation="free",rot=0)),add=TRUE)
#savePlot(paste(outPath,"SMS2010_TotalNaturalMortalityNSAS.png",sep=""),type="png")
#
#windows()
#xyplot((M1+M2)~Year|as.factor(Age),data=subset(SMSher,SMSyear==2015),type="l",xlab="Years",ylab="Total Natural Mortality",
#       prepanel=function(...) {list(ylim=range(c(0,list(...)$y*1.05)))},main="North Sea herring M SMS2015",
#       panel=function(...){
#         dat <- list(...)
#         panel.grid(h=-1, v= -1)
#         panel.xyplot(...)
#         panel.loess(...,col="red",lwd=2)
#       },
#       scales=list(alternating=1,y=list(relation="free",rot=0)))
#savePlot(paste(outPath,"SMS2015_TotalNaturalMortalityNSAS.png",sep=""),type="png")

  #-----------------------------------------------------------------------------
  # 3) Fit loess smoothers to each age and SMS year and predict new smoothed values
  #-----------------------------------------------------------------------------

storeSmooth   <- array(NA,dim=c(length(sort(unique(SMSher$Age))),1,length(sort(unique(SMSher$Year))),3),
                          dimnames=list(Age=sort(unique(SMSher$Age)),SMS=c(2016),Year=sort(unique(SMSher$Year)),Fit=c("5%","50%","95")))
for(iAge in sort(unique(SMSher$Age))){
  #for(iSMS in sort(unique(SMSher$SMSyear))){
  for(iSMS in 2016){
#    res         <- predict(loess((M1+M2)~Year,data=subset(SMSher,SMSyear==iSMS & Age == iAge),span=0.5),
#                           newdata=expand.grid(Year=sort(unique(subset(SMSher,SMSyear==iSMS & Age == iAge)$Year))),
#                           se=T)
#   yrs         <- sort(unique(subset(SMSher,SMSyear==iSMS & Age == iAge)$Year))
#   storeSmooth[ac(iAge),ac(iSMS),ac(yrs),] <- matrix(c(res$fit-1.96*res$se.fit,res$fit,res$fit+1.96*res$se.fit),nrow=length(yrs),ncol=3)
    res         <- predict(loess((M1+M2)~Year,data=subset(SMSher, Age == iAge),span=0.5),
                           newdata=expand.grid(Year=sort(unique(subset(SMSher,Age == iAge)$Year))),
                           se=T)                           
    yrs         <- sort(unique(subset(SMSher, Age == iAge)$Year))
    storeSmooth[ac(iAge),ac(iSMS),ac(yrs),] <- matrix(c(res$fit-1.96*res$se.fit,res$fit,res$fit+1.96*res$se.fit),nrow=length(yrs),ncol=3)
  }
}

  #-----------------------------------------------------------------------------
  # 4) Plot averaged new predicted smoothed values
  #-----------------------------------------------------------------------------
#
#  #- 2010
#dtfSmooth           <- vectorise(storeSmooth[,"2010",,"50%"])
#dtfRaw              <- subset(SMSher,SMSyear==2010)[,c("M1","M2","Age","Year")]
#  dtfRaw$M          <- dtfRaw$M1+dtfRaw$M2
#  dtfRaw            <- dtfRaw[,c("M","Age","Year")]; colnames(dtfRaw) <- c("Value","Age","Year")
#colnames(dtfSmooth) <- c("Value","Age","Year")
#dtfSmooth$Age       <- as.factor(dtfSmooth$Age)
#dtfSmooth$Year      <- as.numeric(dtfSmooth$Year)
#dtftotal            <- rbind(cbind(dtfSmooth,type="Smooth"),cbind(dtfRaw,type="Raw"))
##save(file="dtftotal2010.RData",dtftotal)

#- 2016
dtfSmooth           <- vectorise(storeSmooth[,"2016",,"50%"])
dtfRaw              <- SMSher[,c("M1","M2","Age","Year")]
dtfRaw$M          <- dtfRaw$M1+dtfRaw$M2
dtfRaw            <- dtfRaw[,c("M","Age","Year")]; colnames(dtfRaw) <- c("Value","Age","Year")
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
savePlot(paste(output.dir,"/SmoothedNaturalMortalityNSAS_NEW.png",sep=""),type="png")

  #-----------------------------------------------------------------------------
  # 5) Check for lags in autocorrelation to see what extrapolation smoother
  #     years can be taken
  #-----------------------------------------------------------------------------

#sigac     <- matrix(NA,nrow=length(sort(unique(SMSher$Age))),ncol=length(sort(unique(SMSher$SMSyear))),dimnames=list(age=sort(unique(SMSher$Age)),SMS=sort(unique(SMSher$SMSyear))))
#for(iAge in sort(unique(SMSher$Age))){
#  for(iSMS in sort(unique(SMSher$SMSyear))){
#    tsdat <- subset(SMSher,SMSyear==iSMS & Age == iAge)$M1+subset(SMSher,SMSyear==iSMS & Age == iAge)$M2
#    n     <- length(subset(SMSher,SMSyear==iSMS & Age == iAge)$M1+subset(SMSher,SMSyear==iSMS & Age == iAge)$M2)
#    ci    <- 0.95
#    clim0 <- qnorm((1+ci)/2)/sqrt(n)
#    autco <- acf(as.ts(tsdat),ci.type="ma",plot=F)
#    clim  <- clim0 * sqrt(cumsum(c(1, 2 * autco$acf[-1, 1, 1]^2)))
#    sigac[ac(iAge),ac(iSMS)] <- max(which(autco$acf[-1]>clim[-length(clim)]))
#  }
#}
#
#sigacSM   <- matrix(NA,nrow=length(sort(unique(SMSher$Age))),ncol=1,dimnames=list(age=sort(unique(SMSher$Age)),SMS="smooth"))
#for(iAge in sort(unique(subset(dtfSmooth,Year %in% 1963:2010)$Age))){
#  tsdat <- subset(dtfSmooth,Year %in% 1963:2007 & Age == iAge)$Value
#  n     <- length(subset(dtfSmooth,Year %in% 1963:2007 & Age == iAge)$Value)
#  ci    <- 0.95
#  clim0 <- qnorm((1+ci)/2)/sqrt(n)
#  autco <- acf(as.ts(tsdat),ci.type="ma",plot=F)
#  clim  <- clim0 * sqrt(cumsum(c(1, 2 * autco$acf[-1, 1, 1]^2)))
#  sigacSM[ac(iAge),1] <- max(which(autco$acf[-1]>clim[-length(clim)]))
#}

  #-----------------------------------------------------------------------------
  # 6) Extrapolate the M-at-age
  #-----------------------------------------------------------------------------

years   <- 1974:2016
extryrs <- years[which(!years %in% unique(subset(dtfSmooth,Year %in% 1963:2016)$Year))]
ages    <- 0:9
finalM  <- matrix(NA,nrow=length(ages),ncol=length(1974:max(years)),dimnames=list(ages=ages,years=years))

  #- Fill in values already known for finalM
finalM[ac(0:9),ac(1974:2016)] <- storeSmooth[,"2016",,"50%"]
write.csv(finalM,file=paste(output.dir,"/Smoothed_span50_M_NotExtrapolated_NSAS2016.csv",sep=""))

#  #- Extrapolate for early years back based on age-smoother correlation factor
#for(iYr in rev(sort(extryrs[which(extryrs < min(unique(subset(dtfSmooth,Year %in% 1963:2007)$Year)))]))){
#  for(iAge in 0:7){
#    finalM[ac(iAge),ac(iYr)] <- mean(finalM[ac(iAge),ac((iYr+1):(iYr+sigacSM[ac(iAge),1]))],na.rm=T)
#  }
#}

#  #- Extrapolate for continuation years forward based on age-smoother correlation factor
#for(iYr in sort(extryrs[which(extryrs > max(unique(subset(dtfSmooth,Year %in% 1963:2007)$Year)))])){
#  for(iAge in 0:7){
#    finalM[ac(iAge),ac(iYr)] <- mean(finalM[ac(iAge),ac((iYr-1):(iYr-sigacSM[ac(iAge),1]))],na.rm=T)
#  }
#}

#  #- Fill in values for ages 8 and 9, similar to age 7
#finalM[ac(8),]    <- finalM[ac(7),]
#finalM[ac(9),]    <- finalM[ac(7),]
#
#MSmooth           <- vectorise(finalM)
#colnames(MSmooth) <- c("Value","Age","Year")
#MSmooth$Age       <- as.factor(MSmooth$Age)
#MSmooth$Year      <- as.numeric(MSmooth$Year)
#
#  #- Plot the result
#
#windows()
#xyplot(Value~Year|Age,data=MSmooth,type="l",xlab="Years",ylab="Total Natural Mortality",
#       prepanel=function(...) {list(ylim=range(c(0,list(...)$y*1.05)))},main="North Sea herring smoothed M",
#       panel=function(...){
#        dat <- list(...)
#        panel.grid(h=-1, v= -1)
#        panel.xyplot(...)
#       },
#       scales=list(alternating=1,y=list(relation="free",rot=0)))
#savePlot(paste(outPath,"SmoothedNaturalMortalityNSAS.png",sep=""),type="png")
#
#  #- Save the final matrix
#write.csv(finalM,file=paste(outPath,"../SmoothedMNSAS.csv",sep=""))
# 
#-----------------------------------------------------------------------------
# 7) plot comparison between assessment runs
#-----------------------------------------------------------------------------

finalM2015 <- finalM

load("finalM2010.RData")
finalM[ac(8),]    <- finalM[ac(7),]
finalM[ac(9),]    <- finalM[ac(7),]
finalM2010 <- finalM
load("finalM2013.RData")
finalM[ac(8),]    <- finalM[ac(7),]
finalM[ac(9),]    <- finalM[ac(7),]
finalM2013 <- finalM

MSmooth           <- vectorise(finalM2010)
colnames(MSmooth) <- c("Value","Age","Year")
MSmooth$Age       <- as.factor(MSmooth$Age)
MSmooth$Year      <- as.numeric(MSmooth$Year)
a <- xyplot(Value~Year|Age,data=MSmooth,type="l",xlab="Years",ylab="Total Natural Mortality",
       prepanel=function(...) {list(ylim=range(c(0,1.5)))},main="North Sea herring smoothed M",
       panel=function(...){
        dat <- list(...)
        panel.grid(h=-1, v= -1)
        panel.xyplot(...,col='red')
       },
       scales=list(alternating=1,y=list(relation="free",rot=0)))
MSmooth           <- vectorise(finalM2015)
colnames(MSmooth) <- c("Value","Age","Year")
MSmooth$Age       <- as.factor(MSmooth$Age)
MSmooth$Year      <- as.numeric(MSmooth$Year)
b <- xyplot(Value~Year|Age,data=MSmooth,type="l",xlab="Years",ylab="Total Natural Mortality",
       prepanel=function(...) {list(ylim=range(c(0,1.5)))},main="North Sea herring smoothed M",
       panel=function(...){
         dat <- list(...)
         panel.grid(h=-1, v= -1)
         panel.xyplot(...,col='blue')
       },
       scales=list(alternating=1,y=list(relation="free",rot=0)))

windows()
a + as.layer(b)
savePlot(paste(output.dir,"SmoothedNaturalMortalityNSAS_combined2018.png",sep=""),type="png")
