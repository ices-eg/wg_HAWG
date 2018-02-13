#-------------------------------------------------------------------------------
#- WKPELA 2018
#
# Looking at herring natural estimates in retrospect
#
# By: Niels Hintzen
# Date: 19 Dec 2011
#
# SMS key-run 2007 & SMS key-run 2010
# Fit loess smoother and use fitted values
# Use autocorrelation to estimate best period to use as extrapolation smoothing
# period
#
# 13/02/2018 update by Martin Pastoors; scaled to Old M from 2017 assessment
#-------------------------------------------------------------------------------
rm(list=ls())

library(lattice)
library(latticeExtra)
library(nlme)
library(tidyverse)

library(FLSAM)
library(FLEDA)
library(FLBRP)

source("vectorise.r")
path <- "D:/git/wg_HAWG/NSAS/benchmark/"
try(setwd(path),silent=TRUE)

# output directory
output.dir  <-  file.path(path,"R/2b_newM_scaledpast/") 

# load natural mortality data frame from SMS2017
load(file.path(output.dir,"natMortDataFrame.RData"))

### ======================================================================================================
### Get M from last years assessment
### ======================================================================================================

# read the old M from file
load("D:/HAWG/2017/06. Data/NSAS/SAM/North Sea Herring 2017.RData");     

# calculate scaler
scaler <-
  as.data.frame(NSH@m) %>% 
  group_by(age) %>% 
  summarize(oldm = mean(data, na.rm=TRUE))


#-----------------------------------------------------------------------------
# 1b) create M matrix raw from 2016 data
#-----------------------------------------------------------------------------

SMSher  <-  (her2016)

scalor <-
  SMSher %>% 
  group_by(Age) %>% 
  mutate(M = M1 + M2) %>% 
  summarize(M = mean(M)) %>%
  rename(age = Age) %>% 
  left_join(scaler, by=c("age")) %>% 
  mutate(scaler = oldm / M) %>% 
  select(scaler) %>% 
  unlist()

SMSher$M   <- SMSher$M1+SMSher$M2
agesM      <- unique(SMSher$Age)

for(idxAge in 1:length(agesM)){
  SMSher$M[SMSher$Age == agesM[idxAge]] <- SMSher$M[SMSher$Age == agesM[idxAge]]*scalor[idxAge]
}

rawM                <- t(matrix(SMSher$M,
                                ncol=length(sort(unique(SMSher$Age))),
                                nrow=length(sort(unique(SMSher$Year))),
                                dimnames=list(sort(unique(SMSher$Year)),
                                              sort(unique(SMSher$Age)))))


years   <- 1960:2016
extryrs <- 1960:1973
ages    <- 0:9
finalM  <- matrix(NA,nrow=length(ages),ncol=length(1960:max(years)),dimnames=list(ages=ages,years=years))

  #- Fill in values already known for finalM
finalM[ac(0:9),ac(1974:2016)] <- rawM
write.csv(finalM,file=paste(output.dir ,"/Raw_NotExtrapolated_NSAS2016scaledto2014.csv",sep=""))


#-----------------------------------------------------------------------------
# 2) Plot the natural mortality estimates of the last SMS key-run
#-----------------------------------------------------------------------------

windows()
xyplot((M1+M2)~Year|as.factor(Age),data=SMSher,type="l",xlab="Years",ylab="Total Natural Mortality",
       prepanel=function(...) {list(ylim=range(c(0,list(...)$y*1.05)))},main="North Sea herring M SMS2017 scaled to 2014",
       panel=function(...){
        dat <- list(...)
        panel.grid(h=-1, v= -1)
        panel.xyplot(...)
        panel.loess(...,col="red",lwd=2)
       },
       scales=list(alternating=1,y=list(relation="free",rot=0)))
savePlot(paste(output.dir ,"/SMS2017_TotalNaturalMortalityNSASscaledto2014.png",sep=""),type="png")


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

#- 2016
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
savePlot(paste(output.dir,"/SmoothedNaturalMortalityNSAS_SMS2017scaledto2014.png",sep=""),type="png")

#-----------------------------------------------------------------------------
# 5) Check for lags in autocorrelation to see what extrapolation smoother
#     years can be taken
#-----------------------------------------------------------------------------

# Removed

#-----------------------------------------------------------------------------
# 6) Extrapolate the M-at-age
#-----------------------------------------------------------------------------

years   <- 1974:2016
extryrs <- years[which(!years %in% unique(subset(dtfSmooth,Year %in% 1963:2016)$Year))]
ages    <- 0:9
finalM  <- matrix(NA,nrow=length(ages),ncol=length(1974:max(years)),dimnames=list(ages=ages,years=years))

#- Fill in values already known for finalM
finalM[ac(0:9),ac(1974:2016)] <- storeSmooth[,"2016",,"50%"]
write.csv(finalM,file=paste(output.dir,"/Smoothed_span50_M_NotExtrapolated_NSAS2016scaledto2014.csv",sep=""))

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
savePlot(paste(outPath,"SmoothedNaturalMortalityNSAS_combined2018scaledto2014.png",sep=""),type="png")
