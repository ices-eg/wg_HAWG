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

#-----------------------------------------------------------------------------
# 1. get new and old M data and calculate scaling factor
#-----------------------------------------------------------------------------

# load natural mortality data frame from SMS2017 (object her2016)
SMSher  <-  get(load(file.path(output.dir,"natMortDataFrame.RData")))

# Get M from last years assessment
load("D:/HAWG/2017/06. Data/NSAS/SAM/North Sea Herring 2017.RData");     

# calculate average m by age from old assessment
oldmall <-
  as.data.frame(NSH@m) 

oldm <-
  oldmall %>% 
  filter(year >= 1974) %>% 
  group_by(age) %>% 
  summarize(oldm = mean(data, na.rm=TRUE))

# remove old data objects
rm(NSH, NSH.ctrl, NSH.sam, NSH.tun)
rm(her2016)

# calculate scaled from newM to oldM
scaler <-
  SMSher %>% 
  mutate(M = M1 + M2) %>% 
  group_by(Age) %>% 
  summarize(M = mean(M)) %>%
  rename(age = Age) %>% 
  left_join(oldm, by=c("age")) %>% 
  mutate(ratio = oldm / M) %>% 
  rename(Age = age) %>% 
  dplyr::select(Age, ratio)

#-----------------------------------------------------------------------------
# 2. create M matrix raw from SMS 2016 data, scaled to M values of 2017 assessment
#-----------------------------------------------------------------------------

# add the M1 and M2 columns in the SMS data and apply the scaling
SMSher   <- 
  SMSher %>%
  filter(Age %in% 0:8) %>% 
  mutate(M = M1 + M2) %>% 
  left_join(scaler, by="Age") %>% 
  mutate(scaledM = M * ratio)

# convert to a matrix for further manipulations
rawM  <- t(matrix(SMSher$scaledM,
                  ncol=length(sort(unique(SMSher$Age))),
                  nrow=length(sort(unique(SMSher$Year))),
                  dimnames=list(sort(unique(SMSher$Year)),
                                sort(unique(SMSher$Age)))))


# construct matrix for final M to be used in assessment
# only fill in the years 1974 and beyond. 
years   <- 1960:2016
extryrs <- 1960:1973
ages    <- 0:9
finalM  <- matrix(NA,
                  nrow=length(ages),
                  ncol=length(1960:max(years)),
                  dimnames=list(ages=ages,years=years))

#- Fill in values already known for finalM
finalM[ac(0:8),ac(1974:2016)] <- rawM

# Write raw, non-smoothed values to file
write.csv(finalM,
          file=paste(output.dir ,"/Raw_NotExtrapolated_NSAS2016scaledto2014.csv",sep=""))

#-----------------------------------------------------------------------------
# 3) Fit loess smoothers to each age and SMS year and predict new smoothed values
#-----------------------------------------------------------------------------

# library(broom)
# library(mgcv)
# 
# storesmooth <-
#   SMSher %>% 
#   group_by(Age) %>% 
#   do(smoothed = loess(scaledM ~ Year, data = .)) 
# 
# m <- gam(scaledM ~ Age + s(Year, by = Age), data = SMSher)
# summary(m)
# pdat <- expand.grid(Year=1974:2016, Age=0:8)
# xp <- predict(m, newdata = pdat, type = 'lpmatrix')

# Create array to store smoothed values
storeSmooth   <- 
  array(NA,
        dim=c(length(sort(unique(SMSher$Age))),1,length(sort(unique(SMSher$Year))),3),
        dimnames=list(Age=sort(unique(SMSher$Age)),
                      SMS=c(2016),
                      Year=sort(unique(SMSher$Year)),
                      Fit=c("5%","50%","95%")))

# Fill the array of smoothed values
for(iAge in sort(unique(SMSher$Age))){
  for(iSMS in 2016){
    res <- 
      predict(loess((scaledM)~Year,
                    data=subset(SMSher, Age == iAge),
                    span=0.5),
              newdata=expand.grid(Year=sort(unique(subset(SMSher,Age == iAge)$Year))),
              se=T)                           
    yrs <- 
      sort(unique(subset(SMSher, Age == iAge)$Year))
    
    storeSmooth[ac(iAge),ac(iSMS),ac(yrs),] <- 
      matrix(c(res$fit-1.96*res$se.fit,res$fit,res$fit+1.96*res$se.fit),
             nrow=length(yrs),
             ncol=3)
  }
}





#-----------------------------------------------------------------------------
# 4) Extrapolate the M-at-age
#-----------------------------------------------------------------------------

dtfSmooth           <- vectorise(storeSmooth[,"2016",,"50%"])
dtfRaw              <- SMSher[,c("M1","M2","scaledM", "Age","Year")]
# dtfRaw$M            <- dtfRaw$M1+dtfRaw$M2
dtfRaw              <- dtfRaw[,c("scaledM","Age","Year")]; colnames(dtfRaw) <- c("Value","Age","Year")
colnames(dtfSmooth) <- c("Value","Age","Year")
dtfSmooth$Age       <- as.factor(dtfSmooth$Age)
dtfSmooth$Year      <- as.numeric(dtfSmooth$Year)
dtftotal            <- rbind(cbind(dtfSmooth,type="Smooth"),cbind(dtfRaw,type="Raw"))

years   <- 1974:2016
extryrs <- years[which(!years %in% unique(subset(dtfSmooth,Year %in% 1963:2016)$Year))]
ages    <- 0:9
finalM  <- matrix(NA,nrow=length(ages),ncol=length(1974:max(years)),dimnames=list(ages=ages,years=years))

#- Fill in values already known for finalM
finalM[ac(0:8),ac(years)] <- storeSmooth[,"2016",,"50%"]
write.csv(finalM,file=paste(output.dir,"/Smoothed_span50_M_NotExtrapolated_NSAS2016scaledto2014.csv",sep=""))


#-----------------------------------------------------------------------------
# 2) Plot the raw natural mortality estimates of the last SMS key-run
#-----------------------------------------------------------------------------

SMSher %>% 
  rename(age=Age, year=Year) %>% 
  
  ggplot(aes(x=year, y=scaledM, group=age)) +
  geom_line(aes(y=M), colour="red") +
  geom_line(aes(y=scaledM), size=1, colour="blue") +
  geom_line(data=oldmall, aes(x=year, y=data), colour="green") +
  facet_wrap(~age, scales="free_y")

# savePlot(paste(output.dir ,"/SMS2017_TotalNaturalMortalityNSASscaledto2014.png",sep=""),type="png")

#-----------------------------------------------------------------------------
# 4) Plot averaged new predicted smoothed values
#-----------------------------------------------------------------------------

dtftotal %>% 
  ggplot(aes(x=Year, y=Value, group=type)) +
  geom_line(aes(colour=type), size=1) +
  facet_wrap(~Age, scales="free_y")

# savePlot(paste(output.dir,"/SmoothedNaturalMortalityNSAS_SMS2017scaledto2014.png",sep=""),type="png")


