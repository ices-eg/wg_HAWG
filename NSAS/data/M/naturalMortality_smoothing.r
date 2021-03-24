rm(list=ls())

library(nlme)
library(tidyr)
library(ggplot2)

path <- "J:/git/wg_HAWG/NSAS"


                                                  
try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
data.dir            <-  file.path(".","data/")              # result directory

keyRuns <- c(2019)

### ============================================================================
### read files
### ============================================================================

M2M1_smooth_2016    <- read.csv(file.path(data.dir,"M","M_NSAS_smoothedSpan50_notExtrapolated_2016.csv"),header=TRUE,check.names = FALSE)
M2M1_raw_2016       <- read.csv(file.path(data.dir,"M","M_NSAS_raw_notExtrapolated_2016.csv"),header=TRUE,check.names = FALSE)
M2M1_raw_2019     <- read.csv(file.path(data.dir,"M","M_NSAS_raw_notExtrapolated_2019.csv"),header=TRUE,check.names = FALSE)

# reformat. Important to note, the 2016 run is M2 only.
M2M1_raw_2019 <- gather(M2M1_raw_2019,'year','M',-age)
M2M1_raw_2019$year  <- as.numeric(M2M1_raw_2019$year)
M2M1_raw_2019$run   <- '2019_raw'

M2M1_raw_2016 <- gather(M2M1_raw_2016,'year','M',-age)
M2M1_raw_2016$year <- as.numeric(M2M1_raw_2016$year)
M2M1_raw_2016$run   <- '2016_raw'

M2M1_smooth_2016      <- gather(M2M1_smooth_2016,'year','M',-age)
M2M1_smooth_2016$year <- as.numeric(M2M1_smooth_2016$year)
M2M1_smooth_2016$run  <- '2016_smooth'

M2M1_all <- rbind(M2M1_raw_2016,M2M1_raw_2019,M2M1_smooth_2016)

### ============================================================================
### process time series
### ============================================================================

storeSmooth   <- array(NA,dim=c(length(sort(unique(M2M1_raw_2019$age))),
                                length(keyRuns),
                                length(sort(unique(M2M1_raw_2019$year))),
                                3),
                          dimnames=list(age=sort(unique(M2M1_raw_2019$age)),
                                        SMS=c(keyRuns),
                                        year=sort(unique(M2M1_raw_2019$year)),
                                        Fit=c("5%","50%","95")))
for(iAge in sort(unique(M2M1_raw_2019$age))){
  #for(iSMS in sort(unique(SMSher$SMSyear))){
  for(iSMS in keyRuns){
    res         <- predict(loess((M)~year,data=subset(M2M1_raw_2019, age == iAge),span=0.5),
                           newdata=expand.grid(year=sort(unique(subset(M2M1_raw_2019,age == iAge)$year))),
                           se=T)                           
    yrs         <- sort(unique(subset(M2M1_raw_2019, age == iAge)$year))
    storeSmooth[ac(iAge),ac(iSMS),ac(yrs),] <- matrix(c(res$fit-1.96*res$se.fit,res$fit,res$fit+1.96*res$se.fit),nrow=length(yrs),ncol=3)
  }
}

# convert to df
M2M1_smooth_2019      <- as.data.frame(storeSmooth[,"2019",,"50%"])
M2M1_smooth_2019$age  <- rownames(M2M1_smooth_2019)
M2M1_smooth_2019      <- gather(M2M1_smooth_2019,'year','M',-age)
M2M1_smooth_2019$year <- as.numeric(M2M1_smooth_2019$year)
M2M1_smooth_2019$run  <- '2019_smooth'

M2M1_all <- rbind(M2M1_all,M2M1_smooth_2019)

# save file
#write.csv(storeSmooth[,"2019",,"50%"],file=file.path(data.dir,"M_NSAS_smoothedSpan50_notExtrapolated_2019.csv"))
#write.csv(storeSmooth[,"2019",,"50%"],file=file.path(data.dir,'M',"M_NSAS_smoothedSpan50_notExtrapolated_2019.csv"))

### ============================================================================
### Plotting
### ============================================================================

scaling_factor <- 2
png(file.path(file.path(data.dir,"M",'NSAS_M_comparison.png')), 
    width = 12*scaling_factor, height = 8*scaling_factor, units = "cm", res = 300, pointsize = 10)

p <- ggplot(M2M1_all,aes(x=year,y=M-0.1,color=run))+
      geom_line()+
      expand_limits(y=0)+
      facet_wrap(~age,scales='free')

print(p)
dev.off()