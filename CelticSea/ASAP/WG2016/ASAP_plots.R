#ASAP plots

rm(list=ls())
gc()

setwd("C:/ICES/HAWG2016")

library(ggplot2)
library(dplyr)

source("Supporting.R")

#runName <- "2016_prelim"
#runName <- "2016_inc_2015_survey"
#runName <- "2015_assessment"
runName <- "2016_assessment"

base <- paste0(".\\Assessment\\",runName)
plots.dir <- paste0(".\\Assessment\\plots\\",runName,"\\")
dir.create(plots.dir, recursive=TRUE, showWarnings=FALSE)

#read the ADMB report file, the input data and the precision estimates
dfOut <- fReadReport(reportFile = paste0(base,".REP"))

#report <- trim(readLines(paste0(base,".REP")))
input <- trim(readLines(paste0(base,".dat")))
prec <- trim(readLines(paste0(base,".std")))

#check time difference between precision data and report file
#a significant difference would indicate the std file was from a previous run and the current one did not complete
difftime(file.info(paste0(base,".std"))$mtime,file.info(paste0(base,".REP"))$mtime)

#line header positions
posHead <- match(ipHeaders, input)

#report any headers not found
ipHeaders[is.na(posHead)]

#report any negative differences (implying a different order for the data)
posHead[diff(posHead)<0]

names(posHead) <- ipHeaders

#report header positions
#posRepHead <- match(repHeaders, report)

#report any headers not found
#repHeaders[is.na(posRepHead)]

#report any negative differences (implying a different order for the data)
#posRepHead[diff(posRepHead)<0]

#names(posRepHead) <- repHeaders


#number of years, first year, vector of years
nyears <- as.integer(input[posHead["# Number of Years"]+1])
year1 <- as.integer(input[posHead["# First Year"]+1])
years <- year1:(year1+nyears-1)

#number if ages, firstage, vector of ages
nages <- as.integer(input[posHead["# Number of Ages"]+1])
age1 <- 1
ages <- age1:(age1+nages-1)

#indices
nind <- as.integer(input[posHead["# Number of Fleets"]+1])
indnames <- input[(posHead["# Survey Names"]+1):(posHead["# Survey Names"]+nind)]

#index ages
indStartAge <- as.integer(input[posHead["# Index Start Age"]+1])
indEndAge <- as.integer(input[posHead["# Index End Age"]+1])

#index data (year, abundance, CV, 1,2,3,4,5,6,7,8,9,Sample Size)
indDat <- input[(posHead["# Index-1 Data"]+1):(posHead["# Index-1 Data"] + nyears)]
indDat <- strsplit(indDat,"\\s+")
dfIndDat <- data.frame("Year" = as.integer(lapply(indDat,"[[",1)),
                       "Abundance" = as.numeric(lapply(indDat,"[[",2)),
                       "CV" = as.numeric(lapply(indDat,"[[",3)),
                       "Age1" = as.numeric(lapply(indDat,"[[",4)),
                       "Age2" = as.numeric(lapply(indDat,"[[",5)),
                       "Age3" = as.numeric(lapply(indDat,"[[",6)),
                       "Age4" = as.numeric(lapply(indDat,"[[",7)),
                       "Age5" = as.numeric(lapply(indDat,"[[",8)),
                       "Age6" = as.numeric(lapply(indDat,"[[",9)),
                       "Age7" = as.numeric(lapply(indDat,"[[",10)),
                       "Age8" = as.numeric(lapply(indDat,"[[",11)),
                       "Age9" = as.numeric(lapply(indDat,"[[",12)),
                       "SampleSize" = as.integer(lapply(indDat,"[[",13)))


#catches (fleet 1)
fltStartAge <- as.integer(input[posHead["# Fleet Start Age"]+1])
fltEndAge <- as.integer(input[posHead["# Fleet End Age"]+1])

#FBar age range
fBarRange <- as.integer(unlist(strsplit(input[posHead["# Age Range for Average F"]+1],"\\s+")))

flt1Catch <- input[(posHead["# Fleet-1 Catch Data"]+1):(posHead["# Fleet-1 Catch Data"] + nyears)]
flt1Catch <- strsplit(flt1Catch,"\\s+")

dfFlt1Catch <- data.frame("Year" = rep(years,9),
                          "Age" = rep(c(1:9), each = length(years)),
                          "Catch" = c(as.numeric(lapply(flt1Catch,"[[",1)),
                                      as.numeric(lapply(flt1Catch,"[[",2)),
                                      as.numeric(lapply(flt1Catch,"[[",3)),
                                      as.numeric(lapply(flt1Catch,"[[",4)),
                                      as.numeric(lapply(flt1Catch,"[[",5)),
                                      as.numeric(lapply(flt1Catch,"[[",6)),
                                      as.numeric(lapply(flt1Catch,"[[",7)),
                                      as.numeric(lapply(flt1Catch,"[[",8)),
                                      as.numeric(lapply(flt1Catch,"[[",9))))
                                      
dfFlt1Catch <- dplyr::inner_join(dfFlt1Catch, 
                                 dfFlt1Catch %>% group_by(Year) %>% summarise(AnnMean = mean(Catch)),
                                 by="Year")                                    

dfFlt1Catch$StdCatch <- dfFlt1Catch$Catch/dfFlt1Catch$AnnMean

png(filename = paste0(plots.dir,"CAA.png"),
    width = 1440, height = 800)

#bubble plot observed catches
ggplot(dfFlt1Catch, aes(x = Year, y = rev(Age))) + 
  geom_point(mapping = aes(size = StdCatch)) +
  scale_size_area(max_size = 20) +
  scale_y_discrete(labels=c(9:1)) +
  scale_x_continuous(limits = c(1958,2015), breaks = c(1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015)) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  theme(axis.text.x = element_text(size = 15, colour = 'black', angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 15, colour = 'black'),
        title = element_text(size = 20, colour = 'black'),
        axis.title.x = element_text(size = 20, colour = 'black'),
        axis.title.y = element_text(size = 20, colour = 'black', angle = 90)) +
  theme(axis.ticks.length = unit(0.3,"cm")) +
  labs(title='Mean Standardised Catch Numbers at Age', x='Year', y='Age')

dev.off()

#stock summary plot
png(filename = paste0(plots.dir,"StockSummary.png"),
    width = 840, height = 840)

layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
with(dfOut, {
     plot(Year,PredCatch/1000,type="l",main="Landings",ylab="Kt",ylim=c(0,45))
     plot(Year,SSB/1000,type="l",main="Spawning Stock Biomass",ylab="Kt",ylim=c(0,160))
     plot(Year,FBarUnweighted,type="l",main="FBar 2-5",ylab="F",ylim=c(0,0.85))
     plot(Year,ObsRec/1000,type="l",main="Recruits (age 1)",ylab="Millions",ylim=c(0,1600))}
)

dev.off()


#Observed and predicted catch
png(filename = paste0(plots.dir,"ObsAndPredCatch.png"),
    width = 840, height = 840)

with(dfOut, {
  plot(Year,PredCatch/1000,type="l",ylab="",xlab="")
  points(Year,PredCatch/1000,pch=19)
  lines(Year,ObsCatch/1000,type="l")
  points(Year,ObsCatch/1000,pch=22)
  title(main="Observed and Predicted Catch",
        cex.main = 2,
        ylab="Catch (t)",
        cex.lab = 2,
        xlab="Year")
})

dev.off()


#bubble plot index residuals
#restructure for bubble plot
dfIdxResids <- data.frame("Year" = rep(dfOut$Year,9),
                          "Age" = rep(c(1:9),each = length(dfOut$Year)),
                          "Residual" = c(dfOut$ResIdxAge1,dfOut$ResIdxAge2,dfOut$ResIdxAge3,
                                         dfOut$ResIdxAge4,dfOut$ResIdxAge5,dfOut$ResIdxAge6,
                                         dfOut$ResIdxAge7,dfOut$ResIdxAge8,dfOut$ResIdxAge9))

df2Plot <- dfIdxResids[abs(dfIdxResids$Residual)>0,]

png(filename = paste0(plots.dir,"IndexResiduals.png"),
    width = 840, height = 840)

ggplot(df2Plot, aes(x=Year, y=Age, colour=factor(sign(Residual)))) +
  geom_point(mapping = aes(size = abs(Residual))) +
  theme(legend.position = "none") +
  scale_size_area(max_size = 25) +
  scale_colour_manual(values=c("red","green")) +
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year") +
  ylab("Age") + 
  scale_x_continuous(limits = c(2002, 2015))
  
dev.off()


#bubble plot catch proportion at age residuals
#restructure a new data frame for bubble plot
dfCatchPropResids <- data.frame("Year" = rep(dfOut$Year,9),
                                "Age" = rep(c(1:9),each = length(dfOut$Year)),
                                "Residual" = c(dfOut$ResCatchPropAge1,dfOut$ResCatchPropAge2,dfOut$ResCatchPropAge3,
                                               dfOut$ResCatchPropAge4,dfOut$ResCatchPropAge5,dfOut$ResCatchPropAge6,
                                               dfOut$ResCatchPropAge7,dfOut$ResCatchPropAge8,dfOut$ResCatchPropAge9))

df2Plot <- dfCatchPropResids[abs(dfCatchPropResids$Residual)>0,]

png(filename = paste0(plots.dir,"CatchPropResiduals.png"),
    width = 840, height = 840)

ggplot(df2Plot, aes(x=Year, y=Age, colour=factor(sign(Residual)))) +
  geom_point(mapping = aes(size = abs(Residual))) +
  theme(legend.position = "none") +
  scale_size_area(max_size = 25) +
  scale_colour_manual(values=c("red","green")) +
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  xlab("Year") +
  ylab("Age")

dev.off()

#read in retros
dfOut_000 <- fReadReport(reportFile = paste0(base,"_RETRO_000.REP"))
dfOut_001 <- fReadReport(reportFile = paste0(base,"_RETRO_001.REP"))
dfOut_002 <- fReadReport(reportFile = paste0(base,"_RETRO_002.REP"))
dfOut_003 <- fReadReport(reportFile = paste0(base,"_RETRO_003.REP"))
dfOut_004 <- fReadReport(reportFile = paste0(base,"_RETRO_004.REP"))
dfOut_005 <- fReadReport(reportFile = paste0(base,"_RETRO_005.REP"))
dfOut_006 <- fReadReport(reportFile = paste0(base,"_RETRO_006.REP"))
dfOut_007 <- fReadReport(reportFile = paste0(base,"_RETRO_007.REP"))

#Mohn's rho
rho <- (1/7)*(
(1 - dfOut_001$FBarUnweighted[dfOut_001$Year==2014]/dfOut_000$FBarUnweighted[dfOut_000$Year==2014])
+
(1 - dfOut_002$FBarUnweighted[dfOut_002$Year==2013]/dfOut_000$FBarUnweighted[dfOut_000$Year==2013])
+
(1 - dfOut_003$FBarUnweighted[dfOut_003$Year==2012]/dfOut_000$FBarUnweighted[dfOut_000$Year==2012])
+
(1 - dfOut_004$FBarUnweighted[dfOut_004$Year==2011]/dfOut_000$FBarUnweighted[dfOut_000$Year==2011])
+
(1 - dfOut_005$FBarUnweighted[dfOut_005$Year==2010]/dfOut_000$FBarUnweighted[dfOut_000$Year==2010])
+
(1 - dfOut_006$FBarUnweighted[dfOut_006$Year==2009]/dfOut_000$FBarUnweighted[dfOut_000$Year==2009])
+
(1 - dfOut_007$FBarUnweighted[dfOut_007$Year==2008]/dfOut_000$FBarUnweighted[dfOut_000$Year==2008]))


#retro summary plot
png(filename = paste0(plots.dir,"RetroSummary.png"),
    width = 840, height = 840)

layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
with(dfOut_000, {
  plot(Year,PredCatch/1000,type="l",main="Landings",ylab="Kt",ylim=c(0,45))
  plot(Year,SSB/1000,type="l",main="Spawning Stock Biomass",ylab="Kt",ylim=c(0,160))
  lines(dfOut_001$Year,dfOut_001$SSB/1000)
  lines(dfOut_002$Year,dfOut_002$SSB/1000)
  lines(dfOut_003$Year,dfOut_003$SSB/1000)
  lines(dfOut_004$Year,dfOut_004$SSB/1000)
  lines(dfOut_005$Year,dfOut_005$SSB/1000)
  lines(dfOut_006$Year,dfOut_006$SSB/1000)
  plot(Year,FBarUnweighted,type="l",main="FBar 2-5",ylab="F",ylim=c(0,0.85))
  lines(dfOut_001$Year,dfOut_001$FBarUnweighted)
  lines(dfOut_002$Year,dfOut_002$FBarUnweighted)
  lines(dfOut_003$Year,dfOut_003$FBarUnweighted)
  lines(dfOut_004$Year,dfOut_004$FBarUnweighted)
  lines(dfOut_005$Year,dfOut_005$FBarUnweighted)
  lines(dfOut_006$Year,dfOut_006$FBarUnweighted)
  plot(Year,ObsRec/1000,type="l",main="Recruits (age 1)",ylab="Millions",ylim=c(0,1600))
  lines(dfOut_001$Year,dfOut_001$ObsRec/1000)
  lines(dfOut_002$Year,dfOut_002$ObsRec/1000)
  lines(dfOut_003$Year,dfOut_003$ObsRec/1000)
  lines(dfOut_004$Year,dfOut_004$ObsRec/1000)
  lines(dfOut_005$Year,dfOut_005$ObsRec/1000)
  lines(dfOut_006$Year,dfOut_006$ObsRec/1000)}
)

dev.off()

#focus on fishing mortality retro
plot(dfOut_000$Year,dfOut_000$FBarUnweighted,type="l",main="FBar 2-5",ylab="F",ylim=c(0,0.25),xlim=c(2008,2015),col="red",lwd=2)
lines(dfOut_001$Year,dfOut_001$FBarUnweighted)
lines(dfOut_002$Year,dfOut_002$FBarUnweighted)
lines(dfOut_003$Year,dfOut_003$FBarUnweighted)
lines(dfOut_004$Year,dfOut_004$FBarUnweighted)
lines(dfOut_005$Year,dfOut_005$FBarUnweighted)
lines(dfOut_006$Year,dfOut_006$FBarUnweighted)
lines(dfOut_007$Year,dfOut_007$FBarUnweighted)

#write file for input to Julios seg reg algorithm

#data, trim first two recruit values to offset by 2 years and pad with NAs on end
data <- data.frame(year = dfOut_000$Year,
                   SSB = dfOut_000$SSB,
                   R = c(dfOut_000$Age1[-c(1,2)],NA,NA))

write.table(data[complete.cases(data),], file = "SegReg_000.csv", row.names=FALSE, sep="\t", quote = FALSE)

#data, trim first two recruit values to offset by 2 years and pad with NAs on end
data <- data.frame(year = dfOut_001$Year,
                   SSB = dfOut_001$SSB,
                   R = c(dfOut_001$Age1[-c(1,2)],NA,NA))

write.table(data[complete.cases(data),], file = "SegReg_001.csv", row.names=FALSE, sep="\t", quote = FALSE)

#data, trim first two recruit values to offset by 2 years and pad with NAs on end
data <- data.frame(year = dfOut_002$Year,
                   SSB = dfOut_002$SSB,
                   R = c(dfOut_002$Age1[-c(1,2)],NA,NA))

write.table(data[complete.cases(data),], file = "SegReg_002.csv", row.names=FALSE, sep="\t", quote = FALSE)

#data, trim first two recruit values to offset by 2 years and pad with NAs on end
data <- data.frame(year = dfOut_003$Year,
                   SSB = dfOut_003$SSB,
                   R = c(dfOut_003$Age1[-c(1,2)],NA,NA))

write.table(data[complete.cases(data),], file = "SegReg_003.csv", row.names=FALSE, sep="\t", quote = FALSE)

#data, trim first two recruit values to offset by 2 years and pad with NAs on end
data <- data.frame(year = dfOut_004$Year,
                   SSB = dfOut_004$SSB,
                   R = c(dfOut_004$Age1[-c(1,2)],NA,NA))

write.table(data[complete.cases(data),], file = "SegReg_004.csv", row.names=FALSE, sep="\t", quote = FALSE)

#data, trim first two recruit values to offset by 2 years and pad with NAs on end
data <- data.frame(year = dfOut_005$Year,
                   SSB = dfOut_005$SSB,
                   R = c(dfOut_005$Age1[-c(1,2)],NA,NA))

write.table(data[complete.cases(data),], file = "SegReg_005.csv", row.names=FALSE, sep="\t", quote = FALSE)

#data, trim first two recruit values to offset by 2 years and pad with NAs on end
data <- data.frame(year = dfOut_006$Year,
                   SSB = dfOut_006$SSB,
                   R = c(dfOut_006$Age1[-c(1,2)],NA,NA))

write.table(data[complete.cases(data),], file = "SegReg_006.csv", row.names=FALSE, sep="\t", quote = FALSE)

#data, trim first two recruit values to offset by 2 years and pad with NAs on end
data <- data.frame(year = dfOut_007$Year,
                   SSB = dfOut_007$SSB,
                   R = c(dfOut_007$Age1[-c(1,2)],NA,NA))

write.table(data[complete.cases(data),], file = "SegReg_007.csv", row.names=FALSE, sep="\t", quote = FALSE)


#write stock numbers, fishing mortalities at age

#dfOut_000
data <- data.frame(Age1 = dfOut_000$Age1, Age2 = dfOut_000$Age2, Age3 = dfOut_000$Age3,
                   Age4 = dfOut_000$Age4, Age5 = dfOut_000$Age5, Age6 = dfOut_000$Age6,
                   Age7 = dfOut_000$Age7, Age8 = dfOut_000$Age8, Age9 = dfOut_000$Age9)

header <- c("Celtic Sea Herring stock numbers at age - stock.n (units : NA )",
            "1   13",
            "1958   2015",
            "1   9",
            "1")

write.table(header, file = "N_000.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "N_000.csv", row.names=FALSE, col.names=FALSE, append=TRUE)

data <- data.frame(Age1 = dfOut_000$F1, Age2 = dfOut_000$F2, Age3 = dfOut_000$F3,
                   Age4 = dfOut_000$F4, Age5 = dfOut_000$F5, Age6 = dfOut_000$F6,
                   Age7 = dfOut_000$F7, Age8 = dfOut_000$F8, Age9 = dfOut_000$F9)

header <- c("Celtic Sea Herring fishing mortality - harvest (units : f )",
            "1   13",
            "1958   2015",
            "1   9",
            "1")

write.table(header, file = "F_000.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "F_000.csv", row.names=FALSE, col.names=FALSE, append=TRUE)

#dfOut_001
data <- data.frame(Age1 = dfOut_001$Age1, Age2 = dfOut_001$Age2, Age3 = dfOut_001$Age3,
                   Age4 = dfOut_001$Age4, Age5 = dfOut_001$Age5, Age6 = dfOut_001$Age6,
                   Age7 = dfOut_001$Age7, Age8 = dfOut_001$Age8, Age9 = dfOut_001$Age9)

header <- c("Celtic Sea Herring stock numbers at age - stock.n (units : NA )",
            "1   13",
            "1958   2014",
            "1   9",
            "1")

write.table(header, file = "N_001.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "N_001.csv", row.names=FALSE, col.names=FALSE, append=TRUE)


data <- data.frame(Age1 = dfOut_001$F1, Age2 = dfOut_001$F2, Age3 = dfOut_001$F3,
                   Age4 = dfOut_001$F4, Age5 = dfOut_001$F5, Age6 = dfOut_001$F6,
                   Age7 = dfOut_001$F7, Age8 = dfOut_001$F8, Age9 = dfOut_001$F9)

header <- c("Celtic Sea Herring fishing mortality - harvest (units : f )",
            "1   13",
            "1958   2014",
            "1   9",
            "1")

write.table(header, file = "F_001.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "F_001.csv", row.names=FALSE, col.names=FALSE, append=TRUE)



#dfOut_002
data <- data.frame(Age1 = dfOut_002$Age1, Age2 = dfOut_002$Age2, Age3 = dfOut_002$Age3,
                   Age4 = dfOut_002$Age4, Age5 = dfOut_002$Age5, Age6 = dfOut_002$Age6,
                   Age7 = dfOut_002$Age7, Age8 = dfOut_002$Age8, Age9 = dfOut_002$Age9)

header <- c("Celtic Sea Herring stock numbers at age - stock.n (units : NA )",
            "1   13",
            "1958   2013",
            "1   9",
            "1")

write.table(header, file = "N_002.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "N_002.csv", row.names=FALSE, col.names=FALSE, append=TRUE)



data <- data.frame(Age1 = dfOut_002$F1, Age2 = dfOut_002$F2, Age3 = dfOut_002$F3,
                   Age4 = dfOut_002$F4, Age5 = dfOut_002$F5, Age6 = dfOut_002$F6,
                   Age7 = dfOut_002$F7, Age8 = dfOut_002$F8, Age9 = dfOut_002$F9)

header <- c("Celtic Sea Herring fishing mortality - harvest (units : f )",
            "1   13",
            "1958   2013",
            "1   9",
            "1")

write.table(header, file = "F_002.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "F_002.csv", row.names=FALSE, col.names=FALSE, append=TRUE)


#dfOut_003
data <- data.frame(Age1 = dfOut_003$Age1, Age2 = dfOut_003$Age2, Age3 = dfOut_003$Age3,
                   Age4 = dfOut_003$Age4, Age5 = dfOut_003$Age5, Age6 = dfOut_003$Age6,
                   Age7 = dfOut_003$Age7, Age8 = dfOut_003$Age8, Age9 = dfOut_003$Age9)

header <- c("Celtic Sea Herring stock numbers at age - stock.n (units : NA )",
            "1   13",
            "1958   2012",
            "1   9",
            "1")

write.table(header, file = "N_003.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "N_003.csv", row.names=FALSE, col.names=FALSE, append=TRUE)


data <- data.frame(Age1 = dfOut_003$F1, Age2 = dfOut_003$F2, Age3 = dfOut_003$F3,
                   Age4 = dfOut_003$F4, Age5 = dfOut_003$F5, Age6 = dfOut_003$F6,
                   Age7 = dfOut_003$F7, Age8 = dfOut_003$F8, Age9 = dfOut_003$F9)

header <- c("Celtic Sea Herring fishing mortality - harvest (units : f )",
            "1   13",
            "1958   2012",
            "1   9",
            "1")

write.table(header, file = "F_003.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "F_003.csv", row.names=FALSE, col.names=FALSE, append=TRUE)


#dfOut_004
data <- data.frame(Age1 = dfOut_004$Age1, Age2 = dfOut_004$Age2, Age3 = dfOut_004$Age3,
                   Age4 = dfOut_004$Age4, Age5 = dfOut_004$Age5, Age6 = dfOut_004$Age6,
                   Age7 = dfOut_004$Age7, Age8 = dfOut_004$Age8, Age9 = dfOut_004$Age9)

header <- c("Celtic Sea Herring stock numbers at age - stock.n (units : NA )",
            "1   13",
            "1958   2011",
            "1   9",
            "1")

write.table(header, file = "N_004.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "N_004.csv", row.names=FALSE, col.names=FALSE, append=TRUE)



data <- data.frame(Age1 = dfOut_004$F1, Age2 = dfOut_004$F2, Age3 = dfOut_004$F3,
                   Age4 = dfOut_004$F4, Age5 = dfOut_004$F5, Age6 = dfOut_004$F6,
                   Age7 = dfOut_004$F7, Age8 = dfOut_004$F8, Age9 = dfOut_004$F9)

header <- c("Celtic Sea Herring fishing mortality - harvest (units : f )",
            "1   13",
            "1958   2011",
            "1   9",
            "1")

write.table(header, file = "F_004.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "F_004.csv", row.names=FALSE, col.names=FALSE, append=TRUE)

#dfOut_005
data <- data.frame(Age1 = dfOut_005$Age1, Age2 = dfOut_005$Age2, Age3 = dfOut_005$Age3,
                   Age4 = dfOut_005$Age4, Age5 = dfOut_005$Age5, Age6 = dfOut_005$Age6,
                   Age7 = dfOut_005$Age7, Age8 = dfOut_005$Age8, Age9 = dfOut_005$Age9)

header <- c("Celtic Sea Herring stock numbers at age - stock.n (units : NA )",
            "1   13",
            "1958   2010",
            "1   9",
            "1")

write.table(header, file = "N_005.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "N_005.csv", row.names=FALSE, col.names=FALSE, append=TRUE)


data <- data.frame(Age1 = dfOut_005$F1, Age2 = dfOut_005$F2, Age3 = dfOut_005$F3,
                   Age4 = dfOut_005$F4, Age5 = dfOut_005$F5, Age6 = dfOut_005$F6,
                   Age7 = dfOut_005$F7, Age8 = dfOut_005$F8, Age9 = dfOut_005$F9)

header <- c("Celtic Sea Herring fishing mortality - harvest (units : f )",
            "1   13",
            "1958   2010",
            "1   9",
            "1")

write.table(header, file = "F_005.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "F_005.csv", row.names=FALSE, col.names=FALSE, append=TRUE)


#dfOut_006
data <- data.frame(Age1 = dfOut_006$Age1, Age2 = dfOut_006$Age2, Age3 = dfOut_006$Age3,
                   Age4 = dfOut_006$Age4, Age5 = dfOut_006$Age5, Age6 = dfOut_006$Age6,
                   Age7 = dfOut_006$Age7, Age8 = dfOut_006$Age8, Age9 = dfOut_006$Age9)

header <- c("Celtic Sea Herring stock numbers at age - stock.n (units : NA )",
            "1   13",
            "1958   2009",
            "1   9",
            "1")

write.table(header, file = "N_006.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "N_006.csv", row.names=FALSE, col.names=FALSE, append=TRUE)

data <- data.frame(Age1 = dfOut_006$F1, Age2 = dfOut_006$F2, Age3 = dfOut_006$F3,
                   Age4 = dfOut_006$F4, Age5 = dfOut_006$F5, Age6 = dfOut_006$F6,
                   Age7 = dfOut_006$F7, Age8 = dfOut_006$F8, Age9 = dfOut_006$F9)

header <- c("Celtic Sea Herring fishing mortality - harvest (units : f )",
            "1   13",
            "1958   2009",
            "1   9",
            "1")

write.table(header, file = "F_006.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "F_006.csv", row.names=FALSE, col.names=FALSE, append=TRUE)


#dfOut_007
data <- data.frame(Age1 = dfOut_007$Age1, Age2 = dfOut_007$Age2, Age3 = dfOut_007$Age3,
                   Age4 = dfOut_007$Age4, Age5 = dfOut_007$Age5, Age6 = dfOut_007$Age6,
                   Age7 = dfOut_007$Age7, Age8 = dfOut_007$Age8, Age9 = dfOut_007$Age9)

header <- c("Celtic Sea Herring stock numbers at age - stock.n (units : NA )",
            "1   13",
            "1958   2008",
            "1   9",
            "1")

write.table(header, file = "N_007.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "N_007.csv", row.names=FALSE, col.names=FALSE, append=TRUE)


data <- data.frame(Age1 = dfOut_007$F1, Age2 = dfOut_007$F2, Age3 = dfOut_007$F3,
                   Age4 = dfOut_007$F4, Age5 = dfOut_007$F5, Age6 = dfOut_007$F6,
                   Age7 = dfOut_007$F7, Age8 = dfOut_007$F8, Age9 = dfOut_007$F9)

header <- c("Celtic Sea Herring fishing mortality - harvest (units : f )",
            "1   13",
            "1958   2008",
            "1   9",
            "1")

write.table(header, file = "F_007.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(data, file = "F_007.csv", row.names=FALSE, col.names=FALSE, append=TRUE)

