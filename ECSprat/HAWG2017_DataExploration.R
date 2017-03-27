# Analyse data from Sprat

# created: 14 March 2017
# modified
# author: Piera
# purpose: analyses data for HAWG 2017

# notes: 

# load libraries
library(ggplot2)
library(data.table)
library(reshape2)
library(gsubfn)

##########
### PELTIC
## Get data from hauls for length and weight
# For the Numbers: get the column "acN", NOT the "actualN", because the "acN" are the numbers from the acoustic 
# (that is what you want), while the "actualN" are the number from the trawl.
# Extract
df <- data.table(read.csv("PELTIC/PELTIC13-16_bio.csv"))
# summary
summary(df)
# adjust "ices" column <- should be the rectangle
df$ices <- format(df$ices, scientific = TRUE)
df$ices1 <- gsub("[[:punct:]0]", "", df$ices)
# get spr
# subarea = NA: those points are there because there are data, however what happens is that for every point along 
# the transect has the species composition based on data trawl, but it might happen that the points in that 
# rectangle are really few and therefore not enough to be rised. So no need to consider those.
dfSpr <- droplevels(df[NewSpecies=="SPR" & !is.na(subarea),])
# aggregate by year 
dfSpr.y <- dfSpr[, list(totN=sum(acN)), by=c("subarea", "year", "Lengthcat")]

# rectangles 30E6 - 29E6
LymeBay <- dfSpr[ices1=="3e6" | ices1=="29e6"]
# plot it
# options(scipen=999)
png("Plots/PELTIC_LFD_by_subarea.png", width=2000, height=1000, res=200)
ggplot(dfSpr.y, aes(Lengthcat, totN/1000, fill=subarea)) + geom_bar(position="stack", stat="identity") +
  xlab("LT (cm)") + ylab("Numbers ('000)") +
  theme_light(base_size = 15) +
  facet_grid(subarea~year, scales = "free_y") +
  scale_x_continuous(breaks=seq(4,16,2)) +
  theme(legend.position="NULL")
dev.off()


##########
# 2013=75546, 2014=77800, 2015=60011
### PELTIC
# Get biomass
# Extract data
dfB <- data.table(read.csv("PELTIC/PELTIC13-16_biomass_2016.txt"))
# summary
summary(dfB)
# get spr only
dfB.spr <- dfB[NewSpecies=="SPR",]

# plot 
png("Plots/PELTIC_TotBio_by_subarea.png", width=1400, height=1200, res=300)
ggplot(dfB.spr, aes(year, biomass, colour=subarea)) + geom_line(size=1) + geom_point(size=1.5) +
  theme_light(base_size=10) +
  xlab("Year") + ylab("Biomass") + 
  scale_y_continuous(breaks=seq(0, 150000, 10000)) +
  theme(legend.key = element_blank(), legend.title= element_blank(), legend.position=c(0.9,0.9))
dev.off()


# Sprat 3a approach to estimate ref points (Bescapement)
dfB.sprEc <- dfB.spr[subarea=="ec",]
Bmax <- max(dfB.sprEc[,biomass])
Bmsy <- Bmax/2
refPnt <- (Bmsy/2)*1.2

# plot only ec
ggplot(dfB.spr[subarea=="ec",], aes(year, biomass, colour=subarea)) + geom_line(size=1) + geom_point(size=1.5) +
  theme_light(base_size=20) +
  geom_hline(yintercept = refPnt, col = "blue", size = 2) +
  xlab("Year") + ylab("Biomass") + 
  scale_y_continuous(breaks=seq(0, 150000, 10000)) +
  theme(legend.position = "none")

# Extract data
dfBices16 <- data.table(read.csv("PELTIC/PELTIC16_biomass_ices.csv"))
dfBices15 <- data.table(read.csv("PELTIC/2015_SPR_ices (old).txt"))
dfBices14 <- data.table(read.csv("PELTIC/2014_SPR_ices (old).txt"))
dfBices13 <- data.table(read.csv("PELTIC/2013_SPR_ices (old).txt"))


dfBices <- cbind(rbind(dfBices15,dfBices14,dfBices13),year=c(rep(2015, 22),rep(2014, 22),rep(2013, 23)))
# subset only 29E6 and 30E6
dfBices1315 <-dfBices[i==29e+06 | i==30e+06, .(year, SPR_biomass)] 
dfBicesAll <- rbind(cbind(year=rep(2016,2), dfBices16[i=="29E6" | i=="30E6",.(SPR_biomass)]),dfBices1315)
LymeBay <- dfBicesAll[,sum(SPR_biomass), by=year]
LymeBay <- LymeBay[order(year),] 


LB11_29e6 <- 18930
LB11_30e6 <- 3017

LB12_29e6N <- 10573
LB12_29e6S <- 6683
LB12_30e6S <- 1790

LB12 <- cbind(year=2012, biomass=LB12_29e6N+LB12_29e6S+LB12_30e6S)
LB11 <- cbind(2011, biomass=LB11_29e6+LB11_30e6)
# Join the two
dfB.sprEc$LymeBay <- LymeBay[,V1]
dfB.sprEc$perc <- dfB.sprEc$LymeBay/dfB.sprEc$biomass
ggplot(dfB.sprEc, aes(x=year, y=perc)) + geom_line() 

################
#### Check landings: 
dfLand <- data.table(read.csv("Landings/Sprat_Landings.csv"))
# melt
dfLandR <- melt(dfLand, id.vars="Year")

# plot 
png("Plots/LANDINGS_Tons.png", width=1400, height=1200, res=300)
ggplot(dfLandR[variable!="OfficialLandings"], aes(Year, value*1000, colour=variable)) + geom_line(size=1) + geom_point(size=1.5) +
  theme_light(base_size=10) +
  xlab("Year") + ylab("Tons") + 
  scale_y_continuous(breaks=seq(0, 15000, 1000)) +
  scale_x_continuous(breaks=seq(2000, 2016, 2)) +
  theme(legend.key = element_blank(), legend.title= element_blank(), legend.position=c(0.9,0.9))
dev.off()

# Harvest rate
land1316 <- c(3793,3658,3012,3339)
bio1316 <- dfB.spr[subarea=="ec" ,.(year,biomass)]
HR <- data.frame(cbind(year=2013:2016,HR=land1316/bio1316[,biomass]))

library(scales)
ggplot(HR, aes(year,HR)) + geom_line(size=2)+ 
  scale_y_continuous(labels = comma) +
  theme_light(base_size = 20) + ylab("%") +
  ggtitle("Harvest rate")

