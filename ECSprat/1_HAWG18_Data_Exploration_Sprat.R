#### 
# Sprat data

# Load libraries
library(data.table)
library(ggplot2)
library(xlsx)

# =======================================
############ ACOUSTIC DATA ##############
# =======================================
# Directory
inpDir <- file.path(getwd(), "Sprat_7de", "Data", "PELTIC/")
outDir <- file.path(getwd(), "Sprat_7de", "Plots")

# Load data   .sp.MethEsdu
# mypath
mypath = inpDir

# WE HAVE TWO DIFFERENT STRATIFICATIONS: THE FIRST (STR1) ONE DOESN'T JOIN AREAS, WHICH MEANS THAT
# IF THERE ARE NO TRAWL IN ONE AREA, BUT YES ACOUSTIC MARK, THE ACOUSTIC BIOMASS WILL BE LOST.
# THE SECOND STRATIFICATION (STR2) MERGE PROXIMAL REGION IF NO TRAWL IN ONE BUT YES ACOUSTIC
# SIGNAL. 

# ===================================
# STRATIFICATION 1 & STRATIFICATION 2
# ===================================
# List all files with biomass by region
sources.files <- list.files(path=mypath,
                               recursive=T,
                               pattern='Biom.region.sp.df.csv',
                               include.dirs=T,
                               full.names=T)

# create a list of files
myfiles <- list()
# and read all the csv into it
for(i in sources.files){
  myfiles[[i]] <- read.csv(i, sep=";", head=T)
}
# change names assigning different name if new strata (STR2) or old strata (STR1)
names(myfiles) <- ifelse(nchar(names(myfiles))==110, paste("Areas", "_", substr(names(myfiles), 66, 69), sep=""),
       paste("NewStrata", "_", substr(names(myfiles), 66, 69), sep=""))
# Add column to each element in the list with the type of data (STR1 or STR2)
myfiles <- mapply(cbind, myfiles, "type"=names(myfiles), SIMPLIFY=F)
# Convert into a data.table
dat <- data.table(do.call(rbind, myfiles))


# Add year variable
dat[,c("year", "region") := list(as.numeric(substr(CAMPAGNE, 7, 11)), substr(Region,6,9))]
# Select SPRAT and only regions of interest (to have a complete time series we have to use only region from 4 to 7).
HAWG_region <- c("4","5","6","7", "43", "47", "4A", "4C", "54", "56", "57", "65", "67") 
eco_spr <- dat[GENR_ESP=="SPRA-SPR" & region %in% HAWG_region,] 
# Try adding also region 8 and see how much differ
HAWG_region_w8 <- c("4","5","6","7", "8") 
eco_spr_reg8 <- dat[GENR_ESP=="SPRA-SPR" & region %in% HAWG_region_w8,] 

# Sum of column wbiom
PEL17 <- eco_spr[,sum(wbiom), by=c("year", "type")]
totEC_w8 <- eco_spr_reg8[,sum(wbiom), by="year"]
# Region 8 is really negligeble so not a problem if it's removed. 

# rename the type removing the year
PEL17[,type:=gsub("[^a-zA-Z]", "", type)]
# compare biomass with last year estimates
PEL16 <- data.table(read.table("C:/Users/PC09/Documents/Work/WGs/2017/HAWG/Sprat/PELTIC/PELTIC13-16_biomass_2016.txt", sep=",", head=T))
PEL16ec <- PEL16[subarea=="ec" & NewSpecies=="SPR",]
names(PEL17)[3] <- "biomass"
PEL17[,source:="EcoR_17"]
PEL16ec[,c("type", "source"):=list("old_Method", "Est_16")]
# Join dataset
PEL <- rbind(PEL16ec[,.(year, type, biomass, source)], PEL17)
# Plot
P_BIO <- ggplot(PEL, aes(year, biomass, col=type)) + geom_line(size=1.2) + 
  theme_bw(18) + ylab("Biomass (t)") + xlab("Year") +
  scale_y_continuous(breaks=seq(0, 100000, 10000)) +
  theme(legend.position = c(0.8,0.8), legend.title = element_blank()) 

png(file.path(outDir, "PEL_BIO_Comp_1617.png"), width = 2500, height=2500, res=280)
P_BIO
dev.off()

## the sum of biomass by region differs from the total biomass estimation because if there is a trawl missing in one region, this 
## region will not enter in the estimates by region even if there is biomass saw by ecosounder. 
## Also, the total biomass estimates provided by EchoR are brute (not stratified). 
## 1) Let's first check how different are these values. 2) Then we could look at how many regions are there and if all but the ones I'm
## interested in (i.e. the english channel) are there, then I could just subtract all the non-channel to the overall. 3) Otherwise, I can
## ask Silvia to re-estimate the biomass by region assigning the biomass where there are no trawls to another region with trawl (merge
## for example area 1a and 1b).

## 1) Check differences between regional biomass and total biomass
# List files
sources.files.b  <- list.files(path=mypath,
                             recursive=T,
                             pattern='biomtot.sp.MethEsdu.csv',
                             full.names=T)
# Load total biomass
dat_tot <- data.table(do.call(rbind,lapply(sources.files.b, read.csv, sep=";")))
# sprat - column to consider are "biomW.esdu" and "biomN.esdu"
dat_tot[,year:=as.numeric(substr(CAMPAGNE, 7,12))]
dat_tot_spr <- dat_tot[sp=="SPRA-SPR", .(sp, biomW.esdu, biomN.esdu, year)]
dat_tot_spr[,type:="EchoR_RawBio"]
# Sum up region by year 
dat_reg_spr <- dat[GENR_ESP=="SPRA-SPR",sum(wbiom), by=c("year", "type")]
# rename the type removing the year
dat_reg_spr[,type:=gsub("[^a-zA-Z]", "", type)]
# Calculate difference between the two now, i.e. between "dat_reg_spr" and "dat_tot_spr"
# change names and remove cols:
names(dat_reg_spr)[3] <- "biomW.esdu"
dat_tot_spr[,c("sp", "biomN.esdu"):=NULL]
# rbind in one:
DAT_SPR <- rbind(dat_tot_spr[,.(year, type, biomW.esdu)], dat_reg_spr)
# difference
DAT_SPR[,diff:=1-(REG_BIO/TOT_BIO)]
# The reason for 2015 and 2017 being so different (with regional biomass much higher) is because EchoR estimate the overall biomass 
# on the whole data, without stratification. 

P_BIO_TOT <- ggplot(DAT_SPR, aes(year, biomW.esdu, col=type)) + geom_line(size=1.2) + 
  theme_bw(18) + ylab("Biomass (t)") + xlab("Year") +
  scale_y_continuous(breaks=seq(0, 1000000, 100000)) +
  theme(legend.position = c(0.8,0.8), legend.title = element_blank()) 

png(file.path(outDir, "PEL_BIO_TOT_Comp_1617.png"), width = 2500, height=2500, res=280)
P_BIO_TOT
dev.off()

###################
## AREA 6 (LYME BAY)
# Get CV
sources.files.c  <- list.files(path=mypath,
                               recursive=T,
                               pattern='CVspeciesRegion.csv',
                               full.names=T)

# Load CV by region 
dat_CV <- list()
for(i in 1:length(sources.files.c)){
  dat_CV[[i]] <- read.csv(sources.files.c[i], sep=";")
  dat_CV[[i]]$year <- as.numeric(substr(sources.files.c[i], 66,69))
}
# rbind into one
dat_CV <- data.table(do.call(rbind,dat_CV))
# We interest in columns CVi (error of the trawl) and CVs (spatial CV). However, error of the trawl is tricky becuase we changed the 
# trawl to be considered monospecific, so let's not use it now. 
dat_CVs <- dat_CV[, region:=substr(region,6,8)]
# Get CV for sprat in EnglishChannel
dat_CVs_EC <- dat_CVs[sp=="SPRA-SPR" & region %in% HAWG_region,.(sp, region, year, CVs)] #"6"
# Get CV for sprat in area 6
dat_CVs_6 <- dat_CVs[sp=="SPRA-SPR" & region== 6,.(sp, region, year, CVs)] #"6"
# Sprat biomass region 6
eco_spr <- dat[GENR_ESP=="SPRA-SPR" & region==6,] 
# merge with biomass estimations
BIO_CV <- merge(dat_CVs_6, eco_spr[,.(year, region, wbiom)], by=c("year", "region"))
# Calculate SD = CV * mean, then Confidence intervals = +- 1.96*SD
# SD 
BIO_CV[,SD:=CVs*wbiom]
# CI
BIO_CV[, c("UCI","LCI") := list(wbiom+(1.96*SD),wbiom-(1.96*SD))]
# Now plot region 6 with CV and add also line for English Channel
BIO_CV_a <- BIO_CV[,.(year, region, wbiom, UCI, LCI)]
names(BIO_CV_a)[3] <- "biomass" 
EnglishChannel[,c("region", "UCI", "LCI") := list("EC", biomass, biomass)]
BIO_CV_All <- rbind(BIO_CV_a, EnglishChannel[,.(year, region, biomass, UCI, LCI)])
BIO_CV_All$region <- ifelse(BIO_CV_All$region=="6", "LymeBay", "7e")
# PLOT
BIO_wCV <- ggplot(BIO_CV_All) + geom_line(aes(year, biomass, col=region), size=1) + 
  geom_line(aes(year, UCI, col=region), linetype=2, size=1) + geom_line(aes(year, LCI, col=region), linetype=2, size=1) +
  theme_bw(18) + ylab("Biomass (tonnes)") + xlab("Year") + 
  scale_y_continuous(breaks=seq(0, 90000, 10000)) +
  theme(legend.position=c(0.8, 0.8), legend.title=element_blank(), legend.key=element_blank())

png(file=file.path(outDir, "PELTIC_BIO_wCV.png"), res=300, width=3500, height=2500)
BIO_wCV
dev.off()
#####################################
### LENGTH & MATURITY DATA FROM PELTIC 2013-2017
# Read file 
require(scales)
PEL_BIO <- data.table(read.csv(file.path(inpDir, "PELTIC_2017-2013_SPR_BIO_DATA.csv"), sep=",", head=T))
summary(PEL_BIO)

##########
# MATURITY
##########
MAT <- PEL_BIO[Area=="WEC" & fldFishMaturity !="U" & fldFishSex != "U",]
# Add column for mature/immature
MAT$maturity <- ifelse(MAT$fldFishMaturity %in% c("1","2"), "Imm", "Mat")
# Percentage mature/immature by year
MAT_PERC <- setDT(MAT)[order(maturity), .(event = unique(maturity), percentage = tabulate(factor(maturity))/.N), by = Year]
dcast(MAT_PERC, Year~event)

table(MAT$Year, MAT$fldFishLength, MAT$fldFishMaturity)
PEL_AgeTL <- data.table(table(PEL_BIO[Area=="WEC", Year], PEL_BIO[Area=="WEC", fldFishLength], PEL_BIO[Area=="WEC", fldResult1]))
names(PEL_AgeTL) <- c("year", "TL", "age", "count")
# remove 99 from age and convert TL into numeric
PEL_AgeTL <- PEL_AgeTL[age!="99",]
PEL_AgeTL[,TL:=as.numeric(as.character(TL))]

##########
# SEX RATIO
##########
data.table(table(MAT$Year, MAT$fldFishSex))
PEL_SR <- setDT(MAT)[order(fldFishSex), .(event = unique(fldFishSex), percentage = tabulate(factor(fldFishSex))/.N), by = Year]
dcast(PEL_SR, Year~event)
PEL_SR[,mean(percentage), by=event]
##########
# AGE CLASSES
##########
# Check percentage by age
# Get only WEC
AGE <- PEL_BIO[Area=="WEC" & fldResult1 !="99",]
AGE_PERC <- setDT(AGE)[order(fldResult1), .(event = unique(fldResult1), percentage = tabulate(factor(fldResult1))/.N), by = Year]
AGE_PERCr <- dcast(AGE_PERC, Year~event)
colMeans(AGE_PERCr, na.rm = TRUE)
# PLOT NUMBERS AT AGE IN SAMPLES BY YEAR
P_LT_AGE <- ggplot(PEL_AgeTL[year=="2017" & count!=0,], aes(TL/10, count, fill=age)) + geom_bar(position="fill", stat="identity") +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) + 
  scale_x_continuous(breaks=seq(8,14,1)) + #expand = c(0,0), 
  theme_bw(16) + xlab("TL (cm)") + ylab("%") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(title="Age class", nrow=1))

png(file=file.path(outDir, "PELTIC_TL_AGE_2017.png"), res=300, width=3500, height=2500)
P_LT_AGE
dev.off()



#####################################
#####################################
## PLOT FOR PRESENTATION
## 1. Trends Overall biomass (from sum of all regions/areas), English Channel (areas 
## "4","5","6","7", "43", "47", "4A", "4C", "54", "56", "57", "65", "67") and Lyme bay (area 6)
OverallBio <- DAT_SPR[type=="NewStrata", ]
OverallBio <- rbind(OverallBio, DAT_SPR[type=="Areas" & year == 2017,])
OverallBio <- OverallBio[,area:="ALL_STRATA"]
EnglishChannel <- PEL17[type=="NewStrata"]
EnglishChannel <- rbind(EnglishChannel, PEL17[type=="Areas" & year == 2017,])
EnglishChannel <- EnglishChannel[,c("source", "area") := list(NULL, "ENGLISH_CHANNEL")]
LymeBay <- eco_spr[substr(type, 1,5)=="Areas" & region==6, ]
LymeBay <- LymeBay[,.(year, type, wbiom)]
LymeBay[,c("type", "area") := list(substr(type, 1, 5), "LYME_BAY")]
# uniform names
names(OverallBio) <- names(EnglishChannel)
names(LymeBay) <- names(EnglishChannel)
# rbind
TIME_SERIES_PEL <- rbind(OverallBio, EnglishChannel, LymeBay)
# Plot
P_TimeSeries <- ggplot(TIME_SERIES_PEL, aes(year, biomass, col=area)) + geom_line(size=1.2) + 
  theme_bw(18) + ylab("Biomass (t)") + xlab("Year") +
  scale_y_continuous(breaks=seq(0, 1000000, 50000)) +
  theme(legend.position = c(0.8,0.8), legend.title = element_blank()) 

png(file.path(outDir, "PEL_TimeSeries.png"), width = 3000, height=2500, res=280)
P_TimeSeries
dev.off()

###
## PAST AND PREVIOUS ESTIMATES ENGLISH CHANNEL
PEL16ec[,c("NewSpecies", "source", "subarea", "type") := list(NULL, NULL, NULL, "2016_Estimates")]
# Create column for area and re-order columns
PEL16ec[,area:="ENGLISH_CHANNEL"]
PEL16ec <- PEL16ec[,c(names(EnglishChannel)), with=FALSE]
# Re-define type
EnglishChannel[,type:="2017_Estimates_EchoR"]
# rbind
EC_Comp <- rbind(PEL16ec, EnglishChannel)
# Plot
P_EC_COMP <- ggplot(EC_Comp, aes(year, biomass, col=type)) + geom_line(size=1.2) + 
  theme_bw(18) + ylab("Biomass (t)") + xlab("Year") +
  scale_y_continuous(breaks=seq(0, 100000, 10000)) +
  theme(legend.position = c(0.8,0.8), legend.title = element_blank()) 

png(file.path(outDir, "PEL_EC_COMP.png"), width = 3000, height=2500, res=280)
P_EC_COMP
dev.off()


### ====================================================================
### LANDINGS 
### ====================================================================
# Directories
inpDir_LAN <- file.path(getwd(), "Sprat_7de", "Data", "Catch/")
outDir_LAN <- file.path(getwd(), "Sprat_7de", "Plots")
# Load data extracted from intercatch
LAN <- data.table(read.xlsx(file.path(inpDir_LAN, "Intercatch_TotalCatch_2018.xlsx"), sheetIndex = 1))
# total by country/season
LAN_a <- LAN[,sum(Catch..kg), by=c("Country", "Season")]
# Plot
P1_LAN <- ggplot(LAN_a, aes(Country, V1/1000, fill=factor(Season))) + geom_bar(position="dodge", stat="identity") +
  theme_bw(18) + ylab("Landings (t)") + scale_y_continuous(breaks=seq(0,1500,200)) +
  theme(legend.position = c(0.2,0.8)) +
  guides(fill=guide_legend(title="Quarter"))#

# total by country/fleet
LAN_b <- LAN[,sum(Catch..kg), by=c("Country", "Fleets")]
# plot
P2_LAN <- ggplot(LAN_b, aes(Fleets, V1/1000, fill=factor(Country))) + geom_bar(position="dodge", stat="identity") +
  theme_bw(18) + ylab("Landings (t)") + scale_y_continuous(breaks=seq(0,3000,200)) +
  theme(legend.position = c(0.8,0.8)) +
  guides(fill=guide_legend(title="Country"))#


png(file.path(outDir_LAN, "SPR_Landings_CountrySeason.png"), width=2500, height=2500, res=300)
P1_LAN
dev.off()

png(file.path(outDir_LAN, "SPR_Landings_CountryFleet.png"), width=2500, height=2500, res=300)
P2_LAN
dev.off()


## READ LANDINGS TIME SERIES
TOT_LAN <- data.table(read.csv(file.path(inpDir_LAN,"Sprat_Landings_2000-2017.csv"), sep=",", head=T))
# reshape
TOT_LAN_a <- melt(TOT_LAN, id.var="Year")
# Plot landings time series
P3_LAN <- ggplot(TOT_LAN_a[variable!="OfficialLandings",], aes(Year, value*1000, col=factor(variable))) + 
  geom_line(size=1.2) + #geom_point(x=2017, y=2163, col="black", shape=18, size=2) +
  #geom_point(x=2017, y=2498, col="red", shape=4, size=2) +
  theme_bw(20) + ylab("Landings (t)") + scale_y_continuous(breaks=seq(0,12000,2000)) +
  theme(legend.position = c(0.8,0.8), legend.title=element_blank(), legend.key = element_blank()) +
  scale_x_continuous(breaks=seq(2000, 2017, 2)) 

png(file.path(outDir_LAN, "SPR_Landings_ALL_TAC.png"), width=3000, height=2500, res=280)
P3_LAN
dev.off()

##############################################
# READ LANDINGS FROM 1950 IN EXCEL SPREADSHEET
##############################################
TOT_LAN_5017 <- data.table(read.xlsx(file.path(inpDir_LAN,"Sprat7de_2018_Exploration.xlsx"), sheetName="Landings_By_country", head=T))
# rename countries
names(TOT_LAN_5017)[c(6,7,9)] <- c("UK-Eng_Wales_Nireland", "UK-Scotland", "FaroeIsland")
# reshape
TOT_LAN_5017r <- melt(TOT_LAN_5017[,1:11, with=F], id.vars = "Year")

# Plot
LAND_5017 <- ggplot(TOT_LAN_5017r, aes(Year, value)) +
  geom_area(aes(colour = variable, fill= variable), position = 'stack') +
  theme_bw(16) + ylab("Tonnes") +
  theme(legend.position="bottom", legend.title=element_blank()) + 
  scale_y_continuous(expand=c(0,0), breaks=seq(0,20000, 2500)) + 
  scale_x_continuous(expand=c(0,0), breaks=seq(1950,2020,5)) +
  guides(fill=guide_legend(nrow=1),colour=guide_legend(nrow=1))

png(file=file.path(outDir_LAN, "LAND_1950-2017.png"), res=300, width = 3500, height=2000)
LAND_5017
dev.off()
##################################################
## Read LFD from self sampling and Yellow Sheets
# Self-sampling are 4 samples from 1 boat (Girl Rona) in Nov-Dec 2017. 
# Yellow sheets are only 2 samples from the same boat (I believe) one in jan 2017 and one in quarter 4 2017. 
# I merged it into one sheet 
LFD_LAN <- data.table(read.xlsx(file.path(inpDir_LAN, "SELF_SAMPLING_Sprat_LFD_Season17-18.xlsx"), sheetName = "OverallLT"))
# Plot LFD
P_LFD_SS <- ggplot(LFD_LAN[Source=="SelfSampling",], aes(LT, Numbers, fill=factor(Sample))) + 
  geom_bar(stat="identity", position="dodge") + xlab("TL (cm)") +
  facet_wrap(~Sample) + theme_bw(18) + theme(legend.position = "none") 

P_LFD_YS <- ggplot(LFD_LAN[Source=="YellowSheets",], aes(LT, Numbers, fill=factor(Quarter))) + 
  geom_bar(stat="identity", position="dodge") + xlab("TL (cm)") +
  facet_wrap(~Quarter, scales = "free_y") + theme_bw(18) + theme(legend.position = "none") 

png(file.path(outDir_LAN, "SPR_Landings_LFD_SS.png"), width=3500, height=2500, res=250)
P_LFD_SS
dev.off()

png(file.path(outDir_LAN, "SPR_Landings_LFD_YS.png"), width=3500, height=1800, res=250)
P_LFD_YS
dev.off()


#####
# FALFISH
# Directories
inpDir_LAN_FF <- file.path(inpDir_LAN, "Falfish")
# 
LW_FF <- data.table(read.csv(file.path(inpDir_LAN_FF, "LW_Sprat_Falfish.csv")))
LW_FF$month <- month(LW_FF$date)
# Length in cm
LW_FF[,TL:=Length/10]

# plot Length
plot(LW_FF$Length)
LW_FF <- LW_FF[TL>6,]
# aggregate by month
table(LW_FF[,month], LW_FF[,TL])



#####
# Oceanfish
# Directories
inpDir_LAN_OF <- file.path(inpDir_LAN, "Interfish")
# read file
OF_LW <- list()
for(i in 1:2) {
  OF_LW[[i]] <- data.table(read.xlsx(file.path(inpDir_LAN_OF, "LW_2017_GirlRona.xlsx"), sheetIndex = i))}
# remove col with all NAs
OF_LW_a <- list()
for(i in 1:2) {
  OF_LW_a[[i]] <- OF_LW[[i]][,which(unlist(lapply(OF_LW[[i]], function(x)!all(is.na(x))))),with=F]}

# Remove column with word "count" in it
CountCol <- list()
OF_LW_b <- list()
OF_LW_c <- list()
for(i in 1:2){
  CountCol[[i]] <- which(OF_LW_a[[i]][2,]=="COUNT")
  OF_LW_b[[i]] <- OF_LW_a[[i]][,-CountCol[[i]], with=F]
  OF_LW_c[[i]] <- OF_LW_b[[i]][, WT:=as.numeric(as.character(VESSEL))]}

# Write Weight
myWT <- list()
for(i in 1:2){
myWT[[i]] <- OF_LW_c[[i]][,WT]
myWT[[i]] <- rep(myWT[[i]][which(!is.na(myWT[[i]]))], each=4)}
# copy into my dt

# Create vector for DATE (day number)
myDate <- list()
for(i in 1:2){
  myDate[[i]] <- as.numeric(as.character(as.vector(t(OF_LW_c[[i]][1,]))))[-c(1,ncol(OF_LW_c[[i]]))]
  for(j in 1:length(myDate[[i]])){ 
    if(is.na(myDate[[i]][j])){myDate[[i]][j] <- myDate[[i]][j-1]}}
  } 

# Add date to my dt
for(i in 1:2){
colnames(OF_LW_c[[i]]) <- c("oldWT", myDate[[i]], "WT")}

# Remove first 2 rows
for(i in 1:2){
  OF_LW_d[[i]] <- OF_LW_c[[i]][-c(1:2),]}

# Add WT to the dt
OF_LW_e <- list()
for(i in 1:2) {
  OF_LW_e[[i]] <- data.table(cbind(c(myWT[[i]], "TotLand"), OF_LW_d[[i]]))
  OF_LW_e[[i]][,c("oldWT", "WT") := NULL]}
# Remove column with "L" and "W"
rowLetter <- list()
OF_LW_f <- list()
OF_LWr <- list()
myMonth <- c("nov", "dec")
for(i in 1:2){
  rowLetter[[i]] <- which(OF_LW_e[[i]][,2] == "W")
  OF_LW_f[[i]] <- OF_LW_e[[i]][-rowLetter[[i]],]
  # convert column to numeric
  # OF_LW_g[[i]] <- OF_LW_f[[i]][, lapply(2:ncol(OF_LW_f[[i]]), as.numeric), with=F]
  # Change colnames 
  colnames(OF_LW_f[[i]]) <- paste(colnames(OF_LW_f[[i]]), letters, sep="")
  OF_LW_f[[i]]$month <- myMonth[i]
  OF_LWr[[i]] <- melt(OF_LW_f[[i]], id.vars=c("V1a", "month"))}

OF_LW_all <- do.call(rbind, OF_LWr)
# assign numeric to length
OF_LW_all[,TL:=as.numeric(as.character(value))]

# Remove rows with "NA" in TL
OF_LW_all <- OF_LW_all[!is.na(TL),]
# Now reset day 
OF_LW_all$day <- as.numeric(gsub("[a-zA-Z]", "", OF_LW_all$variable))

# Tot landing by date
OF_LAND <- OF_LW_all[V1a=="TotLand",]

# Only length
OF_LW_all <- OF_LW_all[V1a != "TotLand",]
OF_LW_all[,c("variable", "value"):=NULL]
names(OF_LW_all)[1] <- "WT"
# convert into numeric
OF_LW_all[,WT:=as.numeric(as.character(WT))]
OF_LW_all <- OF_LW_all[TL<20,]
# Plot length-weight relationship
ggplot(OF_LW_all, aes(TL, WT)) + geom_point()


# Aggregate everything by month
OF_LFD <- data.table(table(OF_LW_all$TL, OF_LW_all$month))
OF_LFD[,TL:=as.numeric(as.character(V1))]
OF_LFD[,TL:=round(TL, digits = 1)]
OF_LFD$TL <- ifelse(OF_LFD$TL == 13.1, 13, OF_LFD$TL)
# PLOT LFD
OF_LFD <- ggplot(OF_LFD, aes(TL, N, fill=V2)) + geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~V2, scale="free_x") + theme_bw(16) + 
  scale_x_continuous(breaks=seq(2,18,2)) + theme(legend.position = "none")

png(file=file.path(outDir, "OF_LFD.png"), res=300, width=3500, height=2000)
OF_LFD
dev.off()
