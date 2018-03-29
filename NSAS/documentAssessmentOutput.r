### ============================================================================
### Document Assessment
###
### 18/03/2018 Added Standard Graph database output
### ============================================================================

library(FLCore)
library(FLSAM)

# WKPELA 2018
# load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/benchmarks/2018/wkherring/2014 Meeting docs/06. Data/NSAS/SAM/NSH_final.RData")

# HAWG 2018
load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2018 Meeting docs1/05. Data/NSAS/NSH_HAWG2018_sf.Rdata")


old.opt           <- options("width","scipen")
options("width"=75,"scipen"=1000)

sam.out.file      <- FLSAM.out(NSH,NSH.tun,NSH.sam,format="TABLE 2.6.3.%i North Sea Herring.")
write(sam.out.file,file=paste(output.base,"sam.out",sep="."))
options("width"=old.opt$width,"scipen"=old.opt$scipen)
#
##And finally, write the results out in the lowestoft VPA format for further analysis
writeFLStock(NSH,output.file=file.path(output.dir,"NSAS_47d3_"))

stockSummaryTable <- cbind(rec(NSH.sam)$year,
                           rec(NSH.sam)$value,      rec(NSH.sam)$lbnd,    rec(NSH.sam)$ubnd,
                           tsb(NSH.sam)$value,      tsb(NSH.sam)$lbnd,    tsb(NSH.sam)$ubnd,
                           ssb(NSH.sam)$value,      ssb(NSH.sam)$lbnd,    ssb(NSH.sam)$ubnd,
                           catch(NSH.sam)$value,    catch(NSH.sam)$lbnd,  catch(NSH.sam)$ubnd,
                           catch(NSH.sam)$value / ssb(NSH.sam)$value, catch(NSH.sam)$lbnd / ssb(NSH.sam)$lbnd, catch(NSH.sam)$ubnd / ssb(NSH.sam)$ubnd,
                           fbar(NSH.sam)$value,     fbar(NSH.sam)$lbnd,   fbar(NSH.sam)$ubnd,
                           c(quantMeans(harvest(NSH.sam)[ac(0:1),])),
                           c(sop(NSH),NA),
                           c(catch(NSH),NA))
colnames(stockSummaryTable) <-
  c("Year",paste(rep(c("Recruits Age 0 (Thousands)","Total biomass (tonnes)","Spawing biomass (tonnes)",
                       "Landings (tonnes)","Yield / SSB (ratio)","Mean F ages 2-6"),each=3),c("Mean","Low","High")),"Mean F ages 0-1","SoP (%)","WG Catch")
stockSummaryTable[nrow(stockSummaryTable),] <- NA
stockSummaryTable[nrow(stockSummaryTable),"Spawing biomass (tonnes) Mean"] <- 2271364
stockSummaryTable[nrow(stockSummaryTable),2:4] <- c(rec(NSH.sam)$value[nrow(rec(NSH.sam))],rec(NSH.sam)$lbnd[nrow(rec(NSH.sam))],rec(NSH.sam)$ubnd[nrow(rec(NSH.sam))])

# write.csv(stockSummaryTable,file=file.path(output.dir,paste(name(NSH),"stockSummaryTable.csv",sep="_")))
write.csv(stockSummaryTable,file=file.path("NSAS","stockSummaryTable.csv"))

options("width"=old.opt$width,"scipen"=old.opt$scipen)


# ----------------------------------------------------------------------------
# Add assessment output to SAG database
# ----------------------------------------------------------------------------

# library(devtools)
# devtools::install_github("ices-tools-prod/icesSAG")

library(icesSAG)
library(icesSD)
library(tidyverse)

# login to ICES SAG, generate a token and put it in the ~/.Renviron_SG

# make use of the token
options(icesSAG.use_token = TRUE)

info     <- stockInfo(StockCode="her.27.3a47d", AssessmentYear = 2018, ContactPerson = "benoit.berges@wur.nl")

FiY       <- ac(range(NSH)["minyear"]) 
DtY       <- ac(range(NSH)["maxyear"]) 
LaY       <- ac(range(NSH.sam)["maxyear"]) 
nyrs      <- an(LaY)-an(FiY)+1

fishdata <- stockFishdata(an(FiY):an(LaY))

info$StockCategory             <- "1"
info$MSYBtrigger               <- 1400000
info$Blim                      <- 800000
info$Bpa                       <- 900000
info$Flim                      <- 0.34
info$Fpa                       <- 0.30 
info$FMSY                      <- 0.26
info$Fage                      <- "2-6"
info$RecruitmentAge            <- 0
info$CatchesCatchesUnits       <- "tonnes"
info$RecruitmentDescription    <- "wr"
info$RecruitmentUnits          <- "thousands"
info$FishingPressureDescription<- "F"
info$FishingPressureUnits      <- "Year-1"
info$StockSizeDescription      <- "SSB"
info$StockSizeUnits            <- "tonnes"
info$CustomSeriesName1         <- "model catch"
info$CustomSeriesName2         <- "model catch low"
info$CustomSeriesName3         <- "model catch high"
info$CustomSeriesUnits1        <- "tonnes"
info$CustomSeriesUnits2        <- "tonnes"
info$CustomSeriesUnits3        <- "tonnes"
info$Purpose                   <- "Advice"
# info$Purpose                   <- "Bench"


fishdata$Catches[1:nyrs-1]       <- an(NSH@landings) 
fishdata$Low_Recruitment       <- rec(NSH.sam)$lbnd
fishdata$Recruitment           <- rec(NSH.sam)$value
fishdata$High_Recruitment      <- rec(NSH.sam)$ubnd 
fishdata$TBiomass              <- tsb(NSH.sam)$value
fishdata$Low_TBiomass          <- tsb(NSH.sam)$lbnd
fishdata$High_TBiomass         <- tsb(NSH.sam)$ubnd
fishdata$StockSize             <- ssb(NSH.sam)$value
fishdata$Low_StockSize         <- ssb(NSH.sam)$lbnd
fishdata$High_StockSize        <- ssb(NSH.sam)$ubnd
fishdata$FishingPressure[1:nyrs-1]       <- fbar(NSH.sam)$value[1:nyrs-1]
fishdata$Low_FishingPressure[1:nyrs-1]   <- fbar(NSH.sam)$lbnd[1:nyrs-1]
fishdata$High_FishingPressure[1:nyrs-1]  <- fbar(NSH.sam)$ubnd[1:nyrs-1]

fishdata$CustomSeries1[1:nyrs-1]         <- catch(NSH.sam)$value[1:nyrs-1]
fishdata$CustomSeries2[1:nyrs-1]         <- catch(NSH.sam)$lbnd[1:nyrs-1]
fishdata$CustomSeries3[1:nyrs-1]         <- catch(NSH.sam)$ubnd[1:nyrs-1]

# summary(fishdata)
# glimpse(fishdata)

# upload to SAG
key <- icesSAG::uploadStock(info, fishdata)

# now add a comment to the upload of an assessment
setSAGSettingForAStock(AssessmentKey=9351, 
                       chartKey=0,
                       settingKey=21,
                       settingValue="Martin Pastoors Historical data",
                       copyNextYear=FALSE) 


# get stock info from the stock database (Mike)
stockinfo <- getSD()

# get all the sandeel stocks
filter(stockinfo, substr(StockKeyLabel,1,3) == "san") %>% 
  distinct(StockKeyLabel)

# set all the hawgstocks
hawgstocks <- c("her.27.3a47d", "her.27.nirs","her.27.irls","her.27.20-24", "her.27.6a7bc",
                "spr.27.4","spr.27.3a","spr.27.7de",
                "san.sa.1r","san.sa.2r","san.sa.3r","san.sa.4", "san.sa.5r", "san.sa.6", "san.sa.7r")

# download all the stocks in HAWG 2018
hawgdata <- 
  getSAG(hawgstocks, year = 2018) %>% 
  filter(AssessmentKey != 9351)

save(hawgdata, file="hawgdata.RData")



# Catch or landings
hawgdata %>% 
  mutate(catches = ifelse(is.na(catches), landings, catches)) %>% 
  
  ggplot(aes(x=Year, y=catches)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_line(aes(colour=substr(fishstock,1,3))) +
  facet_wrap(~fishstock, scales="free_y") +
  expand_limits(y=0) +
  labs(title="HAWG 2018: Landings or catches")

# SSB
hawgdata %>% 
  filter(Year >= 1980) %>% 
  
  ggplot(aes(x=Year, y=SSB)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_ribbon(aes(ymin=low_SSB, ymax=high_SSB, fill=substr(fishstock,1,3)), alpha=0.3) +
  geom_line(aes(colour=substr(fishstock,1,3))) +
  facet_wrap(~fishstock, scales="free_y") +
  expand_limits(y=0) +
  labs(title="HAWG 2018: SSB")

# F
hawgdata %>% 
  filter(Year >= 1980) %>% 
  
  ggplot(aes(x=Year, y=F)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_ribbon(aes(ymin=low_F, ymax=high_F, fill=substr(fishstock,1,3)), alpha=0.3) +
  geom_line(aes(colour=substr(fishstock,1,3))) +
  facet_wrap(~fishstock, scales="free_y") +
  expand_limits(y=0) +
  labs(title="HAWG 2018: Fishing mortality")

# recruitment
hawgdata %>% 
  filter(Year >= 1980) %>% 
  
  ggplot(aes(x=Year, y=recruitment)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_ribbon(aes(ymin=low_recruitment, ymax=high_recruitment, fill=substr(fishstock,1,3)), alpha=0.3) +
  geom_line(aes(colour=substr(fishstock,1,3))) +
  facet_wrap(~fishstock, scales="free_y") +
  expand_limits(y=0) +
  labs(title="HAWG 2018: recruitment")





# Download all the assessments in SAG; also unpublished ones
SAGdata  <- 
  getSAG(stock=NULL, year = 0) %>% 
  filter(Purpose == "Advice")

save(SAGdata, file="SAGdata 20180320.RData")
summary(SAGdata)
  
  
SAGdata %>% 
  filter(Year >= 1980) %>% 
  filter(substr(fishstock,1,3) %in% c("her","cod")) %>% 
  
  ggplot(aes(x=Year, y=SSB, group=AssessmentYear)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_line(aes(colour=factor(AssessmentYear))) +
  expand_limits(y=0) +
  
  facet_wrap(~fishstock, scales="free_y") +
  labs(title="SSB")

