# ============================================================================
# Compare assessment results with previous years
#
# 18/03/2020 Adapted from UPload Assessment Data in SAG code
# ============================================================================

rm(list=ls())

library(FLCore)
library(FLSAM)
library(icesSAG)  # devtools::install_github("ices-tools-prod/icesSAG")
library(tidyverse)


# Load utils code
# source("../_Common/get_dropbox.r")

# Set dropbox folder
# advicedir <- paste(get_dropbox(), "/iAdvice", sep="")


# Get the assessment data and convert to dataframe
load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2018 Meeting docs1/05. Data/NSAS/NSH_HAWG2018_sf.Rdata")
NSH2018      <- NSH
NSH2018.ctrl <- NSH.ctrl
NSH2018.sam  <- NSH.sam
NSH2018.tun  <- NSH.tun

load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2019 Meeting Docs/06. Data/NSAS/NSH_HAWG2019_sf.Rdata")
NSH2019      <- NSH
NSH2019.ctrl <- NSH.ctrl
NSH2019.sam  <- NSH.sam
NSH2019.tun  <- NSH.tun

load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2020 Meeting Docs/06. Data/her.27.3a47d/NSH_HAWG2020_sf.Rdata")
NSH2020      <- NSH
NSH2020.ctrl <- NSH.ctrl
NSH2020.sam  <- NSH.sam
NSH2020.tun  <- NSH.tun

rm(NSH, NSH.ctrl, NSH.sam, NSH.tun)

ssb2018    <- ssb(NSH2018.sam)     %>% mutate(var = "ssb", assessmentyear=2018) 
ssb2019    <- ssb(NSH2019.sam)     %>% mutate(var = "ssb", assessmentyear=2019) 
ssb2020    <- ssb(NSH2020.sam)     %>% mutate(var = "ssb", assessmentyear=2020) 

bind_rows(ssb2018, ssb2019, ssb2020) %>% 
  filter(year >= 2000) %>% 
  ggplot(aes(x=year, y=value)) +
  theme_bw() +
  geom_ribbon(aes(ymin=lbnd, ymax=ubnd, fill=factor(assessmentyear)), alpha=0.4) +
  geom_line(aes(colour=factor(assessmentyear)), size=1) 
  

