################################################################################
# HAWG data overviews
#
# Generating overviews of several stocks assessed within HAWG
#
# 18/03/2018 coding during HAWG 2018
# 20/03/2018 added all weight in the catch; added the crayola plots; Note stock trends now 
#            via the SAG download
# 21/03/2019 Updated during HAWG 2019 (MP)
# 25/03/2020 Updated for HAWG 2020 (MP)
################################################################################

rm(list=ls());

# R.version
# find_rtools()

# library(devtools)
# library(pkgbuild)

# Installing the stockassessment package is tricky. Better done in R directly than in RStudio 
# (because there the RTools cannot be found)
# install.packages("Matrix")
# install.packages("ellipse")
# install.packages("TMB")
# devtools::install_github("fishfollower/SAM/stockassessment", ref="components", dependencies=FALSE)

library(stockassessment)

# libraries
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(directlabels)
library(ggmisc) # e.g. for crayola
library(icesSAG)

# source("_Common/crayola.r")

# use token
options(icesSAG.use_token = TRUE)

# Load utils code
source("../mptools/r/my_utils.r")


# ===================================================================================
# Load datasets 
# ===================================================================================

# Load NSAS data
load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2020 Meeting Docs/06. Data/her.27.3a47d/NSH_HAWG2020_sf.Rdata")
NSH.df     <- as.data.frame(NSH) %>% mutate(stock="her.27.3a47d")
NSH.tun.df <- as.data.frame(NSH.tun) %>% mutate(stock="her.27.3a47d")
NSH.sag.df <- as.data.frame(getSAG(stock = "her.27.3a47d", year = 2020)) %>% tolower()

# Irish Sea herring ---------------------------------------------------
load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2020 Meeting Docs/06. Data/her.27.nirs/SAM/results/ISH_assessment 2020.RData")
ISH.df     <- as.data.frame(ISH) %>% mutate(stock="her.27.nirs")
ISH.tun.df <- as.data.frame(ISH.tun) %>% mutate(stock="her.27.nirs")


# Load Celtic Sea herring ------------------------------------
mypath     <- "//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2020 Meeting docs/06. Data/her.27.irls/data/"
CSH.df     <- readFLStock(file.path(mypath, 'index.txt')) %>% as.data.frame() %>% mutate(stock="her.27.irls")
CSH.tun.df <- readFLIndices(file.path(mypath, "fleet.txt")) %>% as.data.frame() %>% mutate(stock="her.27.irls")

# Load WBSS data -------------------------------------------
WBSS.dir    <- "//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2020 Meeting docs/06. Data/her.27.2024/data/"
WBSS.df     <- readFLStock(file.path(WBSS.dir, 'data/index.txt')) %>% as.data.frame() %>% mutate(stock="her.27.20-24")
WBSS.tun.df <- readFLIndices(file.path(WBSS.dir, "data/survey.dat")) %>% as.data.frame() %>% mutate(stock="her.27.20-24")
NSH.sag.df <- getSAG(stock = "her.27.20-24", year = 2020)

data.frame(stockassessment::ssbtable( stockassessment::fitfromweb("WBSS_HAWG_2020_sf")))

# Load 6a-7bc data -----------------------------------------------------

load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2019 Meeting Docs/06. Data/6a7bc/Final_2017_VIaHerring.Rdata")

MSH.stock.n <-
  slot(MSH,"stock.n") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, number = data) %>%
  filter(number != -1) %>% 
  mutate(stock = "MSH")

MSH.canum <-
  slot(MSH,"catch.n") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, number = data) %>%
  filter(number != -1) %>% 
  mutate(stock = "MSH")

MSH.weca <-
  slot(MSH,"catch.wt") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, weight = data) %>%
  filter(weight != -1) %>% 
  mutate(stock = "MSH")

MSH.west <-
  slot(MSH,"stock.wt") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, weight = data) %>%
  filter(weight != -1) %>% 
  mutate(stock = "MSH")

MSH.HERAS <-
  slot(MSH.tun[[1]],"index") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, index = data) %>%
  filter(index != -1) %>% 
  mutate(stock = "MSH",
         survey = "MSHAS")



# North Sea sprat ---------------------------------------------------

# spr.path <- "//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2018 Meeting docs1/05. Data/Celtic Sea/"
spr.path  <- "D:/HAWG/2019/06. Data/SPR-3a4"
startyear <- 1974

NSsprat.canum <-
  read.table(file=file.path(spr.path, "canum.in"),
           header=FALSE, skip=1) %>%
  setNames(c("age0","age1","age2","age3")) %>% 
  mutate(year   = startyear + floor((row_number()-1)/4),
         season = (row_number() %% 4),
         season = ifelse(season == 0, 4, season)) %>% 
  gather(key=age, value=number, age0:age3) %>%
  mutate(age = an(gsub("age", "", age))) %>%
  group_by(year, age) %>%
  summarize(number= sum(number, na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(stock = "SPR34")

NSsprat.weca <-
  read.table(file=file.path(spr.path, "weca.in"),
             header=FALSE, skip=1) %>%
  setNames(c("age0","age1","age2","age3")) %>% 
  mutate(year   = startyear + floor((row_number()-1)/4),
         season = (row_number() %% 4),
         season = ifelse(season == 0, 4, season)) %>% 
  gather(key=age, value=weight, age0:age3) %>%
  mutate(age = an(gsub("age", "", age))) %>%
  
  left_join(NSsprat.canum, by=c("year","age")) %>% 
  group_by(year, age) %>%
  summarize(weight= weighted.mean(weight, number, na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(stock = "SPR34")

# ===================================================================================
# Combine all the data 
# ===================================================================================

canum <-
  bind_rows(
    NSH.canum,
    MSH.canum,
    WBSS.canum,
    CSH.canum,
    ISH.canum,
    NSsprat.canum
  ) %>% 
  mutate(stock = factor(stock, levels=c("CSH","ISH","MSH","NSH","WBSS","SPR34")))

weca <-
  bind_rows(
    NSH.weca,
    MSH.weca,
    WBSS.weca,
    CSH.weca,
    ISH.weca,
    NSsprat.weca
  ) %>% 
  filter(!is.na(weight), !weight <= 0) %>% 
  mutate(stock = factor(stock, levels=c("CSH","ISH","MSH","NSH","WBSS","SPR34")))

west <-
  bind_rows(
    NSH.west,
    # MSH.west,
    WBSS.west,
    CSH.west
    # ISH.west
  ) %>% 
  filter(!is.na(weight), !weight <= 0)

# ===================================================================================
# Plot the crayola of catch at age or stock at age
# ===================================================================================


canum %>% 

  filter(stock %in% c("WBSS")) %>% 
  # filter(stock %in% c("NSH","WoS")) %>% 
  # filter(stock %in% c("CSH","ISH")) %>% 
  # filter(stock %in% c("NSH", "MSH")) %>% 
  filter(year >= 1980) %>% 
  filter(age %in% 0:9) %>% 
  
  # first calculate the proportions at age
  group_by(stock, year) %>%
  mutate(number = number/sum(number, na.rm=TRUE)) %>%
  
  group_by(stock, year, age) %>% 
  summarise(value = sum(number, na.rm=TRUE)) %>% 
  group_by(stock, age) %>% 
  mutate(value = value/mean(value, na.rm=TRUE)) %>% 
  mutate(yc = year - age) %>% 
  data.frame() %>% 
  
  ggplot() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank()) +
  theme(panel.border     = element_rect(colour="black" , size=0.1)) +
  theme(axis.ticks.y     = element_blank() ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
  theme(panel.spacing = unit(0.2, "lines")) +
  geom_col(aes(year, value, fill = factor(yc))) + 
  scale_fill_crayola() +
  labs(x = NULL, y = NULL, title="Herring relative stock at age") +
  facet_grid(age ~ stock, scale = "free_y", switch = "y")


# in a loop
for (i in 1:length(levels(canum$stock))) {
  t <- 
    canum %>% 
    complete(year, age, stock) %>% 
    filter(stock %in% levels(canum$stock)[i]) %>% 
    filter(year >= 1980) %>% 
    filter(age %in% 1:8) %>% 
    
    group_by(stock, year, age) %>% 
    summarise(value = sum(number, na.rm=TRUE)) %>% 
    group_by(stock, age) %>% 
    mutate(value = value/mean(value, na.rm=TRUE)) %>% 
    mutate(yc = year - age) %>% 
    data.frame()
    
  assign(
    paste("p",levels(canum$stock)[i],sep=""),
    ggplot(t) +
      theme_bw() +
      theme(legend.position = "none") +
      theme(axis.text.y     = element_blank()) +
      theme(panel.border    = element_rect(colour="black" , size=0.1)) +
      theme(axis.ticks.y    = element_blank() ) +
      theme(axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
      theme(panel.spacing   = unit(0.2, "lines")) +
      {if (i != 1) theme(strip.background.y = element_blank()) } +
      {if (i != 1) theme(strip.text.y       = element_blank()) } +
      theme(plot.margin=unit(c(0,0,0,0),"mm")) +
      geom_col(aes(year, value, fill = factor(yc))) + 
      scale_fill_crayola() +
      labs(x = NULL, y = NULL, title=NULL) +
      facet_grid(age ~ stock, scale = "free_y", switch = "y")
  ) # end of assign  
}

cowplot::plot_grid(plotlist=mget(paste("p", levels(canum$stock), sep="")),
                   ncol=length(unique(canum$stock)), scale=0.99, align="hv", 
                   rel_widths = c(1.0, rep(1.0,length(levels(canum$stock)))) )  


# ===================================================================================
# Plot the crayola of acoustic survey
# ===================================================================================

NSH.HERAS %>% 
# bind_rows(NSH.HERAS, MSH.HERAS) %>% 
  group_by(stock, survey, year, age) %>% 
  
  filter(year >= 1990) %>% 
  filter(age %in% 1:8) %>% 
  
  summarise(value = sum(index, na.rm=TRUE)) %>% 
  group_by(stock, survey, age) %>% 
  mutate(value = value/mean(value, na.rm=TRUE)) %>% 
  mutate(yc = year - age) %>% 
  data.frame() %>% 
  
  ggplot() +
  
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank()) +
  theme(panel.border     = element_rect(colour="black" , size=0.1)) +
  theme(axis.ticks.y     = element_blank() ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
  theme(panel.spacing = unit(0.2, "lines")) +
  
  geom_col(aes(year, value, fill = factor(yc))) + 
  scale_fill_crayola() +
  labs(x = NULL, y = NULL, title="Herring acoustic survey relative index at age") +
  facet_grid(age ~ survey, scale = "free_y", switch = "y")

# integrated plots
bind_rows(NSH.HERAS, MSH.MSHAS2) %>% 
  group_by(stock, survey, year, age) %>% 
  
  filter(year >= 2000) %>% 
  filter(age %in% 1:8) %>% 
  
  summarise(value = sum(index, na.rm=TRUE)) %>% 
  group_by(stock, survey, age) %>% 
  mutate(value = value/mean(value, na.rm=TRUE)) %>% 
  mutate(yc = year - age) %>% 
  data.frame() %>% 
  
  ggplot() +
  
  theme_bw() +
  # theme(legend.position = "none") +
  theme(axis.text.y = element_blank()) +
  theme(panel.border     = element_rect(colour="black" , size=0.1)) +
  theme(axis.ticks.y     = element_blank() ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
  theme(panel.spacing = unit(0.2, "lines")) +

  geom_bar(aes(year, value, fill = factor(survey)), stat="identity", position=position_dodge()) + 
  scale_fill_crayola() +
  labs(x = NULL, y = NULL, title="Herring acoustic survey relative index at age") +
  facet_grid(age ~ ., scale = "free_y", switch = "y")


# ===================================================================================
# weight in the stock
# ===================================================================================

bind_rows(NSH.stock.n, MSH.stock.n, ISH.stock.n) %>%
  setNames(gsub("number","stock.n", names(.))) %>% 
  left_join(bind_rows(NSH.west, MSH.west, ISH.west), by=c("year","age","stock")) %>% 
  filter(year >= 1960) %>% 
  
  group_by(stock, year) %>% 
  summarize(weightw = weighted.mean(weight, w=stock.n, na.rm=TRUE),
            weight  = mean(weight, na.rm=TRUE)) %>% 
  group_by(stock) %>% 
  mutate(rel_weight  = weight  / mean(weight, na.rm=TRUE),
         rel_weightw = weightw / mean(weightw, na.rm=TRUE)) %>% 
  
  ggplot(aes(year,rel_weight, group=stock)) +
  theme_bw() +
  geom_point(aes(colour=stock)) +
  geom_smooth(aes(colour=stock), size=1, method="loess", se=FALSE, span=0.3) 


bind_rows(NSH.stock.n, MSH.stock.n, ISH.stock.n) %>%
  setNames(gsub("number","stock.n", names(.))) %>% 
  left_join(bind_rows(NSH.west, MSH.west, ISH.west), by=c("year","age","stock")) %>% 
  filter(year >= 1960) %>% 
  filter(age == 4) %>% 
  
  ggplot(aes(year,weight, group=stock)) +
  theme_bw() +
  geom_point(aes(colour=stock)) +
  geom_smooth(aes(colour=stock), size=1, method="loess", se=FALSE, span=0.3) 

  
# ===================================================================================
# weight in the catch
# ===================================================================================


for (i in 1:length(levels(weca$stock))) {
  t <- 
    weca %>% 
    complete(year, age, stock) %>% 
    filter(stock %in% levels(weca$stock)[i]) %>% 
    filter(year >= 1980) %>% 
    filter(age %in% 1:8) 

  assign(
    paste("p",levels(weca$stock)[i],sep=""),
    ggplot(t, aes(year,weight, group=stock)) +
      theme_bw() +
      theme(legend.position = "none") +
      # theme(axis.text.y     = element_blank()) +
      # theme(axis.ticks.y    = element_blank() ) +
      theme(panel.border    = element_rect(colour="black" , size=0.1)) +
      theme(axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
      theme(panel.spacing   = unit(0.1, "lines")) +
      {if (i != 6) theme(strip.background.y = element_blank()) } +
      {if (i != 6) theme(strip.text.y       = element_blank()) } +
      {if (i != 6) theme(plot.margin = unit( c(0,0.5,0,0), units = "lines")) else  theme(plot.margin=unit(c(0,0,0,0),"lines"))}  +

      geom_point(aes(colour=stock)) +
      geom_smooth(aes(colour=stock, fill=stock), size=1, method="loess", span=0.5, se=TRUE, alpha=0.3) +
      labs(x=NULL, y=NULL, title=NULL) +
      facet_grid(age ~ stock, scale = "free_y")
  ) # end of assign  
}

cowplot::plot_grid(plotlist=mget(paste("p", levels(weca$stock), sep="")),
                   ncol=length(unique(weca$stock)), scale=0.99, align="hv", 
                   rel_widths = c(1.0, rep(1.0,length(levels(weca$stock)))) )


weca %>% 
  complete(year, age, stock) %>% 
  filter(stock %in% c("NSH")) %>% 
  filter(year >= 1980) %>% 
  filter(age %in% 1:8) %>% 

  ggplot(aes(year,weight, group=stock)) +
    theme_bw() +
    theme(legend.position = "none") +
    # theme(axis.text.y     = element_blank()) +
    # theme(axis.ticks.y    = element_blank() ) +
    theme(panel.border    = element_rect(colour="black" , size=0.1)) +
    theme(axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
    theme(panel.spacing   = unit(0.1, "lines")) +

    geom_point(aes(colour=stock)) +
    geom_smooth(aes(colour=stock, fill=stock), size=1, method="loess", span=0.5, se=TRUE, alpha=0.3) +
    labs(x=NULL, y=NULL, title=NULL) +
    facet_wrap(. ~ age, scale = "free_y")
