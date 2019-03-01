################################################################################
# HAWG data overviews
#
# Generating overviews of several stocks assessed within HAWG
#
# 18/03/2018: coding during HAWG 2018
# 20/03/2018: added all weight in the catch; added the crayola plots; Note stock trends now 
#             via the SAG download
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
library(FLSAM); 
library(FLEDA)
library(readxl);
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(directlabels)
library(ggmisc) # e.g. for crayola

source("_Common/crayola.r")

options(max.print=999999)

# ===================================================================================
# Load datasets 
# ===================================================================================

# Load NSAS data
load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2018 Meeting docs1/05. Data/NSAS/NSH_HAWG2018_sf.Rdata")

NSH.canum <-
  slot(NSH,"catch.n") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, number = data) %>%
  filter(number != -1) %>% 
  mutate(stock = "NSH")

NSH.weca <-
  slot(NSH,"catch.wt") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, weight = data) %>%
  filter(weight != -1) %>% 
  mutate(stock = "NSH")

NSH.HERAS <-
  slot(NSH.tun[[1]],"index") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, index = data) %>%
  filter(index != -1) %>% 
  mutate(stock = "NSH",
         survey = "HERAS")

# Load CSAH data ------------------------------------

CSH.canum  <- 
  readVPAFile("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2018 Meeting docs1/05. Data/Celtic Sea/canum.txt") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, number = data) %>%
  filter(number != -1) %>% 
  mutate(stock = "CSH")

CSH.weca  <- 
  readVPAFile("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2018 Meeting docs1/05. Data/Celtic Sea/weca.txt") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, weight = data) %>%
  filter(weight != -1) %>% 
  mutate(stock = "CSH")


# Load WBSS data -------------------------------------------

WBSS.dir  <- "D:/HAWG/2018/05. Data/WBSS/WBSS_HAWG_2018"
WBSS.data <- get(load(file.path(WBSS.dir,"run/data.RData")))
WBSS.fit  <- get(load(file.path(WBSS.dir,"run/model.RData" )))

WBSS.canum <-
  read.ices(file.path(WBSS.dir,"data/cn.dat")) %>% 
  data.frame() %>% 
  rownames_to_column(var="year") %>% 
  gather(key=age, value=number, X0:X8) %>% 
  mutate(age = as.integer(gsub("X","",age)),
         stock="WBSS", 
         year=an(year))

WBSS.weca <- 
  read.ices(file.path(WBSS.dir,"data/cw.dat")) %>% 
  data.frame() %>% 
  rownames_to_column(var="year") %>% 
  gather(key=age, value=weight, X0:X8) %>% 
  filter(!is.na(weight)) %>% 
  group_by(year, age) %>%
  summarize(n = n(),
            weight = sum(weight, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(age = as.integer(gsub("X","",age)),
         weight = weight / n,
         stock="WBSS", 
         year=an(year)) %>% 
  select(-n)


# Load 6a-7bc data -----------------------------------------------------

load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2018 Meeting docs1/05. Data/6a7bc/Final_6a7bcHerring.Rdata")

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

MSH.MSHAS <-
  slot(MSH.tun[[1]],"index") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, index = data) %>%
  filter(index != -1) %>% 
  mutate(stock = "MSH",
         survey = "MSHAS")

MSH.WOS <-
  slot(MSH.tun[[2]],"index") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, index = data) %>%
  filter(index != -1) %>% 
  mutate(stock = "MSH",
         survey = "WOS")

MSH.MSHAS2 <-
  bind_rows(MSH.WOS, MSH.MSHAS) %>% 
  mutate(survey = "MSHASfull")



# Herring 6a north (WoS)

WOS.canum  <- 
  readVPAFile("D:/HAWG/2018/05. Data/6aN/data/canum.txt") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, number = data) %>%
  filter(number != -1) %>% 
  mutate(stock = "WOS")

WOS.weca  <- 
  readVPAFile("D:/HAWG/2018/05. Data/6aN/data/weca.txt") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, weight = data) %>%
  filter(weight != -1) %>% 
  mutate(stock = "WoS")

# Herring 6a south 7bc (IRLW)

# IRLW.canum  <- 
#   readVPAFile("D:/HAWG/2018/05. Data/6aN/data/canum.txt") %>% 
#   as.data.frame() %>% 
#   dplyr::select(year, age, number = data) %>%
#   filter(number != -1) %>% 
#   mutate(stock = "IRLW")
# 
# IRLW.weca  <- 
#   readVPAFile("E:/HAWG/2018 Meeting docs1/05. Data/IRLW/data/weca.txt") %>% 
#   as.data.frame() %>% 
#   dplyr::select(year, age, weight = data) %>%
#   filter(weight != -1) %>% 
#   mutate(stock = "IRLW")

# Irish Sea herring ---------------------------------------------------

load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2018 Meeting docs1/05. Data/her_nirs/ISH_assessment 2018.RData")

ISH.canum <-
  slot(ISH,"catch.n") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, number = data) %>%
  filter(number != -1) %>% 
  mutate(stock = "ISH")

ISH.weca <-
  slot(ISH,"catch.wt") %>% 
  as.data.frame() %>% 
  dplyr::select(year, age, weight = data) %>%
  filter(weight != -1) %>% 
  mutate(stock = "ISH")


# North Sea sprat ---------------------------------------------------

# NSsprat.canum <-
#   read.csv(file="//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2018%20%Meeting%20%docs1/05.%20%Data/NSsprat/Total_catch_in_numbers_and_mean_weight_2017_IV.csv",
#            header=TRUE) %>% 
#   dplyr::select(year, quarter, n0, n1, n2, n3, n4) %>% 
#   gather(key=age, value=number, n0:n4) %>% 
#   mutate(age = an(gsub("n", "", age))) %>% 
#   group_by(year, age) %>% 
#   summarize(number= sum(number, na.rm=TRUE)) %>% 
#   ungroup() %>% 
#   filter(year <= max(NSsprat.ssb$year))
# 
# NSsprat.weca <-
#   read.csv(file="//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2018 Meeting docs1/05. Data/NSsprat/Total_catch_in_numbers_and_mean_weight_2017_IV.csv",
#            header=TRUE) %>% 
#   dplyr::select(year, quarter, n0, n1, n2, n3, n4, mw0, mw1, mw2, mw3, mw4) %>% 
#   gather(key=var, value=number, n0:mw4) %>% 
#   mutate(weca  = ifelse(grepl("n",var), number, NA),
#          age   = gsub("n|mw","",var),
#          canum = ifelse(grepl("mw", var), number, NA)) %>% 
#   dplyr::select(year, quarter, age, weca, canum) %>%
#   gather(key=var, value=number, weca:canum) %>% 
#   group_by(year, age) %>% 
#   summarize(number= sum(number, na.rm=TRUE)) %>% 
#   ungroup() %>% 
#   filter(year <= max(NSsprat.ssb$year))

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
    # IRLW.canum,
    WOS.canum
  )

weca <-
  bind_rows(
    NSH.weca,
    MSH.weca,
    WBSS.weca,
    CSH.weca,
    ISH.weca,
    # IRLW.weca,
    WOS.weca
  ) %>% 
  filter(!is.na(weight), !weight <= 0)


# ===================================================================================
# Plot the crayola of catch at age
# ===================================================================================


canum %>% 
  group_by(stock, year, age) %>% 

  # filter(stock %in% c("NSH","WoS")) %>% 
  # filter(stock %in% c("IRLW","CSH","ISH")) %>% 
  filter(stock %in% c("WBSS")) %>% 
  filter(year >= 1980) %>% 
  filter(age %in% 1:8) %>% 
  
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
  labs(x = NULL, y = NULL, title="Herring relative catch at age") +
  facet_grid(age ~ stock, scale = "free_y", switch = "y")


# ===================================================================================
# Plot the crayola of acoustic survey
# ===================================================================================

bind_rows(NSH.HERAS, MSH.MSHAS2) %>% 
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
# weight in the catch
# ===================================================================================

weca %>% 
  ungroup() %>% 
  mutate(decade = 10 * (year %/% 10 )) %>% 
  filter(! stock %in% c("MSH","WBSS")) %>% 
  filter(year >= 1980) %>% 
  filter(age %in% 1:8) %>% 
  
  ggplot(aes(year,weight, group=stock)) +
  theme_bw() +
  # theme(legend.position="none") +
  geom_point(aes(colour=stock)) +
  # geom_line(aes(colour=stock), size=0.5) +
  geom_smooth(aes(colour=stock), size=1, method="loess", se=FALSE, span=0.3) +
  facet_wrap(~age, scales="free_y", ncol=4) 
  # facet_grid(age~stock, scales="free_y") 

# 6a north and north sea
weca %>% 
  ungroup() %>% 
  mutate(decade = 10 * (year %/% 10 )) %>% 
  filter(stock %in% c("NSH","WoS")) %>% 
  filter(year >= 1980) %>% 
  filter(age %in% 1:8) %>% 
  
  ggplot(aes(year,weight, group=stock)) +
  theme_bw() +
  # theme(legend.position="none") +
  geom_point(aes(colour=stock)) +
  # geom_line(aes(colour=stock), size=0.5) +
  geom_smooth(aes(colour=stock), size=1, method="loess", se=FALSE) +
  facet_wrap(~age, scales="free_y", ncol=4) 

# celtic sea, irish sea, 6a south 7bc
weca %>% 
  ungroup() %>% 
  mutate(decade = 10 * (year %/% 10 )) %>% 
  filter(stock %in% c("CSH","IRLW","ISH")) %>% 
  filter(year >= 1980) %>% 
  filter(age %in% 1:8) %>% 
  
  ggplot(aes(year,weight, group=stock)) +
  theme_bw() +
  # theme(legend.position="none") +
  geom_point(aes(colour=stock)) +
  # geom_line(aes(colour=stock), size=0.5) +
  geom_smooth(aes(colour=stock), size=1, method="loess", se=FALSE) +
  facet_wrap(~age, scales="free_y", ncol=4) 

# WBSS herring

weca %>% 
  ungroup() %>% 
  mutate(decade = 10 * (year %/% 10 )) %>% 
  filter(stock %in% c("WBSS")) %>% 
  filter(year >= 1980) %>% 
  filter(age %in% 1:8) %>% 
  
  ggplot(aes(year,weight, group=stock)) +
  theme_bw() +
  # theme(legend.position="none") +
  geom_point(aes(colour=stock)) +
  # geom_line(aes(colour=stock), size=0.5) +
  geom_smooth(aes(colour=stock), size=1, method="loess", se=FALSE) +
  facet_wrap(~age, scales="free_y", ncol=4) 

