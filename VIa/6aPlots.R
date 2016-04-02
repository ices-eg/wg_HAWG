# code from M Pastoors to plot coloured plots
# needs to run in R 3.02

library(FLCore)
library(FLSAM)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)

setwd("C:/Users/Lusseaus/Documents/ICES Working Groups/HAWG/2016/Repository/wg_HAWG/_Common")

#source("C:/DATA/GIT/mptools/R/samprocesserror.R")
source("C:/Users/Lusseaus/Documents/ICES Working Groups/HAWG/2016/Repository/wg_HAWG/_Common/flstocktorbya.R")
source("C:/Users/Lusseaus/Documents/ICES Working Groups/HAWG/2016/Repository/wg_HAWG/_Common/flindicestorbya.R")
source("C:/Users/Lusseaus/Documents/ICES Working Groups/HAWG/2016/Repository/wg_HAWG/_Common/crayola.R")

load("C:/Users/Lusseaus/Documents/ICES Working Groups/HAWG/2016/Repository/wg_HAWG/VIa/results/Final_VIaHerring.Rdata")

her2015    <- NSH
her2015sam <- NSH.sam

load("C:/DATA/HAWG/NSAS 2016.RData")
her2016        <- NSH
her2016sam     <- NSH.sam
her2016tun     <- NSH.tun
her2016rbya    <- flstock_to_rbya(her2016, scale=1, project = TRUE, plusgroup = TRUE)
her2016tunrbya <- flindices_to_rbya(her2016tun)
