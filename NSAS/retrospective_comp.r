### ======================================================================================================
### Setting up
### ======================================================================================================
rm(list=ls())
graphics.off()

library(FLSAM)
library(FLEDA)
path <- "C:/git/wg_HAWG/NSAS/"
try(setwd(path),silent=TRUE)

dataDir           <-  file.path(".","data/")        # figures directory
output.dir        <-  file.path(".","results/")        # figures directory
assessment_name_multifleet <- "HAWG2019_multifleet"
assessment_name_singlefleet <- "HAWG2019_singlefleet"

n.retro.years <- 5

load(file.path(output.dir,paste0('NSAS_WKNSMSE2018_sf_retro.Rdata')))

# SSB
SSB_2018   <- mohns.rho(NSH.retro,span=n.retro.years,ref.year=2017,type="ssb")
mean(SSB_2018$rho)
# fbar
fbar_2018   <- mohns.rho(NSH.retro,span=n.retro.years,ref.year=2017,type="fbar")
mean(fbar_2018$rho)
# recruitment
rec_2018   <- mohns.rho(NSH.retro,span=n.retro.years,ref.year=2017,type="rec")
mean(rec_2018$rho)

plot(NSH.retro)

load(paste(output.dir,"/NSH_HAWG2019_mf_retro.RData",sep=""))

# SSB
SSB_2019   <- mohns.rho(NSH.retro,span=n.retro.years,ref.year=2018,type="ssb")
mean(SSB_2019$rho)
# fbar
fbar_2019   <- mohns.rho(NSH.retro,span=n.retro.years,ref.year=2018,type="fbar")
mean(fbar_2019$rho)
# recruitment
rec_2019   <- mohns.rho(NSH.retro,span=n.retro.years,ref.year=2018,type="rec")
mean(rec_2019$rho)

plot(NSH.retro)