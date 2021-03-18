rm(list=ls()); graphics.off(); start.time <- proc.time()[3]

library(ggplot2)

# local path
#path <- "D:/Repository/ICES_HAWG/wg_HAWG/NSAS/"
path <- "J:/git/wg_HAWG/NSAS/"

try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
dir.create("assessment",showWarnings = FALSE)

output.dir          <-  file.path(".","assessment/")              # result directory\

# 2020 HAWG results downloaded from sharepoint
load(file.path(output.dir,'NSH_HAWG2020_sf.Rdata'))
NSH_HAWG2020_SMS2016      <- NSH
NSH.sam_HAWG2020_SMS2016  <- NSH.sam

SSB_HAWG2020_SMS2016      <- ssb(NSH.sam_HAWG2020_SMS2016)
SSB_HAWG2020_SMS2016$run  <- 'HAWG2020_SMS2016'
fbar_HAWG2020_SMS2016     <- fbar(NSH.sam_HAWG2020_SMS2016)
fbar_HAWG2020_SMS2016$run <- 'HAWG2020_SMS2016'
M_HAWG2020_SMS2016        <- as.data.frame(NSH_HAWG2020_SMS2016@m)
M_HAWG2020_SMS2016$run    <- 'HAWG2020_SMS2016'

# 2021 HAWG results recomputed as for HAWG2020
load(file.path(output.dir,'NSH_HAWG2021_sf_SMS2016_0.11.Rdata'))
NSH_HAWG2021_SMS2016      <- NSH
NSH.sam_HAWG2021_SMS2016  <- NSH.sam

SSB_HAWG2021_SMS2016      <- ssb(NSH.sam_HAWG2021_SMS2016)
SSB_HAWG2021_SMS2016$run  <- 'HAWG2021_SMS2016'
fbar_HAWG2021_SMS2016     <- fbar(NSH.sam_HAWG2021_SMS2016)
fbar_HAWG2021_SMS2016$run <- 'HAWG2021_SMS2016'
M_HAWG2021_SMS2016        <- as.data.frame(NSH_HAWG2021_SMS2016@m)
M_HAWG2021_SMS2016$run    <- 'HAWG2021_SMS2016'
M_HAWG2021_SMS2016$data   <- M_HAWG2021_SMS2016$data-0.11

# 2021 HAWG results with SMS2019
load(file.path(output.dir,'NSH_HAWG2021_sf.Rdata'))
NSH_HAWG2021_SMS2019      <- NSH
NSH.sam_HAWG2021_SMS2019  <- NSH.sam
SSB_HAWG2021_SMS2019      <- ssb(NSH.sam_HAWG2021_SMS2019)
SSB_HAWG2021_SMS2019$run  <- 'HAWG2021_SMS2019'
fbar_HAWG2021_SMS2019     <- fbar(NSH.sam_HAWG2021_SMS2019)
fbar_HAWG2021_SMS2019$run <- 'HAWG2021_SMS2019'
M_HAWG2021_SMS2019        <- as.data.frame(NSH_HAWG2021_SMS2019@m)
M_HAWG2021_SMS2019$run    <- 'HAWG2021_SMS2019'

# combine df
ssb_all <- rbind(SSB_HAWG2021_SMS2019,SSB_HAWG2020_SMS2016)#SSB_HAWG2021_SMS2016,
ssb_all$quant <- 'SSB'
fbar_all <- rbind(fbar_HAWG2021_SMS2019,fbar_HAWG2020_SMS2016)#fbar_HAWG2021_SMS2016,
fbar_all$quant <- 'fbar'

df_all  <- rbind(ssb_all,fbar_all)

df_M    <- rbind(M_HAWG2021_SMS2019,M_HAWG2021_SMS2016,M_HAWG2020_SMS2016)
#df_M    <- rbind(M_HAWG2021_SMS2016,M_HAWG2020_SMS2016)

#df_M    <- rbind(M_HAWG2021_SMS2019,M_HAWG2020_SMS2016)

windows()
ggplot(df_all,aes(x=year,y=value,col=run))+
  geom_line()+
  facet_wrap(~quant,scales='free')

windows()
ggplot(df_M,aes(x=year,y=data,col=run))+
  geom_line()+
  facet_wrap(~age)
