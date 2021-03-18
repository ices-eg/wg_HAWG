### ============================================================================
### ============================================================================
### ============================================================================
### NSAS single fleet assessment
### ============================================================================
### ============================================================================
### ============================================================================

rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nNSH Final Assessment (single fleet)\n=====================\n")

# local path
path <- "J:/git/wg_HAWG/NSAS/"

try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
dir.create("assessment",showWarnings = FALSE)

data.dir            <-  file.path(".","data/")
output.dir          <-  file.path(".","assessment/")              # result directory
script.dir          <-  file.path(".","side_scripts/")            # result directory
assessment_name     <- 'NSH_HAWG2020_sf_scanM_SMS2019_0.01'

### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA)
addM <- 0
source(file.path(script.dir,"setupAssessmentObjects_sf.r"))
# window back to WKPELA2018
#NSH     <- window(NSH, end=2016)
#NSH.tun$HERAS <- window(NSH.tun$HERAS, end=2016)
#NSH.tun$`IBTS-Q3` <- window(NSH.tun$`IBTS-Q3`, end=2016)
#NSH.tun$`LAI-ORSH` <- window(NSH.tun$`LAI-ORSH`, end=2016)
#NSH.tun$`LAI-BUN` <- window(NSH.tun$`LAI-BUN`, end=2016)
#NSH.tun$`LAI-CNS` <- window(NSH.tun$`LAI-CNS`, end=2016)
#NSH.tun$`LAI-SNS` <- window(NSH.tun$`LAI-SNS`, end=2016)
#NSH.tun$`IBTS-Q1` <- window(NSH.tun$`IBTS-Q1`, end=2017)
#NSH.tun$`IBTS-Q3` <- window(NSH.tun$`IBTS-Q3`, end=2017)
#NSH.tun$IBTS0     <- window(NSH.tun$IBTS0, end=2017)
source(file.path(script.dir,"setupControlObject_sf.r"))
NSH.ctrl@residuals <- FALSE
NSH.ctrl@cor.F <- 2

# pre-run assessment for initial values
NSH.sam               <- FLSAM(NSH,NSH.tun,NSH.ctrl)
stk0.NSH.sam <- NSH.sam

mOrig <- NSH@m
NSH.sams  <- new("FLSAMs")
NSHs      <- new("FLStocks")
flagFirst <- TRUE
inc <- 0.1

for(iM in seq(-0.1,0.6,inc)){
  print(iM)
  NSH@m <- mOrig + iM
  NSHs[[ac(iM)]] <- NSH
  if(flagFirst == TRUE){
    NSH.sams[[ac(iM)]] <- FLSAM(NSH,NSH.tun,NSH.ctrl,starting.values = stk0.NSH.sam)
    flagFirst <- FALSE
  }else{
    NSH.sams[[ac(iM)]] <- FLSAM(NSH,NSH.tun,NSH.ctrl,starting.values = NSH.sams[[ac(iM-inc)]])#,starting.values = stk0.NSH.sam,,starting.values = NSH.sams[[ac(0.1)]]
  }
}

save(NSH.sams,
     NSHs,
     file=file.path(output.dir,paste0(assessment_name,'.Rdata')))


plot(seq(-0.1,0.6,0.1),unlist(lapply(NSH.sams,nlogl)))

#plot(seq(-0.1,0.6,0.01),unlist(lapply(NSH.sams,nlogl)))
#plot(seq(-0.1,0.6,0.1),unlist(lapply(NSH.sams,nlogl)))

plot(seq(-0.1,0.22,0.01),unlist(lapply(NSH.sams,nlogl)))