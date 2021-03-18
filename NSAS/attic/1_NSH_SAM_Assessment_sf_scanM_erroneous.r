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
assessment_name     <- 'NSH_WKPELA2018_sf_scanM_SMS2016_0.1_error_cor.F=2'

### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA)
addM <- 0
source(file.path(script.dir,"setupAssessmentObjects_sf.r"))
#source(file.path(script.dir,"setupControlObject_sf.r"))
#source(file.path(script.dir,"setupControlObject_sf_WKPELA_error.r"))

NSH     <- window(NSH, end=2016)
NSH.tun$HERAS <- window(NSH.tun$HERAS, end=2016)
NSH.tun$`IBTS-Q3` <- window(NSH.tun$`IBTS-Q3`, end=2016)
NSH.tun$`LAI-ORSH` <- window(NSH.tun$`LAI-ORSH`, end=2016)
NSH.tun$`LAI-BUN` <- window(NSH.tun$`LAI-BUN`, end=2016)
NSH.tun$`LAI-CNS` <- window(NSH.tun$`LAI-CNS`, end=2016)
NSH.tun$`LAI-SNS` <- window(NSH.tun$`LAI-SNS`, end=2016)
NSH.tun$`IBTS-Q1` <- window(NSH.tun$`IBTS-Q1`, end=2017)
NSH.tun$`IBTS-Q3` <- window(NSH.tun$`IBTS-Q3`, end=2017)
NSH.tun$IBTS0     <- window(NSH.tun$IBTS0, end=2017)

NSH.tun$HERAS <- NSH.tun$HERAS[ac(2:8)]
NSH.tun$`IBTS-Q3` <- NSH.tun$`IBTS-Q3`[ac(0:4)]

source(file.path(script.dir,"setupControlObject_sf_WKPELA_error.r"))

#source(file.path(script.dir,"setupControlObject_sf.r"))

load(file.path(output.dir,'NSH_WKPELA2018_sf_scanM_SM2016_0.01_erroneous.RData'))

NSH.ctrl.error <- NSH.sams[['0']]@control

#NSH.ctrl@catchabilities <- NSH.ctrl.error@catchabilities
#NSH.ctrl@obs.vars <- NSH.ctrl.error@obs.vars
#NSH.ctrl@cor.F <- NSH.ctrl.error@cor.F
NSH.ctrl <- NSH.ctrl.error
NSH.ctrl@cor.F <- 2

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
    NSH.sams[[ac(iM)]] <- FLSAM(NSH,NSH.tun,NSH.ctrl)#,starting.values = stk0.NSH.sam,,starting.values = NSH.sams[[ac(0.1)]]
    flagFirst <- FALSE
  }else{
    NSH.sams[[ac(iM)]] <- FLSAM(NSH,NSH.tun,NSH.ctrl,starting.values = NSH.sams[[ac(iM-inc)]])#,starting.values = stk0.NSH.sam,,starting.values = NSH.sams[[ac(0.1)]]
  }
}

plot(seq(-0.1,0.6,0.1),unlist(lapply(NSH.sams,nlogl)))

#min(unlist(lapply(NSH.sams,nlogl)))


save(NSH.sams,
     NSHs,
     file=file.path(output.dir,paste0(assessment_name,'.Rdata')))




#plot(seq(-0.1,0.6,0.1),unlist(lapply(NSH.sams,nlogl)))
#plot(seq(-0.1,0.6,0.1),unlist(lapply(NSH.sams,nlogl)))

