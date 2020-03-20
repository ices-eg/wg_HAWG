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
#path <- "D:/Repository/ICES_HAWG/wg_HAWG/NSAS/"
path <- "C:/git/wg_HAWG/NSAS/"
#path <- "D:/git/wg_HAWG/NSAS/"
try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
output.dir          <-  file.path(".","results//")              # result directory
output.base         <-  file.path(output.dir,"NSH Assessment")  # Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  12                                       # Number of years for which to run the retrospective
#assessment_name     <- 'NSH_HAWG2020_sf_alt4'
assessment_name     <- 'NSH_HAWG2020_sf'

### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA)
source(file.path("./setupAssessmentObjects_sf.r"))
source(file.path("./setupControlObject_sf.r"))
#source(file.path("./setupControlObject_sf_alt.r"))
source(file.path("../_Common/HAWG_Common_module.r"))

### ============================================================================
### ============================================================================
### ============================================================================
### Assessment
### ============================================================================
### ============================================================================
### ============================================================================
NSH.sam               <- FLSAM(NSH,NSH.tun,NSH.ctrl)
NSH@stock.n           <- NSH.sam@stock.n[,ac(range(NSH)["minyear"]:range(NSH)["maxyear"])]
NSH@harvest           <- NSH.sam@harvest[,ac(range(NSH)["minyear"]:range(NSH)["maxyear"])]

save(NSH,
     NSH.tun,
     NSH.ctrl,
     NSH.sam,
     file=file.path(output.dir,paste0(assessment_name,'.Rdata')))

### ============================================================================
### run retrospective
### ============================================================================
NSH.ctrl@residuals <- F
NSH.retro             <- retro(NSH,NSH.tun,NSH.ctrl,retro=n.retro.years)

save(NSH,
     NSH.tun,
     NSH.ctrl,
     NSH.sam,
     NSH.retro,
     file=file.path(output.dir,
                    paste0(assessment_name,'_retro.Rdata')))

write.csv(mohns.rho(retro = NSH.retro,ref.year = 2019,span = 7,type = 'ssb'),
          file.path(output.dir,
                    paste0(assessment_name,'_mohn_rho.csv')),row.names = TRUE)


### ============================================================================
### produce stock replicates
### ============================================================================

nits            <- 100
NSH.sim         <- simulate(NSH,NSH.tun,NSH.ctrl,n=nits)
NSH.sam         <- FLSAM(NSH,NSH.tun,NSH.ctrl)
NSH@stock.n     <- NSH.sam@stock.n[,ac(range(NSH)["minyear"]:range(NSH)["maxyear"])]
NSH@harvest     <- NSH.sam@harvest[,ac(range(NSH)["minyear"]:range(NSH)["maxyear"])]

save(NSH,
     NSH.tun,
     NSH.ctrl,
     NSH.sam,
     NSH.sim,
     file=file.path(output.dir,
                    paste0(assessment_name,'_replicates.Rdata')))
