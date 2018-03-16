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
#path <- "D:/Repository/ICES_HAWG/wg_HAWG/NSAS/benchmark/"
path <- "D:/git/wg_HAWG/NSAS/"
try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
output.dir          <-  file.path(".","results//")              # result directory
output.base         <-  file.path(output.dir,"NSH Assessment")  # Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  7                                       # Number of years for which to run the retrospective
assessment_name     <- 'NSH_HAWG2018_sf'

### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA)
source(file.path("./setupAssessmentObjects.r"))
source(file.path("./setupControlObject_sf.r"))
#source(file.path("../_Common/HAWG_Common_module.r"))

### ============================================================================
### ============================================================================
### ============================================================================
### Assessment
### ============================================================================
### ============================================================================
### ============================================================================
NSH.sam               <- FLSAM(NSH,NSH.tun,NSH.ctrl)
NSH                   <- NSH + NSH.sam

save(NSH,
     NSH.tun,
     NSH.ctrl,
     NSH.sam,
     file=file.path(output.dir,
                    paste(assessment_name,
                          '.Rdata')
                    ,sep=""))

### ============================================================================
### run retrospective
### ============================================================================
NSH.ctrl@residuals <- F
NSH.retro <- retro(NSH,NSH.tun,NSH.ctrl,retro=n.retro.years)

save(NSH,
     NSH.tun,
     NSH.ctrl,
     NSH.sam,
     NSH.retro,
     file=file.path(output.dir,
                    paste(assessment_name,
                          '_retro.Rdata'),
                    sep=""))

