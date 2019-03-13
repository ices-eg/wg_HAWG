### ============================================================================
### ============================================================================
### ============================================================================
### Setup
### ============================================================================
### ============================================================================
### ============================================================================

rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nNSH Final Assessment (multi fleet)\n=====================\n")

# local path
#path <- "D:/Repository/ICES_HAWG/wg_HAWG/NSAS/"
path <- "C:/git/wg_HAWG/NSAS/"
try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
output.dir          <-  file.path(".","results//")              # result directory
output.base         <-  file.path(output.dir,"NSH Assessment")  # Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  7                                       # Number of years for which to run the retrospective
assessment_name     <- 'NSH_HAWG2018_mf'

### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA)
source(file.path("./setupAssessmentObjects_mf.r"))
source(file.path("./setupControlObject_mf.r"))

### ============================================================================
### Run the assessment
### ============================================================================

NSH3f.sam   <- FLSAM(NSHs3,
                     NSH.tun,
                     NSH3.ctrl)

save(NSHs3,
     NSH.tun,
     NSH3.ctrl,
     NSH3f.sam,
     file=file.path(output.dir,paste0(assessment_name,'.Rdata')))

### ============================================================================
### run retrospective
### ============================================================================
NSH3.ctrl@residuals <- F
NSH3f.retro             <- retro(NSHs3,NSH.tun,NSH3.ctrl,retro=n.retro.years)

save(NSHs3,
     NSH.tun,
     NSH3.ctrl,
     NSH3f.sam,
     NSH3f.retro,
     file=file.path(output.dir,
                    paste0(assessment_name,'_retro.Rdata')))

