rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nNSH likelyhood profiling\n=====================\n")

# local path
path <- "D:/git/wg_HAWG/NSAS/"
try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
output.dir          <-  file.path(".","results/")        # figures directory
output.base         <-  file.path(output.dir,"NSH Assessment")  # Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  10                                      # Number of years for which to run the retrospective

### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA)
source(file.path("./setupAssessmentObjects.r"))
source(file.path("./setupControlObject_sf.r"))

NSH@m <- NSH@m - 0.11

mOrig <- NSH@m
NSH.sams <- new("FLSAMs")
for(iM in seq(-0.1,0.25,0.05)){
  print(iM)
  NSH@m <- mOrig + iM
  NSH.sams[[ac(iM)]] <- FLSAM(NSH,NSH.tun,NSH.ctrl)
}


