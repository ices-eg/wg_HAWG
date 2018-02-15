rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nNSH Final Assessment\n=====================\n")

# local path
path <- "D:/Repository/ICES_HAWG/wg_HAWG/NSAS/benchmark/"
try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
output.dir          <-  file.path(".","results/7_finalModel/")        # figures directory
output.base         <-  file.path(output.dir,"NSH Assessment")  # Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  10                                      # Number of years for which to run the retrospective


### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA)
source(file.path("R/6_multifleet/setupAssessmentObjects_LAI.r"))
source(file.path("R/6_multifleet/setupControlObject_sf.r"))

mOrig <- NSH@m
NSH.sams <- new("FLSAMs")
for(iM in seq(-0.1,0.6,0.01)){
  print(iM)
  NSH@m <- mOrig + iM
  NSH.sams[[ac(iM)]] <- FLSAM(NSH,NSH.tun,NSH.ctrl)
}

plot(unlist(lapply(NSH.sams,nlogl)))
save(NSH.sams,file="D:/Repository/ICES_HAWG/wg_HAWG/NSAS/benchmark/results/2_newM/scanM.RData")