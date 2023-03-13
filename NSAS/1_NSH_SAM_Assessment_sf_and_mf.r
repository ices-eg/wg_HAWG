### ============================================================================
### ============================================================================
### ============================================================================
### NSAS single fleet and multi fleet assessments
### ============================================================================
### ============================================================================
### ============================================================================

rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nNSH Final Assessment (single fleet)\n=====================\n")

# local path
#path <- "D:/Repository/ICES_HAWG/wg_HAWG/NSAS/"
#path <- "J:/git/wg_HAWG/NSAS/"
#path <- "C:/git/2020_her.27.3a47d_multifleet/"
#path <- "C:/git/2020_her.27.3a47d_assessment/"
#path <- "C:/git/2020_her.27.3a47d_forecast/"
path <- "D:/git/wg_HAWG/NSAS/"
try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
dir.create("assessment",showWarnings = FALSE)

output.dir          <-  file.path(".","assessment/")              # result directory\
script.dir          <-  file.path(".","side_scripts/")            # result directory
n.retro.years       <-  12                                        # Number of years for which to run the retrospective
run_name                <- 'NSH_HAWG2021_M0.11'#_SMS2016
assessment_name         <- paste0(run_name,'_sf')

### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA)
source(file.path(script.dir,"setupAssessmentObjects_sf.r"))
source(file.path(script.dir,"setupControlObject_sf.r"))

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

### ============================================================================
### produce stock replicates
### ============================================================================

# nits            <- 100
# NSH.sim         <- simulate(NSH,NSH.tun,NSH.ctrl,n=nits)
# NSH.sam         <- FLSAM(NSH,NSH.tun,NSH.ctrl)
# NSH@stock.n     <- NSH.sam@stock.n[,ac(range(NSH)["minyear"]:range(NSH)["maxyear"])]
# NSH@harvest     <- NSH.sam@harvest[,ac(range(NSH)["minyear"]:range(NSH)["maxyear"])]
# save(NSH,
#      NSH.tun,
#      NSH.ctrl,
#      NSH.sam,
#      NSH.sim,
#      file=file.path(output.dir,
#                     paste0(assessment_name,'_replicates.Rdata')))

### ============================================================================
### Now run the multifleet model
### ============================================================================

assessment_name         <- paste0(run_name,'_mf')

load(file.path(output.dir,'NSH_HAWG2020_mf.Rdata'))
NSH3f.sam.stk0 <- NSH3f.sam

source(file.path(script.dir,"setupAssessmentObjects_mf.r"))
source(file.path(script.dir,"setupControlObject_mf.r"))


### ============================================================================
### Run the assessment
### ============================================================================

NSH3f.sam   <- FLSAM(NSHs3,
                     NSH.tun,
                     NSH3.ctrl,starting.values = NSH3f.sam.stk0)

save(NSHs3,
     NSH.tun,
     NSH3.ctrl,
     NSH3f.sam,
     file=file.path(output.dir,paste0(assessment_name,'.Rdata')))

### ============================================================================
### run retrospective
### ============================================================================

# NSH3.ctrl@residuals <- F
# NSH3f.retro             <- retro(NSHs3,NSH.tun,NSH3.ctrl,retro=n.retro.years)
# save(NSHs3,
#      NSH.tun,
#      NSH3.ctrl,
#      NSH3f.sam,
#      NSH3f.retro,
#      file=file.path(output.dir,
#                     paste0(assessment_name,'_retro.Rdata')))


# Check the consistency

# library(tidyverse)
# bind_rows(
#     as.data.frame(ssb(NSH.sam)) %>% mutate(assess="sf"),        
#     as.data.frame(ssb(NSH3f.sam)) %>% mutate(assess="mf"),        
#   ) %>% 
#         filter(year >= 2000) %>% 
#   ggplot(aes(year, value, colour=assess)) +
#         geom_line() +
#         geom_ribbon(aes(fill=assess, ymin=lbnd, ymax=ubnd), alpha=0.5) +
#         expand_limits(y=0)

