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

library(ggplot2)

# local path
path <- "J:/git/wg_HAWG/NSAS/"

try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
dir.create("assessment",showWarnings = FALSE)

data.dir            <-  file.path(".","data/")
output.dir          <-  file.path(".","assessment/")              # result directory\

load(file.path(output.dir,paste0('NSH_HAWG2021_sf_scanM_SMS2016_0.1','.Rdata')))
NSH.sams_SMS2016_HAWG2020 <- NSH.sams

load(file.path(output.dir,paste0('NSH_HAWG2021_sf_scanM_SMS2019_0.1','.Rdata')))
NSH.sams_SMS2019_HAWG2020 <- NSH.sams

load(file.path(output.dir,paste0('NSH_WKPELA2018_sf_scanM_SMS2016_0.1','.Rdata')))
NSH.sams_SMS2016_WKPELA2018 <- NSH.sams

load(file.path(output.dir,paste0('NSH_WKPELA2018_sf_scanM_SMS2019_0.1','.Rdata')))
NSH.sams_SMS2019_WKPELA2018 <- NSH.sams



nlogl2016_HAWG2020 <- as.data.frame(unlist(lapply(NSH.sams_SMS2016_HAWG2020,nlogl)))
nlogl2016_HAWG2020$addM <- rownames(nlogl2016_HAWG2020)
colnames(nlogl2016_HAWG2020) <- c('nlogl','addM')
nlogl2016_HAWG2020$run <- 'SMS2016_HAWG2020'

nlogl2019_HAWG2020 <- as.data.frame(unlist(lapply(NSH.sams_SMS2019_HAWG2020,nlogl)))
nlogl2019_HAWG2020$addM <- rownames(nlogl2019_HAWG2020)
colnames(nlogl2019_HAWG2020) <- c('nlogl','addM')
nlogl2019_HAWG2020$run <- 'SMS2019_HAWG2020'

nlogl2016_WKPELA2018 <- as.data.frame(unlist(lapply(NSH.sams_SMS2016_WKPELA2018,nlogl)))
nlogl2016_WKPELA2018$addM <- rownames(nlogl2016_WKPELA2018)
colnames(nlogl2016_WKPELA2018) <- c('nlogl','addM')
nlogl2016_WKPELA2018$run <- 'SMS2016_WKPELA2018'

nlogl2019_WKPELA2018 <- as.data.frame(unlist(lapply(NSH.sams_SMS2019_WKPELA2018,nlogl)))
nlogl2019_WKPELA2018$addM <- rownames(nlogl2019_WKPELA2018)
colnames(nlogl2019_WKPELA2018) <- c('nlogl','addM')
nlogl2019_WKPELA2018$run <- 'SMS2019_WKPELA2018'

nlogl_all <- rbind(nlogl2016_HAWG2020,
                   nlogl2019_HAWG2020,
                   nlogl2016_WKPELA2018,
                   nlogl2019_WKPELA2018)

windows()
ggplot(nlogl_all,aes(x=addM,y=nlogl,col=run))+
  geom_point()
