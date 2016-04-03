################################################################################
#
# WBSS retrospective analysis
#
# 30 March 2016
# HAWG working group
#
################################################################################

rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nWBSS Final Assessment\n=====================\n")

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
output.dir          <-  file.path(".","results")                #Output directory
output.base         <-  file.path(output.dir,"WBSS Assessment")  #Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  5                                       #Number of years for which to run the retrospective

### ============================================================================
### Setup assessment
### ============================================================================
#Import externals
library(FLSAM);
source(file.path("retroResidual.R"))
source(file.path("retro_param.R"))

#Load the output data of the assessment
res <- try(load(file=file.path(output.dir,"WBSS.RData")),silent=TRUE)
if(class(res)=="try-error") stop("Run the assessment first before you can run the retrospective")

# *** adjust obs.vars of catches
# *** required at HAWG.2016 to help the model converging
source("setupControlObject_warmUp.R")
## wbss.ctrl@obs.vars[1,] <- c(1,rep(2,3),rep(3,5))

#Perform retrospective
wbss.retro <- retro(wbss,wbss.tun,stk.ctrl,n.retro.years)

# Save results
save(wbss.retro,file=file.path(output.dir,paste("WBSS_retro.RData",sep="")))

#Setup plots
pdf(file.path(output.dir,paste("WBSS_retro.pdf",sep="")))
#Plot retro
print(plot(wbss.retro,futureYrs=F))

# ad-hoc adjustment of fleet names!!! ***ODD, IT STOPPED TO WORK AT HAWG2015!
for(i in 1:6){
    wbss.retro[i]@.Data[[1]]@residuals$fleet <- as.character(wbss.retro[i]@.Data[[1]]@residuals$fleet)
    wbss.retro[i]@.Data[[1]]@residuals$fleet <- ifelse(wbss.retro[i]@.Data[[1]]@residuals$fleet=="Fleet 1", "catch",
                                                         ac(wbss.retro[i]@.Data[[1]]@residuals$fleet))
    wbss.retro[i]@.Data[[1]]@residuals$fleet <- ifelse(wbss.retro[i]@.Data[[1]]@residuals$fleet=="Fleet 2", "HERAS",
                                                         ac(wbss.retro[i]@.Data[[1]]@residuals$fleet))
    wbss.retro[i]@.Data[[1]]@residuals$fleet <- ifelse(wbss.retro[i]@.Data[[1]]@residuals$fleet=="Fleet 3", "GerAS",
                                                         ac(wbss.retro[i]@.Data[[1]]@residuals$fleet))
    wbss.retro[i]@.Data[[1]]@residuals$fleet <- ifelse(wbss.retro[i]@.Data[[1]]@residuals$fleet=="Fleet 4", "N20",
                                                         ac(wbss.retro[i]@.Data[[1]]@residuals$fleet))
    wbss.retro[i]@.Data[[1]]@residuals$fleet <- ifelse(wbss.retro[i]@.Data[[1]]@residuals$fleet=="Fleet 5", "IBTS Q1",
                                                         ac(wbss.retro[i]@.Data[[1]]@residuals$fleet))
    wbss.retro[i]@.Data[[1]]@residuals$fleet <- ifelse(wbss.retro[i]@.Data[[1]]@residuals$fleet=="Fleet 6", "IBTS Q3",
                                                         ac(wbss.retro[i]@.Data[[1]]@residuals$fleet))
}
                         
                               
print(retroResiduals(   wbss.retro,"HERAS",1991:2014))
print(retroResiduals(   wbss.retro,"GerAS",1991:2014))
print(retroResiduals(   wbss.retro,"IBTS Q1",1991:2014))
print(retroResiduals(   wbss.retro,"IBTS Q3",1991:2014))
print(retroResiduals(   wbss.retro,"catch",1991:2014))
print(retroSelectivity( wbss.retro,2003:2014))
print(lapply(wbss.retro,retroParam))

dev.off()

png(paste(output.base,"figure_retroF.png"),units = "px", height=500,width=550,pointsize = 12, bg = "white")
print(retroSelectivity( wbss.retro,2003:2014))
dev.off()

