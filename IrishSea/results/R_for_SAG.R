setwd("C:\\Users\\Matt Lundy\\Documents\\GIT_HUB\\wg_HAWG\\IrishSea\\UpdateAssessment\\SAM\\results")

cat("# Standard Graphs personal access token",
    "SG_PAT=074843fa-10ce-4315-a153-f4d27372c5cf",
    sep = "\n",
    file = "~/.Renviron_SG")

SSBint<-24786
GMrec<-274883

# ---------------------------------------------------------------------------- 
# Add assessment output to SAG database 
#---------------------------------------------------------------------------- 
setwd("C:\\Users\\Matt Lundy\\Documents\\GIT_HUB\\wg_HAWG\\IrishSea\\UpdateAssessment\\SAM\\results")

maindir <- 'C:\\Users\\Matt Lundy\\Documents\\GIT_HUB\\wg_HAWG\\IrishSea\\UpdateAssessment\\SAM'
outdir <- paste0(maindir,'Outputs')
nirs_sum<-read.csv("stockSummaryTableSAG.csv", header = T, sep = ",")

library(devtools) 
devtools::install_github("ices-tools-prod/icesSAG") 
 
library(icesSAG) 
library(tidyverse) 
 

# login to ICES SAG, generate a token and put it in the ~/.Renviron_SG 

# make use of the token 
options(icesSAG.use_token = TRUE) 
info     <- stockInfo(StockCode="her.27.nirs", AssessmentYear = 2020, ContactPerson = "mathieu.lundy@afbini.gov.uk") 
 
FiY       <- min(nirs_sum$Year)  
DtY       <- max(nirs_sum$Year) 
LaY       <- max(nirs_sum$Year) 
nyrs      <- ((LaY)-(FiY))+1
 

fishdata <- stockFishdata((FiY):((LaY)+1)) 

info$StockCategory             <- "1" 
info$MSYBtrigger               <- 11831
info$Blim                      <- 8500
info$Bpa                       <- 11831
info$Flim                      <- 0.397
info$Fpa                       <- 0.286
info$FMSY                      <- 0.266
info$Fage                      <- "4-6" 
info$RecruitmentAge            <- 1 
info$RecruitmentDescription    <- "WR" 
info$RecruitmentUnits          <- "NE3" 
info$FishingPressureDescription<- "F" 
info$ConfidenceIntervalDefinition<-"CV"
info$FishingPressureUnits      <- NA 
info$StockSizeDescription      <- "SSB" 
info$StockSizeUnits            <- "t" 
info$Purpose                   <- "Advice" 
 
fishdata$Catches[1:nyrs]              <-nirs_sum$Land[1:nyrs]
fishdata$Landings[1:nyrs]             <- nirs_sum$Land[1:nyrs] 
fishdata$Discards                     <- 0

fishdata$Low_Recruitment[1:nyrs]      <- nirs_sum$RecruitsLow[1:nyrs] 
fishdata$Recruitment                  <- c(nirs_sum$RecruitsMean[1:nyrs], GMrec)
fishdata$High_Recruitment[1:nyrs]     <- nirs_sum$RecruitsHigh[1:nyrs] 


fishdata$TBiomass[1:nyrs]             <- nirs_sum$TSB[1:nyrs] 

fishdata$StockSize                    <- c(nirs_sum$SSB[1:nyrs],SSBint) 
fishdata$Low_StockSize[1:nyrs]        <- nirs_sum$SSBL[1:nyrs] 
fishdata$High_StockSize[1:nyrs]       <- nirs_sum$SSBH[1:nyrs] 

fishdata$FishingPressure[1:nyrs]      <- nirs_sum$Fbar[1:nyrs] 
fishdata$High_FishingPressure[1:nyrs]      <- nirs_sum$Fhigh[1:nyrs] 
fishdata$Low_FishingPressure[1:nyrs]      <- nirs_sum$Flow[1:nyrs] 

key <- icesSAG::uploadStock(info, fishdata) 
 
