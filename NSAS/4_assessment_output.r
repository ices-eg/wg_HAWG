### ============================================================================
### Document Assessment
###
### 18/03/2018 Added Standard Graph database output
### ============================================================================

# library(FLCore)
# library(FLSAM)

# WKPELA 2018
# load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/benchmarks/2018/wkherring/2014 Meeting docs/06. Data/NSAS/SAM/NSH_final.RData")

# HAWG 2018
#load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2018 Meeting docs1/05. Data/NSAS/NSH_HAWG2018_sf.Rdata")
rm(list=ls())

path <- "C:/git/wg_HAWG/NSAS/"
try(setwd(path),silent=FALSE)

setwd(file.path(path,'assessment'))

dir.create("output",showWarnings = FALSE)

setwd(path)

assessment_name <- "NSH_HAWG2020"

output.dir      <-  file.path(".","assessment")
function.dir    <-  file.path(".","functions")

# load stuff
source(file.path(function.dir,'FLSAM.out_local.r'))

load(file.path(output.dir,paste0(assessment_name,'_sf','.RData')))
load(file.path(output.dir,paste0(assessment_name,'_mf','.RData')))

# write and output sf and mf data
sam.out.file      <- FLSAM.out_local(NSH,NSH.tun,NSH.sam,format="Table 2.6.3.%i North Sea Herring single fleet assessment.")

write(sam.out.file,file=file.path(output.dir,'output',"sam.out_sf"))

sam.out.file      <- FLSAM.out_local(NSHs3$residual,NSH.tun,NSH3f.sam,format="Table 2.6.2.%i North Sea Herring multi-fleet assessment.")

write(sam.out.file,file=file.path(output.dir,'output',"sam.out_mf"))

##And finally, write the results out in the lowestoft VPA format for further analysis
writeFLStock(NSH,output.file=file.path(output.dir,'output',"NSAS_47d3_"))

stockSummaryTable <- cbind(rec(NSH.sam)$year,
                           rec(NSH.sam)$value,      rec(NSH.sam)$lbnd,    rec(NSH.sam)$ubnd,
                           tsb(NSH.sam)$value,      tsb(NSH.sam)$lbnd,    tsb(NSH.sam)$ubnd,
                           ssb(NSH.sam)$value,      ssb(NSH.sam)$lbnd,    ssb(NSH.sam)$ubnd,
                           catch(NSH.sam)$value,    catch(NSH.sam)$lbnd,  catch(NSH.sam)$ubnd,
                           catch(NSH.sam)$value / ssb(NSH.sam)$value, catch(NSH.sam)$lbnd / ssb(NSH.sam)$lbnd, catch(NSH.sam)$ubnd / ssb(NSH.sam)$ubnd,
                           fbar(NSH.sam)$value,     fbar(NSH.sam)$lbnd,   fbar(NSH.sam)$ubnd,
                           c(quantMeans(harvest(NSH.sam)[ac(0:1),])),
                           c(sop(NSH),NA),
                           c(catch(NSH),NA))
colnames(stockSummaryTable) <-
  c("Year",paste(rep(c("Recruits Age 0 (Thousands)","Total biomass (tonnes)","Spawing biomass (tonnes)",
                       "Landings (tonnes)","Yield / SSB (ratio)","Mean F ages 2-6"),each=3),c("Mean","Low","High")),"Mean F ages 0-1","SoP (%)","WG Catch")
stockSummaryTable[nrow(stockSummaryTable),] <- NA
stockSummaryTable[nrow(stockSummaryTable),"Spawing biomass (tonnes) Mean"] <- 2271364
stockSummaryTable[nrow(stockSummaryTable),2:4] <- c(rec(NSH.sam)$value[nrow(rec(NSH.sam))],rec(NSH.sam)$lbnd[nrow(rec(NSH.sam))],rec(NSH.sam)$ubnd[nrow(rec(NSH.sam))])

# write.csv(stockSummaryTable,file=file.path(output.dir,paste(name(NSH),"stockSummaryTable.csv",sep="_")))
#write.csv(stockSummaryTable,file=file.path("NSAS","stockSummaryTable.csv"))
write.csv(stockSummaryTable,file=file.path(output.dir,'output',"stockSummaryTable.csv"))



options("width"=old.opt$width,"scipen"=old.opt$scipen)

load(paste(output.dir,"/NSH_HAWG2020_sf_retro.RData",sep=""))

SSB_mr  <- mean(mohns.rho(NSH.retro,span=5,ref.year=2019,type="ssb")$rho)
fbar_mr <- mean(mohns.rho(NSH.retro,span=5,ref.year=2019,type="fbar")$rho)
rec_mr  <- mean(mohns.rho(NSH.retro,span=5,ref.year=2019,type="rec")$rho)

mean(mohns.rho(NSH.retro,span=5,ref.year=2020,type="fbar")$rho)

# output for cindy
write.csv(components(NSH.sam),file = file.path(output.dir,'output','LAI_prop.csv'))
write.csv(rec(NSH.sam),file = file.path(output.dir,'output','rec_sf.csv'),row.names = FALSE)
write.csv(ssb(NSH.sam),file = file.path(output.dir,'output','SSB_sf.csv'),row.names = FALSE)
write.csv(fbar(NSH.sam),file = file.path(output.dir,'output','fbar_sf.csv'),row.names = FALSE)