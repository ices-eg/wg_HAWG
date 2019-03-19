######################################################################################################
# Malin FLSAM Assessment
# Updated March 2019
####################################################################################################
## R version 3.5.2


rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)



# Version use March 2018
library(FLSAM)  # 2.1.0
library(FLCore) # 2.6.12
library(FLEDA)  # 2.5.2

log.msg     <-  function(fmt,...) {
  cat(sprintf(fmt,...))
  flush.console()
}

log.msg("\nHerring in VIa and VIIbc FLSAM Assessment\n===================================\n")


### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
##set path to the run 

path<-"C:HAWG 2019/Northwest/FLSAM/MF/HAWG run"



try(setwd(path),silent=TRUE)

data.source         <-  file.path(".","data")     #Data source, not code or package source!!!
indices.data        <- file.path(".","indices.data")
output.dir          <-  file.path(".","results")  #Output directory
output.base         <-  file.path(output.dir)     #Output base filename, including directory. Other output filenames are built by appending onto this one


### ============================================================================
### set the run name
### ============================================================================

Run_Name <- "Run_1_MF"



### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================

log.msg("PREPARING STOCK OBJECT...\n")

MSH<- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)



### ======================================================================================================
### Define parameters for use in the assessment code here

MSH@catch.n                 <- MSH@landings.n
MSH@catch                   <- MSH@landings
MSH@catch.wt                <- MSH@landings.wt
MSH@stock.wt[1,ac(2013)]   <- yearMeans(MSH@stock.wt[1,ac(2010:2012)])
MSH@stock.wt[1,ac(2015)]   <- yearMeans(MSH@stock.wt[1,ac(2010:2014)])
MSH@stock.wt[1,ac(2016)]   <- yearMeans(MSH@stock.wt[1,ac(2010:2015)])
MSH@stock.wt[1,ac(2017)]   <- yearMeans(MSH@stock.wt[1,ac(2010:2016)])
MSH@stock.wt[1,ac(2018)]    <- yearMeans(MSH@stock.wt[1,ac(2010:2017)])
units(MSH)[1:17]            <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))

#Set fbar
range(MSH)[c("minfbar","maxfbar")] <- c(3,6)

#Set plus group
MSH <- setPlusGroup(MSH,MSH@range["max"])

#- Functions to set the plusgroups
setMatrixPlusGroupN <- function(x){
  x[nrow(x)-1,] <- colSums(x[(nrow(x)-1):(nrow(x)),],na.rm=T)
  return(x[-nrow(x),])}
setMatrixPlusGroupWt <- function(x,y){
  y[nrow(x)-1,] <- colSums(x[(nrow(x)-1):(nrow(x)),]*y[(nrow(y)-1):(nrow(y)),],na.rm=T) / colSums(x[(nrow(x)-1):(nrow(x)),],na.rm=T)
  y[is.nan(y)] <- 0
  return(y[-nrow(y),])}

VIaN_canum  <- readVPAFile(file.path(indices.data,"VIaN_canum.txt"))
VIaS_canum  <- readVPAFile(file.path(indices.data,"VIaS_canum.txt"))
VIaN_weca   <- readVPAFile(file.path(indices.data,"VIaN_weca.txt"))
VIaS_weca   <- readVPAFile(file.path(indices.data,"VIaS_weca.txt"))

MSHm <- FLCore::expand(MSH,area=c("N","S"))
MSHm@catch.n[,,,,"N"]   <- VIaN_canum
MSHm@catch.wt[,,,,"N"]  <- VIaN_weca
MSHm@catch.n[,,,,"S"]   <- VIaS_canum
MSHm@catch.wt[,,,,"S"]  <- VIaS_weca
MSHm@landings.n         <- MSHm@catch.n
MSHm@landings.wt        <- MSHm@catch.wt
MSHm@landings           <- computeLandings(MSHm)
MSHm@catch              <- computeCatch(MSHm)


#Set stock object name - this is propagated through into the figure titles
MSHm@name               <- "Herring in 6aN and 6aS,7bc multifleet"


### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
log.msg("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
MSH.tun   <- readFLIndices(file.path(data.source, "fleet.txt"))

## Set the Index Type
MSH.tun[[1]]@type               <- "number"
MSH.tun[[2]]@type               <- "number"
MSH.tun[[3]]@type               <- "number"


## Call the index whatever
MSH.tun[[1]]@name               <- "WOS_MSHAS"
MSH.tun[[2]]@name               <- "IBTS_Q1"
MSH.tun[[3]]@name               <- "IBTS_Q4"


## Set the plus group or specify there isnt one
MSH.tun[[1]]@range["plusgroup"] <- 9
MSH.tun[[2]]@range["plusgroup"] <- 9
MSH.tun[[3]]@range["plusgroup"] <- 9


#Set names
names(MSH.tun)                  <- lapply(MSH.tun,name)

## Trim indices to have the required ages


MSH.tun                         <- FLIndices("WOS_MSHAS"=MSH.tun[[1]],"IBTS_Q1"=trim(MSH.tun[[2]],age=2:9),
                                             "IBTS_Q4"=trim(MSH.tun[[3]],age=2:9))




### ======================================================================================================
### Prepare FLSAM object
### ======================================================================================================
MSH.ctrl <- FLSAM.control(MSHm,MSH.tun)

#All fishing mortality states are free except oldest ages to ensure stablity - default setting here


#Correlated Random walks for fishing mortalities -0= independent, 1= fully correlated, 2= correlation declines with more dis-tance between ages )
MSH.ctrl@cor.F                              <- 2

# Catchabilities
#Default values

#Fishing mortality RWs 
MSH.ctrl@f.vars["catch S",]            <- c(1,2,rep(3,5),rep(4,2))
MSH.ctrl@f.vars["catch N",]            <- c(1,rep(2,8)) + 101


#Binds the variances of the numbers at age random walks together
MSH.ctrl@logN.vars[]                        <- c(1,rep(2,8))

#Bind the observation variances
MSH.ctrl@obs.vars["catch S",]               <- c(001,002,rep(003,4),rep(004,3))
MSH.ctrl@obs.vars["catch N",1:9]            <- c(101,102,rep(103,5),rep(104,2))
MSH.ctrl@obs.vars["WOS_MSHAS",1:9]           <- c(201,202,rep(203,4),rep(204,3))
MSH.ctrl@obs.vars["IBTS_Q1",2:9]            <- c(    302,rep(303,3),rep(304,2),rep(305,2))
MSH.ctrl@obs.vars["IBTS_Q4",2:9]            <- c(    402,rep(403,4),rep(404,3))

#- Correlation structure in the indices
MSH.ctrl@cor.obs["WOS_MSHAS",]               <- c(0,0,rep(1,6))
MSH.ctrl@cor.obs["IBTS_Q1",2:8]             <- c(rep(202,7))
MSH.ctrl@cor.obs["IBTS_Q4",2:8]             <- c(302,303,304,rep(305,4))
MSH.ctrl@cor.obs.Flag[3:5]                  <- as.factor("AR")

MSH.ctrl@residuals                          <- TRUE
MSH.ctrl                                    <- update(MSH.ctrl)

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
MSHm.ctrl            <- MSH.ctrl
MSHm.sam             <- FLSAM(MSHm,MSH.tun,MSH.ctrl)




## Retro
MSH.ctrl@residuals   <- FALSE
MSHm.retro           <- retro(MSHm,MSH.tun,MSH.ctrl,7)
save(MSHm,MSHm.sam,MSH.tun,MSH.ctrl,MSHm.retro,file=file.path(output.dir,paste0("6aHerring_",run_name,"_MF.Rdata")))


#### create plots
run_name<- "Run_1_hawg"

source("./createAssessmentPlotsMF.r")


dev.off()





### ======================================================================================================
### Document Assessment
### ======================================================================================================


#source(file.path(,"_Common","FLSAM.out.r"))
output.dir          <-  file.path(".","results")                #Output directory
log.msg("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt           <- options("width","scipen")
options("width"=80,"scipen"=1000)
units(MSH@harvest)<-'f'
units(MSHm@harvest)<-'f'

#single fleet
sam.out.file<- FLSAM.out(MSH,MSH.tun,MSHm.sam,format="TABLE 4.6.%i Herring in 6a and 7bc.")
write(sam.out.file,file.path(output.dir,"MSH_sam.out",sep=".")) #or could create output.base
writeFLStock(MSH,output.file=file.path(output.dir,"Her27_6a7bc_"))

### multi fleet option

sam.out.file2<- FLSAM.out(MSHm,MSH.tun,MSHm.sam,format="TABLE 4.6.%i Herring in 6a and 7bc.")
write(sam.out.file2,file.path(output.dir,"MSHm_sam.out",sep=".")) #or could create output.base


options("width"=old.opt$width,"scipen"=old.opt$scipen)


stockSummaryTable <- cbind(rec(MSHm.sam)$year,
                           rec(MSHm.sam)$value,      rec(MSHm.sam)$lbnd,    rec(MSHm.sam)$ubnd,
                           tsb(MSHm.sam)$value,      tsb(MSHm.sam)$lbnd,    tsb(MSHm.sam)$ubnd,
                           ssb(MSHm.sam)$value,      ssb(MSHm.sam)$lbnd,    ssb(MSHm.sam)$ubnd,
                           catch(MSHm.sam)$value,    catch(MSHm.sam)$lbnd,  catch(MSHm.sam)$ubnd,
                           catch(MSHm.sam)$value / ssb(MSHm.sam)$value, catch(MSHm.sam)$lbnd / ssb(MSHm.sam)$lbnd, catch(MSHm.sam)$ubnd / ssb(MSHm.sam)$ubnd,
                           fbar(MSHm.sam)$value,     fbar(MSHm.sam)$lbnd,   fbar(MSHm.sam)$ubnd,
                           c(sop(MSH)), c(catch(MSH)))

colnames(stockSummaryTable) <-
  c("Year",paste(rep(c("Recruits Age 0 (Thousands)","Total biomass (tonnes)","Spawing biomass (tonnes)",
                       "Landings (tonnes)","Yield / SSB (ratio)","Mean F ages 3-6"),each=3),c("Mean","Low","High")),"SoP (%)","WG Catch")


#write(stockSummaryTable,file.path(output.dir,"stocksummarytable",sep=".")) #or could create output.base
write.csv(stockSummaryTable,file=file.path(output.dir,"stockSummaryTable.csv"),row.names = FALSE)



### ======================================================================================================
### 
### ======================================================================================================
pdf(paste(output.base,paste(Run_Name,"residuals.pdf",sep="")))

#Bubble plot of survey residuals
# Surveys are in the same order as the input file
res.dat <- subset(residuals(MSHm.sam),fleet=="WOS_MSHAS")
res.dat$data <- res.dat$std.res
p <- xyplot(age  ~ year |fleet,res.dat,
            cex=4*abs(res.dat$std.res)/max(abs(res.dat$std.res))+0.1,
            col=ifelse(res.dat$std.res<0,"red","black"),
            pch=16,ylab="age (ring)")
print(p)


res2.dat <- subset(residuals(MSHm.sam),fleet=="IBTS_Q1")
res2.dat$data <- res2.dat$std.res
p <- xyplot(age  ~ year |fleet,res2.dat,
            cex=4*abs(res2.dat$std.res)/max(abs(res2.dat$std.res))+0.1,
            col=ifelse(res2.dat$std.res<0,"red","black"),
            pch=16,ylab="age (ring)")
print(p)

res3.dat <- subset(residuals(MSHm.sam),fleet=="IBTS_Q4")
res3.dat$data <- res3.dat$std.res
p <- xyplot(age  ~ year |fleet,res3.dat,
            cex=4*abs(res3.dat$std.res)/max(abs(res3.dat$std.res))+0.1,
            col=ifelse(res3.dat$std.res<0,"red","black"),
            pch=16,ylab="age (ring)")
print(p)


## bubble plot of catch residuals
res.dat <- subset(residuals(MSHm.sam),fleet=="catch N")
res.dat$data <- res.dat$std.res
p <- xyplot(age  ~ year |fleet,res.dat,
            cex=4*abs(res.dat$std.res)/max(abs(res.dat$std.res))+0.1,
            col=ifelse(res.dat$std.res<0,"red","black"),
            pch=16,ylab="age (ring)")
print(p)

## bubble plot of catch residuals
res.dat <- subset(residuals(MSHm.sam),fleet=="catch S")
res.dat$data <- res.dat$std.res
p <- xyplot(age  ~ year |fleet,res.dat,
            cex=4*abs(res.dat$std.res)/max(abs(res.dat$std.res))+0.1,
            col=ifelse(res.dat$std.res<0,"red","black"),
            pch=16,ylab="age (ring)")
print(p)


dev.off()



