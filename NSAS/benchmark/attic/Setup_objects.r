################################################################################
# NSH_Setup_Objects
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Sets up the data objects (specifically an FLStock and an FLIndices object)
# necessary to perform a stock assessment. Necessary preprocessing, such as
# the smoothing of weights, is performed here. Script is intended to be called
# from other external sources.
#
# Developed with:
#   - R version 2.13.1
#   - FLCore 2.4
#
# To be done:
#
# Notes: Have fun running this assessment!
#
################################################################################

### ============================================================================
### Misc
### ============================================================================
data.source         <- file.path(".","data")    #Data source, not code or package source!!!

### ============================================================================
### Prepare stock object for assessment
### ============================================================================
#Load object
NSH                 <- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)

#Catch is calculated from: catch.wt * catch.n, however, the reported landings are
#normally different (due to SoP corrections). Hence we overwrite the calculate landings
NSH@catch           <- NSH@landings
units(NSH)[1:17]    <- as.list(c(rep(c("tonnes","thousands","kg"),4), 
                               rep("NA",2),"f",rep("NA",2)))

#Set object details
NSH@name                              <- "North Sea Herring"
range(NSH)[c("minfbar","maxfbar")]    <- c(2,6)
NSH                                   <- setPlusGroup(NSH,NSH@range["max"])

#Historical data is only provided for ages 0-8 prior to 1960 (rather than 0-9 after)
#We therefore need to fill in the last ages by applying the following assumptions
#  - weight in the catch and weight in the stock at age 9 is the same as 
#    in period 1960-1983 (constant in both cases)
#  - catch at age reported as 8+ is split evenly between age 8 and 9
#  - natural mortality at age 9 is 0.1 (same as age 8)
#  - proportion mature at age 9 is 1.0 (same as age 8)
#  - harvest.spwn and m.spwn are the same as elsewhere
hist.yrs                      <- as.character(1947:1959)
NSH@catch.wt                  <- NSH@landings.wt #Automatic population of catch.wt introduces NAs
NSH@catch.wt["9",hist.yrs]    <- 0.271
NSH@landings.wt["9",hist.yrs] <- 0.271
NSH@catch.n["9",hist.yrs]     <- NSH@catch.n["8",hist.yrs]/2
NSH@catch.n["8",hist.yrs]     <- NSH@catch.n["9",hist.yrs]
NSH@landings.n["9",hist.yrs]  <- NSH@landings.n["8",hist.yrs]/2
NSH@landings.n["8",hist.yrs]  <- NSH@landings.n["9",hist.yrs]
NSH@stock.wt["9",hist.yrs]    <- 0.312
NSH@m["9",hist.yrs]           <- 0.1
NSH@mat["9",hist.yrs]         <- 1

#No catches of age 9 in 1977 so stock.wt does not get filled there.
#Hence, we copy the stock weight for that age from the previous year.
#Note that because we use a fixed stock.wt prior to 1983, there is no
#need to use averaging or anything fancier
NSH@stock.wt["9","1977"]              <- NSH@stock.wt["9","1976"]

#Use a running mean(y-2,y-1,y) of input wests (i.e. west_raw) to calculate west
NSH@stock.wt[,3:dim(NSH@stock.wt)[2]] <- (NSH@stock.wt[,3:(dim(NSH@stock.wt)[2]-0)] +
                                          NSH@stock.wt[,2:(dim(NSH@stock.wt)[2]-1)] +
                                          NSH@stock.wt[,1:(dim(NSH@stock.wt)[2]-2)]) / 3


### ============================================================================
### Prepare Natural Mortality estimates 
### ============================================================================
#Read in estimates from external file
M2            <- read.csv(file.path(".","data","Smoothed_span50_M_NotExtrapolated_NSAS.csv"),
                   header=TRUE)
                   
if(exists("Massump")==T){
  if(Massump == "smooth75"){
    M2            <- read.csv(file.path(".","data","Smoothed_span75_M_NotExtrapolated_NSAS.csv"),
                       header=TRUE)}
  if(Massump == "smooth50"){
    M2            <- read.csv(file.path(".","data","Smoothed_span50_M_NotExtrapolated_NSAS.csv"),
                       header=TRUE)}
  if(Massump == "raw"){
    M2            <- read.csv(file.path(".","data","Raw_NotExtrapolated_NSAS.csv"),
                       header=TRUE)}
  if(Massump == "lorentz"){
    M2            <- read.csv(file.path(".","data","Lorentz_M_NotExtrapolated_NSAS.csv"),
                       header=TRUE)}
}

colnames(M2)  <- sub("X","",colnames(M2))
rownames(M2)  <- M2[,1]
M2            <- M2[,-1]# Trim off first column as it contains 'ages'
M2            <- M2[,apply(M2,2,function(x){all(is.na(x))==F})]


#Extract key data from default assessment
NSHM2       <- NSH
NSHM2@m[]   <- NA
yrs         <- dimnames(NSHM2@m)$year
yrs         <- yrs[which(yrs %in% colnames(M2))]
NSHM2@m[,yrs][] <- as.matrix(M2)

#- Apply 5 year running average
extryrs <- dimnames(NSHM2@m)$year[which(!dimnames(NSHM2@m)$year %in% yrs)]
ages    <- dimnames(NSHM2@m)$age
extrags <- names(which(apply(M2,1,function(x){all(is.na(x))==T})==T))
yrAver  <- 5
for(iYr in as.numeric(rev(extryrs))){
  for(iAge in ages[!ages%in%extrags]){
    NSHM2@m[ac(iAge),ac(iYr)] <- yearMeans(NSHM2@m[ac(iAge),ac((iYr+1):(iYr+yrAver)),],na.rm=T)
  }
}
for(iAge in extrags)
  NSHM2@m[ac(iAge),]          <- NSHM2@m[ac(as.numeric(min(sort(extrags)))-1),]

#Write new M values into the original stock object
NSH@m     <- NSHM2@m


### ============================================================================
### Prepare index object for assessment
### ============================================================================
#Load and modify all numbers at age data
NSH.tun   <- readFLIndices(file.path(data.source,"fleet.txt"))
NSH.tun   <- lapply(NSH.tun,function(x) {x@type <- "number"; return(x)})
NSH.tun[["IBTS0"]]@range["plusgroup"] <- NA

#Load additional indices derived from IHLS (SCAI, MLAI)
#Note that there is a bug in readFLIndex which means that we have to
#take such a round-about approach of loading everything, then dropping
#the first set of indices. Hopefully will be patched in a future release
MLAI.idx <-  readFLIndices(file.path(data.source,"fleet.txt"),
                   file.path(data.source,"mlai.txt"),type="ICA")[-seq(NSH.tun)]
SCAI.idx <-  readFLIndices(file.path(data.source,"fleet.txt"),
                   file.path(data.source,"scai.txt"),type="ICA")[-seq(NSH.tun)]

#Combine all surveys into one object and set names
NSH.tun   <- FLIndices(c(MLAI.idx,SCAI.idx,NSH.tun))
names(NSH.tun) <- sapply(NSH.tun,name)

#Set a-priori weighting factors from Simmonds 2003. These are included
#as variances in the indices object
#Assume that IBTS-Q3 has same variance as IBTS-Q1 and SCAI same as MLAI
NSH.tun[["HERAS"]]@index.var[]    <- 1.0/c(0.63,0.62,0.17,0.10,0.09,0.08,0.07,0.07,0.05)
NSH.tun[["IBTS-Q1"]]@index.var[]  <- 1.0/c(0.47,0.28,0.01,0.01,0.01)
NSH.tun[["IBTS-Q3"]]@index.var[]  <- 1.0/c(0.47,0.28,0.01,0.01,0.01)
NSH.tun[["IBTS0"]]@index.var[]    <- 1.0/0.63
NSH.tun[["MLAI"]]@index.var[]     <- 1.0/0.60
NSH.tun[["SCAI"]]@index.var[]     <- 1.0/0.60

#FLICA requires that biomass indices ie MLAI, is the first index. Check this is ok
if (NSH.tun[[1]]@name != "MLAI") warning("MLAI not as the first index")
