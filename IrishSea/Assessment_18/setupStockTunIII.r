#-------------------------------------------------------------------------------
#- Setup stock & tun objects
#-------------------------------------------------------------------------------

ISH                                   <- readFLStock(file.path(data.source,"index.txt"),no.discards=TRUE)
ISH@catch                             <- ISH@landings
units(ISH)[1:17]                      <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",2),"f",rep("NA",2)))

#Set object details
ISH@name                              <- "Irish Sea herring"
range(ISH)[c("minfbar","maxfbar")]    <- c(4,6)
ISH                                   <- setPlusGroup(ISH,ISH@range["max"])

ISH@stock.wt[8,ac(2009)]              <- (ISH@stock.wt[8,ac(2008)]+ISH@stock.wt[8,ac(2010)])/2
ISH@stock.wt[7,ac(1964)]              <- (ISH@stock.wt[7,ac(1963)]+ISH@stock.wt[7,ac(1965)])/2 #manual replacement avg if 0
ISH@stock.wt[8,ac(1966:1969)]         <- c(0.264,0.275,0.264,0.27) #otherwise replace with values in input files

ISH@catch.wt[7,ac(1964)]              <- (ISH@catch.wt[7,ac(1963)]+ISH@catch.wt[7,ac(1965)])/2
ISH@catch.wt[6,ac(1967)]              <- 0.243
ISH@catch.wt[7,ac(1967:1968)]         <- c(0.227,0.234)
ISH@catch.wt[1:8,ac(2009)]            <- c((ISH@catch.wt[1:8,ac(2008)]+ISH@catch.wt[1:8,ac(2010)])/2)
ISH@catch.wt[8,ac(1966:1969)]         <- c(0.264,0.275,0.264,0.270)

ISH@catch.n[,ac(2009)]                <- NA
ISH@landings.n[,ac(2009)]             <- NA

ISH@landings.wt[8,ac(2009)]           <- (ISH@landings.wt[8,ac(2008)]+ISH@landings.wt[8,ac(2010)])/2
ISH@landings.wt[7,ac(1964)]           <- (ISH@landings.wt[7,ac(1963)]+ISH@landings.wt[7,ac(1965)])/2
ISH@landings.wt[8,ac(1966:1969)]      <- c(0.264,0.275,0.264,0.27)
ISH@landings.wt[,ac(2014)]            <- yearMeans(ISH@landings.wt[,ac(2011:2013)])

if(scenario$trimTS == T)
  ISH                                 <- window(ISH,1980)

### ============================================================================
### Prepare index object for assessment
### ============================================================================
#Load and modify all index data
ISH.tun                               <- readFLIndices(file.path(data.source,"fleet.txt"),
                                                       file.path(data.source,"ssbII.txt"),type="ICA")

ISH.tun[[2]]@index                    <- ISH.tun[[2]]@index * 1000
#Set names
ISH.tun[[1]]@name                     <- "AC(VIIaN)"
ISH.tun[[2]]@name                     <- "VIIaNSpawn"
names(ISH.tun)                        <- lapply(ISH.tun,name)

#Set parameters etc
ISH.tun[["AC(VIIaN)"]]@type           <- "number"
ISH.tun[["VIIaNSpawn"]]@type          <- "biomass"

