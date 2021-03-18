knitr::opts_chunk$set(echo = TRUE)

rm(list=(ls()))

library(icesDatras)
library(DATRAS)

#setwd("C:/git/wg_HAWG/NSAS/data/IBTS index/")
setwd("J:/git/wg_HAWG/NSAS/data/IBTS index/")
#source("D:/UserData/Work/2017_course/spatial_temporal_model/01_Monday/Data/HighstatLibV10.R")

## function
Mypairs <- function(Z, mytitle, MyVarx) {
  #MyVarx <- colnames(Z)
  pairs(Z, labels = MyVarx,
        cex.labels =  2,
        lower.panel = function(x, y, digits=2, prefix="", cex.cor = 3) {
          panel.cor(x, y, digits, prefix, cex.cor)}, 
        upper.panel =  function(x, y) points(x, y, 
                                             pch = 16, cex = 0.8, 
                                             col = gray(0.1)),
        diag.panel = function(...) {
          rect(par("usr")[1], par("usr")[3],
               par("usr")[2], par("usr")[4], col="grey80")},
        #text.panel = function(x, y, labels) text(x, y, labels, cex=0.5),
        main=mytitle, cex.main=1.5)
  #print(P)
}

# IBTS-Q1: 1984-2017

year_last   <- 2021
yearlist    <- 1984:year_last
quarter     <- 1

cmSize      <- 1
spectrumMax <- 40
agesQ1      <- 1:6
outFolder   <- "."
species     <- "Clupea harengus"

read_raw    <- TRUE

# workspace created using datras package. Needs some work, this has been extracted by Casper
#load("NSIBTSherring_1984_2018.RData") # data set using DATRAS package (Casper)
#IBTSQ1 <- subset(d,Species==species,Quarter == 1,Year %in% yearlist,HaulVal=="V",StdSpecRecCode==1)
#IBTSQ1 <- addSpatialData(IBTSQ1,"./shapefiles/ICES_areas.shp")

if(read_raw){
  # read data from DATRAS
  # !!!! This takes a very long time, try to only run it once !!!!
  ca <- getDATRAS("CA", survey = "NS-IBTS", years = yearlist:year_last, quarters = 1)
  hl <- getDATRAS("HL", survey = "NS-IBTS", years = yearlist:year_last, quarters = 1)
  hh <- getDATRAS("HH", survey = "NS-IBTS", years = yearlist:year_last, quarters = 1)
  
  # form data into a list- must come in specific order
  IBTS <- list(CA = ca, HH = hh, HL = hl)
  cat("Classes of the variables\n")
  print(lapply(IBTS, sapply, class))
  
  ## Inconsistencies with variable names are resolved here
  for(i in 1:3) IBTS[[i]] <- renameDATRAS(IBTS[[i]])
  IBTS <- minus9toNA(IBTS)
  ## =====================================================
  ## Ices-square variable should have the same name ("StatRec") in age and hydro data.
  if (is.null(IBTS$CA$StatRec)) IBTS$CA$StatRec <- IBTS$CA$AreaCode
  #IBTS <- addExtraVariables(IBTS)
  IBTS <- DATRAS:::addExtraVariables(IBTS)
  #IBTS <- fixMissingHaulIds(IBTS, strict = FALSE)
  IBTS <- DATRAS:::fixMissingHaulIds(IBTS, strict = FALSE)
  
  ## convert all strings to factors
  for(i in 1:3){
    for(k in 1:ncol(IBTS[[i]])){
      if(is.character( IBTS[[i]][,k] ) )
        IBTS[[i]][,k] <- factor(IBTS[[i]][,k])
    }
  }
  
  class(IBTS) <- "DATRASraw"
  
  save(IBTS, file = file.path('.',"IBTSQ1",'DATRAS_IBTSQ1.RData'))
}else{
  load(file = file.path('.',"IBTSQ1",'DATRAS_IBTSQ1.RData'))
}