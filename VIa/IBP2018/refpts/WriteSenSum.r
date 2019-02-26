# ------------------------------------------------------------------------
# WriteSumSen.r
#
# convert stock object to sum and sen files
# Martin Pastoors, 8/2/2015
# 22/05/2015 updated after Webex meeting and new final assessment
# 25/02/2019 update after IBP 2019 to compare with EQSIM 
# ------------------------------------------------------------------------

library(msy)
source("plotmsy.r")

writeSumSen <-
  function(stock.list,filename,stockname=filename)
  {
    senfilename <- paste0(filename,".sen")
    sumfilename <- paste0(filename,".sum")
    cat(stockname, '\n', file=senfilename)
    cat(range(an(stock.list$dim$age)),max(an(stock.list$dim$year)),0,'\n', append=TRUE,file=senfilename)
    cat('1 1 0\n', append=TRUE,file=senfilename)
    cat(paste(paste0("N",stock.list$dims$age),0,0,collapse='\n'),'\n', append=TRUE,file=senfilename)
    for (x in names(stock.list)[2:8]) cat(paste(paste0(x,stock.list$dims$age),stock.list[[x]][,"mean"],stock.list[[x]][,"CV"],collapse='\n'),'\n', append=TRUE,file=senfilename)
    
    cat('Slightly stunted sum file\n12\n0\n0\n', file=sumfilename)
    cat(range(stock.list[[1]][,"year"]),'\n', file=sumfilename, append=TRUE)
    cat("Recruitment\n", file=sumfilename, append=TRUE)
    cat("Dummy line 7\n", file=sumfilename, append=TRUE)
    cat("SSB\n", file=sumfilename, append=TRUE)
    for (i in 9:13) cat("dummy line", i,'\n', file=sumfilename, append=TRUE)
    cat("Yield\n", file=sumfilename, append=TRUE)
    for (i in 15:20) cat("dummy line", i,'\n', file=sumfilename, append=TRUE)
    cat(range(an(stock.list[["dims"]]$fbarage)),'\n', file=sumfilename, append=TRUE)
    for (i in 22:27) cat("dummy line", i,'\n', file=sumfilename, append=TRUE)
    cat(paste(stock.list$rby[,"year"], stock.list$rby[,"rec"], stock.list$rby[,"ssb"],0,0,stock.list$rby[,"yield"],0,0,0,stock.list$rby[,"fbar"],0,0,collapse='\n'), file=sumfilename, append=TRUE)    
  } # End of WriteSumSen

#Use this as working directory (change as appropriate)
# setwd("c://data/wkwest/data/malin")
# load(file.path(".//FINAL SAM 20150421//", "MSH.RData"))
load("D:/TEMP/IBP_VIaHerring_finalRun_MF.Rdata")
x <- MSHm

#subsetting the stock.n one more year in order to correct for winterrings
#x@stock.n[,ac((range(x)["minyear"]): (range(x)["maxyear"]-1))][] <- 
#     x@stock.n[,ac((range(x)["minyear"]+1): (range(x)["maxyear"]))]
#x <- trim(x, year=range(x)["minyear"]:(range(x)["maxyear"]-2) )
#x <- trim(x, year=1989:(range(x)["maxyear"]-2) )
#x <- trim(x, year=1989:2013)

# Create input file
msy.inputs <- fls2list(x, y=3)

# Write Sen and Sum files
writeSumSen(msy.inputs,"MSH")