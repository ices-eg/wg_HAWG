###~~~~~~~~~~~~~~~~~~~***********~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Code to calculate stats from runs of 'SimpSIM_v3' for blue whiting (whb-comb; IBPBLW assessment, 2016)
###~~~~~~~~~~~~~~~~~~~+++++++++++~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Bastardisation of the existing EQSIM function code
# Changes include:
# 1. New list object to save time series of results (SSB, F etc.)
#
#
#
###~~~~~~~~~~~~~~~~~~~***********~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

###~~~~~~~~~~~~~
## Clean slate
rm(list=ls())

### ------------------------------------------------------------------------------------------------------
###  XX. Stats
### ------------------------------------------------------------------------------------------------------

### TEMP
rm(list=ls())
require (xtable)
require(devtools)
require(msy)
require(FLCore)
#source(paste(SimPath,"SimpSIM_v3.R",sep=""))

## Run Name
# RUNS <- c("BW_SimpSIM3_Allrec_noTACchng",
#           "BW_SimpSIM3_noHLrec_noTACchng",
#           "BW_SimpSIM3_Allrec_TACchngBpa",
#           "BW_SimpSIM3_noHLrec_TACchngBpa",
#           "BW_SimpSIM3_Allrec_TACchngBlim",
#           "BW_SimpSIM3_noHLrec_TACchngBlim",
#           "BW_SimpSIM3_Allrec_TACchng0",
#           "BW_SimpSIM3_noHLrec_TACchng0")
#RUNS <- c("BW_SimpSIM3_Allrec_TACchngBpa")
RUNS <- c("CSrev_BW_SimpSIM3_Allrec_2025TACchngBpa2yr")
# RUNS <- c("CSrev_BW_SimpSIM3_Allrec_2020TACchngBpa",
#           "CSrev_BW_SimpSIM3_Allrec_2025TACchngBpa",
#           "CSrev_BW_SimpSIM3_Allrec_TACchngBpa2yr",
#           "CSrev_BW_SimpSIM3_Allrec_202040TACchngBpa",
#           "CSrev_BW_SimpSIM3_Allrec_2025TACchngBpa2yr",
#           "CSrev_BW_SimpSIM3_Allrec_202540TACchngBpa2yr")

runIden <- "BPs_1500000_2250000_Its_1000_Yrs_102"

for (runName in RUNS) {
  #runName <- "BW_SimpSIM3_Allrec_TACchngBpa"
  
  print(runName)
  
  resPath <- paste("E:\\ICES MSEs\\WKBWMSE_2016\\_SimpSIM\\Results\\",runName,"\\",sep="")
  load(file=paste(resPath,runIden,"_SimpSIM_Workspace.Rdata",sep=""))
  source("E:\\ICES MSEs\\WKBWMSE_2016\\_SimpSIM\\MSE_StatPlot_Funcs_2016.r")
  
### TEMP


  ## Percentiles
  lastYr <- assyearNum + (Nyrs-1)
  years <- 1981:lastYr #Change to assYear+x (Nyrs)
  percentiles = c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975)
  numWorm <- 10
  
  ### ------------------------------------------------------------------------------------------------------
  ### Create stock objects for length of simulation
  # Stored in a list (one stock object per F value)
  Stocks <- list()
  stockTemplate <- window(stock, end=lastYr)
  mat(stockTemplate)[,ac(assyearNum:lastYr)] <- mat(stockTemplate)[,ac(assyearNum-1)]
  m(stockTemplate)[,ac(assyearNum:lastYr)] <- m(stockTemplate)[,ac(assyearNum-1)]
  m.spwn(stockTemplate)[,ac(assyearNum:lastYr)] <- m.spwn(stockTemplate)[,ac(assyearNum-1)]
  harvest.spwn(stockTemplate)[,ac(assyearNum:lastYr)] <- harvest.spwn(stockTemplate)[,ac(assyearNum-1)]
  #stock.n(stockTemplate)[,ac(assyearNum)] <- strtNum
  
  # simStks[[ac(Fbar)]] <- list()
  for (ii in Fvals) {
    Stocks[[ac(ii)]] <- propagate(stockTemplate,numIts)  
    stock.n(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- SimRuns[[ac(ii)]]$N[,,]
    harvest(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- SimRuns[[ac(ii)]]$F[,,]
    catch.n(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- SimRuns[[ac(ii)]]$C[,,]
    catch.wt(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- SimRuns[[ac(ii)]]$catW[,,]
    landings.n(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- SimRuns[[ac(ii)]]$L[,,]
    stock.wt(Stocks[[ac(ii)]]) <- discards.wt(Stocks[[ac(ii)]]) <- landings.wt(Stocks[[ac(ii)]]) <- catch.wt(Stocks[[ac(ii)]])
    discards.n(Stocks[[ac(ii)]])[,ac((assyearNum):lastYr),,,,] <- 0
    #for (y in (assyearNum):lastYr) stock.wt(Stocks[[ac(ii)]])[,ac(y),,,,] <- (SimRuns[[ac(ii)]]$stkW[,SimRuns[[ac(ii)]]$stkWran[y-assyearNum+2,]])[,]
    # add up totals
    catch(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)] <- apply(catch.n(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)]*catch.wt(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)],2:6,sum)
    landings(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)] <- apply(landings.n(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)]*landings.wt(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)],2:6,sum)
    discards(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)] <- apply(discards.n(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)]*discards.wt(Stocks[[ac(ii)]])[,ac((assyearNum-1):lastYr)],2:6,sum)
  } # end of Fvals loop
  
  
### ------------------------------------------------------------------------------------------------------
DATA <- list()
for (jj in Fvals) {
  #jj <- Fvals[1]
  
  percStats <- list()
  STK <- Stocks[[ac(jj)]]
  ## Spawner Stock Biomass (SSB)
  statName <- "SSB"
  stat <- ssb(STK)
  percStats[[statName]][["val"]] <- statPercs(stat, years, percentiles)  #SSB
  percStats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
  percStats[[statName]][["bw"]] <- pointPercsBW(stat, percentiles) #Need to change the function to specify years/ranges in the function call
  percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat, percentiles) 
  
  ## Perceived SSB
  statName <- "SSBdevs"
  stat2 <- SimRuns[[ac(jj)]][["percSSB"]]
  percStats[[statName]][["worm"]] <- percStats[["SSB"]][["worm"]][,ac((assyearNum-2):lastYr)]  #SSB
  dimnames(percStats[[statName]][["worm"]])[[2]] <- c("SSBcv","PhiSSB","STDDevs",ac((assyearNum+1):lastYr)) 
  
  for (iw in 1:numWorm) {
    percStats[[statName]][["worm"]][,ac((assyearNum+1):lastYr),,,,iw] <- log(stat2[-1,iw])-log(stat[,ac((assyearNum+1):lastYr),,,,iw])
    percStats[[statName]][["worm"]][,"STDDevs",,,,iw] <- sd(percStats[[statName]][["worm"]][,ac((assyearNum+1):lastYr),,,,iw]) 
    percStats[[statName]][["worm"]][,"PhiSSB",,,,iw] <- acf(percStats[[statName]][["worm"]][,ac((assyearNum+1):lastYr),,,,iw], plot=F)$acf[2] 
    percStats[[statName]][["worm"]][,"SSBcv",,,,iw] <- as.numeric(percStats[[statName]][["worm"]][,"STDDevs",,,,iw]*((1-percStats[[statName]][["worm"]][,"PhiSSB",,,,iw]^2)^0.5))
  }
  
  ## P(SSB<Blim)
  percStats[["pBlim"]][["val"]] <- stat[,,,,,1]; percStats[["pBlim"]][["val"]][] <- NA; for (yy in years) percStats[["pBlim"]][["val"]][,ac(yy)] <- sum(as.numeric(stat[,ac(yy)])<1500000)/numIts
  xx <- FLQuant(NA, dimnames=list(age=c("pBlim"), year=c("2020","2025", "2016-2020","2021-2025","2066-2115")))
  xx[,"2020"]      <- percStats[["pBlim"]][["val"]][,"2020"]
  xx[,"2025"]      <- percStats[["pBlim"]][["val"]][,"2025"]
  xx[,"2016-2020"] <- max(percStats[["pBlim"]][["val"]][,ac(2016:2020)])
  xx[,"2021-2025"] <- max(percStats[["pBlim"]][["val"]][,ac(2021:2025)])
  xx[,"2066-2115"] <- max(percStats[["pBlim"]][["val"]][,ac(2066:2115)])
  percStats[["pBlim"]][["pt"]] <- xx
  
  ## P(SSB<Bpa)
  percStats[["pBpa"]][["val"]] <- stat[,,,,,1]; percStats[["pBpa"]][["val"]][] <- NA; for (yy in years) percStats[["pBpa"]][["val"]][,ac(yy)] <- sum(as.numeric(stat[,ac(yy)])<2250000)/numIts
  xx <- FLQuant(NA, dimnames=list(age=c("pBpa"), year=c("2020","2025", "2016-2020","2021-2025","2066-2115")))
  xx[,"2020"]      <- percStats[["pBpa"]][["val"]][,"2020"]
  xx[,"2025"]      <- percStats[["pBpa"]][["val"]][,"2025"]
  xx[,"2016-2020"] <- max(percStats[["pBpa"]][["val"]][,ac(2016:2020)])
  xx[,"2021-2025"] <- max(percStats[["pBpa"]][["val"]][,ac(2021:2025)])
  xx[,"2066-2115"] <- max(percStats[["pBpa"]][["val"]][,ac(2066:2115)])
  percStats[["pBpa"]][["pt"]] <- xx

  ## P(SSB<Bpa) PERCEIVED
  stat3 <- stat[,ac((assyearNum+1):lastYr)]; stat3[] <- NA; 
  for (ii in 1:numIts) stat3[,,,,,ii] <- stat2[-1,ii]
  
  percStats[["pBpaPerc"]][["val"]] <- stat3[,,,,,1]; percStats[["pBpaPerc"]][["val"]][] <- NA; for (yy in c(2016:2116)) percStats[["pBpaPerc"]][["val"]][,ac(yy)] <- sum(as.numeric(stat3[,ac(yy)])<2250000)/numIts
  xx <- FLQuant(NA, dimnames=list(age=c("pBpaPerc"), year=c("2020","2025", "2016-2020","2021-2025","2066-2115")))
  xx[,"2020"]      <- percStats[["pBpaPerc"]][["val"]][,"2020"]
  xx[,"2025"]      <- percStats[["pBpaPerc"]][["val"]][,"2025"]
  xx[,"2016-2020"] <- max(percStats[["pBpaPerc"]][["val"]][,ac(2016:2020)])
  xx[,"2021-2025"] <- max(percStats[["pBpaPerc"]][["val"]][,ac(2021:2025)])
  xx[,"2066-2115"] <- max(percStats[["pBpaPerc"]][["val"]][,ac(2066:2115)])
  percStats[["pBpaPerc"]][["pt"]] <- xx

  ## Recruitment (Rec)
  statName <- "Rec"
  stat <- rec(STK)
  percStats[[statName]][["val"]] <- statPercs(stat, years, percentiles)
  percStats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
  percStats[[statName]][["bw"]] <- pointPercsBW(stat, percentiles)
  percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat, percentiles) 
  
  ## Mean F (Ftot)
  statName <- "Fbar"
  stat <- fbar(STK)
  percStats[[statName]][["val"]] <- statPercs(stat, years, percentiles)
  percStats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
  percStats[[statName]][["bw"]] <- pointPercsBW(stat, percentiles)
  percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat, percentiles) 
  
  ## Perceived F
  statName <- "Fdevs"
  stat2 <- SimRuns[[ac(jj)]][["intendF"]]
  percStats[[statName]][["worm"]] <- percStats[["Fbar"]][["worm"]][,ac((assyearNum-2):lastYr)]  #MeanF
  dimnames(percStats[[statName]][["worm"]])[[2]] <- c("Fcv","PhiF","STDDevs",ac((assyearNum+1):lastYr)) 
  
  for (iw in 1:numWorm) {
    percStats[[statName]][["worm"]][,ac((assyearNum+1):(lastYr-1)),,,,iw] <- log(stat2[1:length(ac((assyearNum+1):(lastYr-1))),iw])-log(stat[,ac((assyearNum+1):(lastYr-1)),,,,iw])
    percStats[[statName]][["worm"]][,"STDDevs",,,,iw] <- sd(percStats[[statName]][["worm"]][,ac((assyearNum+1):(lastYr-1)),,,,iw]) 
    percStats[[statName]][["worm"]][,"PhiF",,,,iw] <- acf(percStats[[statName]][["worm"]][,ac((assyearNum+1):(lastYr-1)),,,,iw], plot=F)$acf[2] 
    percStats[[statName]][["worm"]][,"Fcv",,,,iw] <- as.numeric(percStats[[statName]][["worm"]][,"STDDevs",,,,iw]*((1-percStats[[statName]][["worm"]][,"PhiF",,,,iw]^2)^0.5))
  }
  
  ## Catch
  statName <- "Catch"
  stat <- catch(STK)
  percStats[[statName]][["val"]] <- statPercs(stat, years, percentiles)
  percStats[[statName]][["worm"]] <- stat[,,,,,1:numWorm]
  percStats[[statName]][["bw"]] <- pointPercsBW(stat, percentiles)
  percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat, percentiles) 
  
  ## Catch density
  statName <- "CatDens"
  stat <- as.numeric(catch(STK)[,ac(2066:2115)])
  xDens <- hist(stat,breaks=100,plot=F)
  percStats[[statName]][["mids"]] <- xDens$mids
  percStats[[statName]][["density"]] <- xDens$density
   
  ## Catch Change
  statName <- "TACchange"
  stat <- catch(STK)
  stat2    <- catch(STK); stat2[] <- 0
  for (iy in 1982:lastYr) stat2[,ac(iy)] <- (stat[,ac(iy)]-stat[,ac(iy-1)])/stat[,ac(iy-1)]  
  percStats[[statName]][["val"]] <- statPercs(stat2, years, percentiles)
  percStats[[statName]][["worm"]] <- stat2[,,,,,1:numWorm]
  percStats[[statName]][["bw"]] <- pointPercsBW(stat2, percentiles)
  percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat2, percentiles) 
  # DIFFICULT TO INTERPRET BECUASE CAN CHANGE BY MORE THAN 20% IF SSB<Bpa
  
  ## Catch Change ABSOLUTE  
  statName <- "TACchange_abs"
  stat2    <- catch(STK); stat2[] <- 0
  for (iy in 1982:lastYr) stat2[,ac(iy)] <- abs( (stat[,ac(iy)]-stat[,ac(iy-1)])/stat[,ac(iy-1)] )  
  percStats[[statName]][["val"]] <- statPercs(stat2, years, percentiles)
  percStats[[statName]][["worm"]] <- stat2[,,,,,1:numWorm]
  percStats[[statName]][["bw"]] <- pointPercsBW(stat2, percentiles)
  percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat2, percentiles) 
  
  ## HCS Catch Change
  # As done by Dankert
  statName <- "TACchange_HCS"
  stat2    <- catch(STK); stat2[] <- 0
  for (iy in 1982:lastYr) stat2[,ac(iy)] <- (stat[,ac(iy)]-stat[,ac(iy-1)])/((stat[,ac(iy-1)] + stat[,ac(iy)])/2)
  percStats[[statName]][["val"]] <- statPercs(stat2, years, percentiles)
  percStats[[statName]][["worm"]] <- stat2[,,,,,1:numWorm]
  percStats[[statName]][["bw"]] <- pointPercsBW(stat2, percentiles)
  percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat2, percentiles) 
  
  ## HCS Catch Change ABSOLUTE
  # As done by Dankert
  statName <- "TACchange_HCS_abs"
  stat2    <- catch(STK); stat2[] <- 0
  for (iy in 1982:lastYr) stat2[,ac(iy)] <- abs((stat[,ac(iy)]-stat[,ac(iy-1)])/((stat[,ac(iy-1)] + stat[,ac(iy)])/2))
  percStats[[statName]][["val"]] <- statPercs(stat2, years, percentiles)
  percStats[[statName]][["worm"]] <- stat2[,,,,,1:numWorm]
  percStats[[statName]][["bw"]] <- pointPercsBW(stat2, percentiles)
  percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat2, percentiles) 
  
  ## Catch drops
  # number of >50% decreases in catch
  statName <- "TACdrops50"
  stat2    <- catch(STK); stat2[] <- 0 
  for (iy in 1982:lastYr) stat2[,ac(iy)] <- (stat[,ac(iy)]-stat[,ac(iy-1)])/stat[,ac(iy-1)]  
  stat3 <- stat2<(-0.5)
  percStats[[statName]][["val"]] <- statPercs(stat3, years, percentiles)
  percStats[[statName]][["worm"]] <- stat3[,,,,,1:numWorm]
  percStats[[statName]][["bw"]] <- pointPercsBW(stat3, percentiles)
  percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat3, percentiles) 
  # use mean values to compare F levels

  ## Catch drops 30%
  # number of >30% decreases in catch
  statName <- "TACdrops30"
  stat2    <- catch(STK); stat2[] <- 0 
  for (iy in 1982:lastYr) stat2[,ac(iy)] <- (stat[,ac(iy)]-stat[,ac(iy-1)])/stat[,ac(iy-1)]  
  stat3 <- stat2<(-0.3)
  percStats[[statName]][["val"]] <- statPercs(stat3, years, percentiles)
  percStats[[statName]][["worm"]] <- stat3[,,,,,1:numWorm]
  percStats[[statName]][["bw"]] <- pointPercsBW(stat3, percentiles)
  percStats[[statName]][["bw_HCS"]] <- pointPercsBW_HCS(stat3, percentiles) 
  
  DATA[[ac(jj)]] <- percStats
} # end of scens data loop

###~~~~~~~~~~~~~
## Save data
out <- list(stats=DATA,run_name=runName,run_set=runIden, fit=FIT)
save(out,file=paste(resPath,runIden,"_SimpSIM_STATS.Rdata",sep=""))

} #end of Run loop
###~~~~~~~~~~~~~~~~~~~***********~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

