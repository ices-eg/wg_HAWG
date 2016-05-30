#some supporting functions to deal with string manipulation
# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# bubble plot function
bubbles <- function(x,z,cex,...){
  maxz <- max(abs(z),na.rm=T)
  panel.fun <- function(x,z,subscripts,cex,...){
    pt.cex <- sqrt(abs(z)/maxz)*cex
    pt.bg <- ifelse(z<0, '#00000000','#00000050')
    lpoints(x,cex=pt.cex[subscripts],pch=21,fill=pt.bg[subscripts],col=1,...)
  }
  text <- as.character(round(seq(maxz,-maxz,length=6),2))
  
  key = list(space = "right",
             text = list(text),
             points = list(pch = c(21), 
                           cex=sqrt(abs(seq(cex,-cex,length=6)))^2, 
                           fill = rep(c('#00000050','#00000000'),each=3)),
             rep = FALSE)
  
  
  xyplot(x,z=z,cex=cex,panel=panel.fun,key=key,...)
}

#input datafile headers (for current Celtic Sea assessment setup)
ipHeaders <- c("# Number of Years","# First Year","# Number of Ages","# Number of Fleets",
               "# Number of Sensitivity Blocks","# Number of Available Survey Indices",
               "# Natural Mortality","# Fecundity Option",
               "# Fraction of year that elapses prior to SSB calculation (0=Jan-1)",
               "# Maturity","# Number of Weights at Age Matrices",
               "# Weight Matrix - 1","# Weight Matrix - 2","# Weight Matrix - 3",
               "# Selectivity Block Assignment","# Fleet 1 Selectivity Block Assignment",
               "# Selectivity Options for each block 1=by age, 2=logisitic, 3=double logistic",
               "# Selectivity Block #1 Data","# Fleet Start Age","# Fleet End Age",
               "# Age Range for Average F",
               "# Average F report option (1=unweighted, 2=Nweighted, 3=Bweighted)",
               "# Use Likelihood constants? (1=yes)","# Release Mortality by Fleet",
               "# Catch Data","# Fleet-1 Catch Data","# Discards","# Fleet-1 Discards Data",
               "# Release Proportion","# Fleet-1 Release Data","# Survey Index Data",
               "# Aggregate Index Units","# Age Proportion Index Units","# Weight at Age Matrix",
               "# Index Month","# Index Selectivity Link to Fleet","# Index Selectivity Options 1=by age, 2=logisitic, 3=double logistic",
               "# Index Start Age","# Index End Age","# Estimate Proportion (Yes=1)",
               "# Use Index (Yes=1)","# Index-1 Selectivity Data","# Index-1 Data",
               "# Phase Control","# Phase for F mult in 1st Year","# Phase for F mult Deviations",
               "# Phase for Recruitment Deviations","# Phase for N in 1st Year",
               "# Phase for Catchability in 1st Year","# Phase for Catchability Deviations",
               "# Phase for Stock Recruitment Relationship","# Phase for Steepness",
               "# Recruitment CV by Year","# Lambdas by Index","# Lambda for Total Catch in Weight by Fleet",
               "# Lambda for Total Discards at Age by Fleet","# Catch Total CV by Year and Fleet",
               "# Discard Total CV by Year and Fleet","# Catch Effective Sample Size by Year and Fleet",
               "# Discard Effective Sample Size by Year and Fleet","# Lambda for F Mult in First year by Fleet",
               "# CV for F Mult in First year by Fleet","# Lambda for F Mult Deviations by Fleet",
               "# CV for F Mult Deviations by Fleet","# Lambda for N in 1st Year Deviations",
               "# CV for N in 1st Year Deviations","# Lambda for Recruitment Deviations",
               "# Lambda for Catchability in First year by Index","# CV for Catchability in First year by Index",
               "# Lambda for Catchability Deviations by Index","# CV for Catchability Deviations by Index",
               "# Lambda for Deviation from Initial Steepness","# CV for Deviation from Initial Steepness",
               "# Lambda for Deviation from Unexploited Stock Size","# CV for Deviation from Unexploited Stock Size",
               "# NAA Deviations Flag","# Initial Numbers at Age in 1st Year","# Initial F Mult in 1st Year by Fleet",
               "# Initial Catchabilty by Index","# Stock Recruitment Flag","# Initial Unexploited Stock",
               "# Initial Steepness","# Maximum F","# Ignore Guesses (Yes=1)","# Projection Control",
               "# Do Projections (Yes=1)","# Fleet Directed Flag","# Final Year in Projection",
               "# Projection Data by Year","# Do MCMC (Yes=1)","# MCMC Year Option","# MCMC Iterations",
               "# MCMC Thinning Factor","# MCMC Random Seed","# Agepro R Option","# Agepro R Option Start Year",
               "# Agepro R Option End Year","# Export R Flag","# Test Value","# Fleet Names",
               "# Survey Names")

#read report file
fReadReport <- function(reportFile){
  
  #read the contents of the report file
  report <- trim(readLines(reportFile))

  #report file headers
  repHeaders <- c("Input and Estimated effective sample sizes for fleet 1",
                  "Input and Estimated effective Discard sample sizes for fleet 1",
                  "Observed and predicted total fleet catch by year and standardized residual",
                  "Observed and predicted total fleet Discards by year and standardized residual",
                  "Index proportions at age by index",
                  "Index Selectivity at Age",
                  "Selectivity by age and year for each fleet",
                  "Fmult by year for each fleet",
                  "Directed F by age and year for each fleet",
                  "Discard F by age and year for each fleet",
                  "Total F",
                  "Average F for ages 2 to 5",
                  "Population Numbers at the Start of the Year",
                  "Biomass Time Series",
                  "q by index",
                  "Proportions of catch at age by fleet",
                  "Proportions of Discards at age by fleet",
                  "Spawning Stock, Obs Recruits(year+1), Pred Recruits(year+1), standardized residual",
                  "Annual stock recruitment parameters")
  
  #report header positions
  posRepHead <- match(repHeaders, report)
  
  #report any headers not found
  if (length(repHeaders[is.na(posRepHead)])>0) {
    stop("Header \"",repHeaders[is.na(posRepHead)],"\" not found in file ",reportFile,"\n")
  }
  
  #report any negative differences (implying a different order for the data)
  if (length(posRepHead[diff(posRepHead)<0])>0){
    stop("Unexpected order of headers, check report file\n")
  }
  
  names(posRepHead) <- repHeaders
  
  #find the location of blank lines
  posBlankLines <- which(nchar(report)==0)
  
  #merge the position vectors
  posRepHead <- sort(c(posRepHead,posBlankLines))
  
  #SSB,Recruitment outputs
  
  idxStart <- match(posRepHead["Spawning Stock, Obs Recruits(year+1), Pred Recruits(year+1), standardized residual"],posRepHead)
  outDat <- report[(posRepHead[idxStart]+3):(posRepHead[idxStart+1])-1]
  #no residual report for the last year so insert a placeholder here so that following code doesn;t crash
  outDat[length(outDat)] <- paste0(outDat[length(outDat)]," ","xxxx")
  outDat <- strsplit(outDat,"\\s+")
  
  #NAs by coercion will be reported here
  dfOut <- data.frame("Year" = as.integer(lapply(outDat,"[[",1)),
                      "SSB" = as.numeric(lapply(outDat,"[[",2)),
                      "ObsRec" = as.numeric(lapply(outDat,"[[",3)),
                      "PredRec" = as.numeric(lapply(outDat,"[[",4)),
                      "ResRec" = as.numeric(lapply(outDat,"[[",5)))
  
  
  #FBar
  idxStart <- match(posRepHead["Average F for ages 2 to 5"],posRepHead)
  outDat <- report[(posRepHead[idxStart]+4):(posRepHead[idxStart+1])-1]
  outDat <- strsplit(outDat,"\\s+")
  
  dfOut$FBarUnweighted <- as.numeric(lapply(outDat,"[[",2))
  dfOut$FBarNWeighted <- as.numeric(lapply(outDat,"[[",3))
  dfOut$FBarBWeighted <- as.numeric(lapply(outDat,"[[",4))

  #Landings
  idxStart <- match(posRepHead["Observed and predicted total fleet catch by year and standardized residual"],posRepHead)
  outDat <- report[(posRepHead[idxStart]+3):(posRepHead[idxStart+1])-1]
  outDat <- strsplit(outDat,"\\s+")

  dfOut$ObsCatch <- as.numeric(lapply(outDat,"[[",2))
  dfOut$PredCatch <- as.numeric(lapply(outDat,"[[",3))
  dfOut$ResCatch <- as.numeric(lapply(outDat,"[[",4))

  #index predictions
  idxStart <- match(posRepHead["Index proportions at age by index"],posRepHead)
  outDat <- report[(posRepHead[idxStart]+3):(posRepHead[idxStart+1])-1]
  outDat <- strsplit(outDat,"\\s+")

  dfOut$ObsIdxAge1 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",5))
  dfOut$ObsIdxAge2 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",6))
  dfOut$ObsIdxAge3 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",7))
  dfOut$ObsIdxAge4 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",8))
  dfOut$ObsIdxAge5 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",9))
  dfOut$ObsIdxAge6 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",10))
  dfOut$ObsIdxAge7 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",11))
  dfOut$ObsIdxAge8 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",12))
  dfOut$ObsIdxAge9 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",13))
  dfOut$PredIdxAge1 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",5))
  dfOut$PredIdxAge2 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",6))
  dfOut$PredIdxAge3 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",7))
  dfOut$PredIdxAge4 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",8))
  dfOut$PredIdxAge5 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",9))
  dfOut$PredIdxAge6 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",10))
  dfOut$PredIdxAge7 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",11))
  dfOut$PredIdxAge8 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",12))
  dfOut$PredIdxAge9 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",13))
  
  #residuals
  dfOut$ResIdxAge1 <- dfOut$ObsIdxAge1 - dfOut$PredIdxAge1
  dfOut$ResIdxAge2 <- dfOut$ObsIdxAge2 - dfOut$PredIdxAge2
  dfOut$ResIdxAge3 <- dfOut$ObsIdxAge3 - dfOut$PredIdxAge3
  dfOut$ResIdxAge4 <- dfOut$ObsIdxAge4 - dfOut$PredIdxAge4
  dfOut$ResIdxAge5 <- dfOut$ObsIdxAge5 - dfOut$PredIdxAge5
  dfOut$ResIdxAge6 <- dfOut$ObsIdxAge6 - dfOut$PredIdxAge6
  dfOut$ResIdxAge7 <- dfOut$ObsIdxAge7 - dfOut$PredIdxAge7
  dfOut$ResIdxAge8 <- dfOut$ObsIdxAge8 - dfOut$PredIdxAge8
  dfOut$ResIdxAge9 <- dfOut$ObsIdxAge9 - dfOut$PredIdxAge9
  
  #catch proportions at age
  idxStart <- match(posRepHead["Proportions of catch at age by fleet"],posRepHead)
  outDat <- report[(posRepHead[idxStart]+3):(posRepHead[idxStart+1])-1]
  outDat <- strsplit(outDat,"\\s+")
  
  #observed catch proportions at age
  dfOut$ObsCatchPropAge1 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",5))
  dfOut$ObsCatchPropAge2 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",6))
  dfOut$ObsCatchPropAge3 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",7))
  dfOut$ObsCatchPropAge4 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",8))
  dfOut$ObsCatchPropAge5 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",9))
  dfOut$ObsCatchPropAge6 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",10))
  dfOut$ObsCatchPropAge7 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",11))
  dfOut$ObsCatchPropAge8 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",12))
  dfOut$ObsCatchPropAge9 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Obs"],"[[",13))
  
  #predicted catch proportions at age
  dfOut$PredCatchPropAge1 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",5))
  dfOut$PredCatchPropAge2 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",6))
  dfOut$PredCatchPropAge3 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",7))
  dfOut$PredCatchPropAge4 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",8))
  dfOut$PredCatchPropAge5 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",9))
  dfOut$PredCatchPropAge6 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",10))
  dfOut$PredCatchPropAge7 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",11))
  dfOut$PredCatchPropAge8 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",12))
  dfOut$PredCatchPropAge9 <- as.numeric(lapply(outDat[lapply(outDat,"[[",3)=="Pred"],"[[",13))
  
  #residual catch proportion at age
  dfOut$ResCatchPropAge1 <- dfOut$ObsCatchPropAge1 - dfOut$PredCatchPropAge1
  dfOut$ResCatchPropAge2 <- dfOut$ObsCatchPropAge2 - dfOut$PredCatchPropAge2
  dfOut$ResCatchPropAge3 <- dfOut$ObsCatchPropAge3 - dfOut$PredCatchPropAge3
  dfOut$ResCatchPropAge4 <- dfOut$ObsCatchPropAge4 - dfOut$PredCatchPropAge4
  dfOut$ResCatchPropAge5 <- dfOut$ObsCatchPropAge5 - dfOut$PredCatchPropAge5
  dfOut$ResCatchPropAge6 <- dfOut$ObsCatchPropAge6 - dfOut$PredCatchPropAge6
  dfOut$ResCatchPropAge7 <- dfOut$ObsCatchPropAge7 - dfOut$PredCatchPropAge7
  dfOut$ResCatchPropAge8 <- dfOut$ObsCatchPropAge8 - dfOut$PredCatchPropAge8
  dfOut$ResCatchPropAge9 <- dfOut$ObsCatchPropAge9 - dfOut$PredCatchPropAge9
  
  #population numbers at start of the year
  idxStart <- match(posRepHead["Population Numbers at the Start of the Year"],posRepHead)
  outDat <- report[(posRepHead[idxStart]+2):(posRepHead[idxStart+1])-1]
  outDat <- strsplit(outDat,"\\s+")
  dfOut$Age1 <- as.numeric(lapply(outDat,"[[",1))
  dfOut$Age2 <- as.numeric(lapply(outDat,"[[",2))
  dfOut$Age3 <- as.numeric(lapply(outDat,"[[",3))
  dfOut$Age4 <- as.numeric(lapply(outDat,"[[",4))
  dfOut$Age5 <- as.numeric(lapply(outDat,"[[",5))
  dfOut$Age6 <- as.numeric(lapply(outDat,"[[",6))
  dfOut$Age7 <- as.numeric(lapply(outDat,"[[",7))
  dfOut$Age8 <- as.numeric(lapply(outDat,"[[",8))
  dfOut$Age9 <- as.numeric(lapply(outDat,"[[",9))
  
  #total F at age
  idxStart <- match(posRepHead["Total F"],posRepHead)
  outDat <- report[(posRepHead[idxStart]+2):(posRepHead[idxStart+1])-1]
  outDat <- strsplit(outDat,"\\s+")
  dfOut$F1 <- as.numeric(lapply(outDat,"[[",1))
  dfOut$F2 <- as.numeric(lapply(outDat,"[[",2))
  dfOut$F3 <- as.numeric(lapply(outDat,"[[",3))
  dfOut$F4 <- as.numeric(lapply(outDat,"[[",4))
  dfOut$F5 <- as.numeric(lapply(outDat,"[[",5))
  dfOut$F6 <- as.numeric(lapply(outDat,"[[",6))
  dfOut$F7 <- as.numeric(lapply(outDat,"[[",7))
  dfOut$F8 <- as.numeric(lapply(outDat,"[[",8))
  dfOut$F9 <- as.numeric(lapply(outDat,"[[",9))
  
  dfOut

}