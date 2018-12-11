# -------------------------------------------------------------------------------------------
# mse.r
# 
# Modified EQSIM3 code applied to WBSS herring. 
# Evaluates the performance of a modified ICES MSY rule that allows for (reduced) fishing 
# below Blim. 
#
# 09/07/2018 adapted version eqsim3 with HCR that has steeper slope below Blim. 
# 15/07/2018 Code is adapted to run for WBSS herring
#              - changed years for which average recruitment is calculated (only for years used in SR estimation)
#              - changed calculation of starting population (old code had an error in the replacement of plus group)
#              - added year to the arrays (instead of numbering from 1 onwards)
# 13/08/2018 Code checking
# -------------------------------------------------------------------------------------------

# packages 
rm(list=ls())

library(ggplot2)
library(FLCore)
library(msy)
library(tidyverse)
library(minpack.lm)  # install.packages("minpack.lm")

# Function for segmented regression through Blim
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= blim, 
                                            ab$a * blim, 
                                            ab$a * ssb))

# Function to calculate the harvest by fleet given a TAC for a stock ; NOT WORKING YET
fleet.harvest <- function(stk,iYr,TACS){
  
  nUnits  <- dims(stk)$unit
  
  #- prepare for iterations
  nIter   <- dims(TACS)$iter
  res     <- matrix(NA,ncol=nIter,nrow=nUnits,
                    dimnames=list(units=dimnames(stk@stock.n)$unit,iter=1:nIter))
  for(iTer in 1:nIter) 
    res[,iTer] <- nls.lm(par=runif(nUnits),
                         lower=rep(1e-8,nUnits), 
                         upper=NULL,
                         rescaleF,
                         stk=iter(stk,iTer),
                         iYr=iYr,
                         TACS=c(iter(TACS,iTer)),
                         nls.lm.control(ftol = (.Machine$double.eps),maxiter = 1000),jac=NULL)$par
  stk@harvest[,iYr] <- sweep(stk@harvest[,iYr],c(3,6),res,"*")
  
  return(stk@harvest[,iYr])
}

# Function to scale the F pattern for stock NOT USED BECAUSE NOT WORKING

rescaleF      <- function(mult,stk.=stk,iYr.=iYr,TACS.=TACS){
  stk.@harvest[,iYr.] <- sweep(stk.@harvest[,iYr.],3,mult,"*")
  stkZ                <- unitSums(stk.@harvest[,iYr.]) + stk.@m[,iYr.,1]
  ctch                <- c(apply(sweep(stk.@stock.n[,iYr.] * stk.@catch.wt[,iYr.] * sweep(stk.@harvest[,iYr.],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T))
  
  #res                 <- -1*c(dnorm(log(c(TACS.)[1]),log(ctch[1]),sd=0.15,log=T),
  #                                                dnorm(log(c(TACS.)[2]),log(ctch[2]),sd=0.3,log=T),
  #                                                dnorm(log(c(TACS.)[3]),log(ctch[3]),sd=0.15,log=T),
  #                                                dnorm(log(c(TACS.)[4]),log(ctch[4]),sd=0.3,log=T))
  #                    print(res)
  res                 <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n[,iYr.] * stk.@catch.wt[,iYr.] * sweep(stk.@harvest[,iYr.],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))
  return(res)}



# -------------------------------------------------------------------------------------------
# load the WBSS assessment object(s)
# -------------------------------------------------------------------------------------------

load("D:/HAWG/2018/05. Data/WBSS/WBSS_mf_004/Herring WBSS_2016_EqSim_Workspace.Rdata")
# plot(stk)

#### recruitment models estimation
noSims    <- 999
appModels <- c("Segreg","Ricker","Bevholt") 
rmSRRYrs  <- c()  
FIT_All   <- eqsr_fit(stk,nsamp=noSims, models = appModels, remove.years=rmSRRYrs)
eqsr_plot(FIT_All,n=2e4)

blim <- 120000
rmSRRYrs  <- c(1991:1999)  
FIT_segregBlim   <- eqsr_fit(stk,nsamp=noSims, models = "SegregBlim", remove.years=rmSRRYrs)
eqsr_plot(FIT_segregBlim,n=2e4)


# -------------------------------------------------------------------------------------------
# simulation settings
# -------------------------------------------------------------------------------------------

fit       = FIT_segregBlim
bio.years = c(2012, 2016)
bio.const = FALSE
sel.years = c(2012, 2016)
sel.const = FALSE
Fmsy      = 0.31
Fcv       = 0.212
Fphi      = 0.423
SSBcv     = 0.0
SSBphi    = 0.0
# Fcv       = 0.23
# Fphi      = 0.24
# SSBcv     = 0.31
rhologRec = FALSE
Blim      = 120000
Bpa       = 150000
Btrigger  = 150000
Blim2     = 60000
Blim2.scan= seq(0, Blim, by = 20000)
Nyear     = 15
Nits      = 500
process.error = FALSE
verbose = TRUE
recruitment.trim = c(3, -3)
CatchInFirstYear = 50740

# initial checks
if (abs(Fphi) >= 1)
  stop("Fphi, the autocorelation parameter for log F should be between (-1, 1)")
if ((recruitment.trim[1] + recruitment.trim[2]) > 0)
  stop("recruitment truncation must be between a high - low range")

# set year ranges
btyr1 <- bio.years[1]
btyr2 <- bio.years[2]
slyr1 <- sel.years[1]
slyr2 <- sel.years[2]

# Number of years to keep (= all)
keep <- Nyear

# Define simulation start and end year
simstartyear <- max(sel.years)+1
simendyear   <- simstartyear + Nyear - 1


# Resample SR parameters for each of the iterations
SR         <- fit$sr.sto[sample(1:dim(fit$sr.sto)[1] , Nits , replace = F),]

# historical data on stock and recruitment (only for the years used to estimate the SR curve)
data       <- 
  fit$rby[, c("rec", "ssb", "year")] %>% 
  filter (year > max(rmSRRYrs))  # make sure to only use the years that were used to estimate SR

# stock object and subsets for bioyears and selectionyears
stk        <- fit$stk
stk.win    <- FLCore::window(stk, start = btyr1, end = btyr2)
stk.winsel <- FLCore::window(stk, start = slyr1, end = slyr2)

# Function to replace zero by NA
ReplaceZeroByNA <- function(x, i) {
  x2 <- x
  x2[i] <- NA
  x2[] <- apply(x2, 1, mean, na.rm = TRUE)
  x[i] <- x2[i]
  return(x)
}

west     <- matrix(FLCore::stock.wt(stk.win), ncol = btyr2 - btyr1 + 1)
i <- west == 0
if (any(i)) west <- ReplaceZeroByNA(west, i)

weca     <- matrix(FLCore::catch.wt(stk.win), ncol = btyr2 - btyr1 + 1)
i <- weca == 0
if (any(i)) weca <- ReplaceZeroByNA(weca, i)

wela     <- matrix(FLCore::landings.wt(stk.win), ncol = btyr2 - btyr1 + 1)
i <- wela == 0
if (any(i)) wela <- ReplaceZeroByNA(wela, i)

Mat      <- matrix(FLCore::mat(stk.win)          , ncol = btyr2 - btyr1 + 1)
M        <- matrix(FLCore::m(stk.win)            , ncol = btyr2 - btyr1 + 1)
landings <- matrix(FLCore::landings.n(stk.winsel), ncol = slyr2 - slyr1 + 1)
catch    <- matrix(FLCore::catch.n(stk.winsel)   , ncol = slyr2 - slyr1 + 1)
sel      <- matrix(FLCore::harvest(stk.winsel)   , ncol = slyr2 - slyr1 + 1)
Fbar     <- matrix(FLCore::fbar(stk.winsel)      , ncol = slyr2 - slyr1 + 1)

# Calculate selection by dividing F at age by Fbar
sel      <- sweep(sel, 2, Fbar, "/")

if (sel.const == TRUE) {
  sel[]      <- apply(sel, 1, mean)
  landings[] <- apply(landings, 1, mean)
  catch[]    <- apply(catch, 1, mean)
}

if (bio.const == TRUE) {
  west[] <- apply(west, 1, mean)
  weca[] <- apply(weca, 1, mean)
  wela[] <- apply(wela, 1, mean)
  Mat[]  <- apply(Mat, 1, mean)
  M[]    <- apply(M, 1, mean)
}

# ratio of number of landings to catch
land.cat <- landings/catch
i        <- is.na(land.cat)
if (any(i)) land.cat[i] <- 1

# Set Fprop and Mprop
Fprop   <- apply(FLCore::harvest.spwn(stk.winsel), 1, mean)[drop = TRUE]
Mprop   <- apply(FLCore::m.spwn(stk.win), 1, mean)[drop = TRUE]

# Create empty objects for storing Operating model ('real') simulation results
ages    <- FLCore::dims(stk)$age

ssbreal <- Fbarreal <- Ferr <- catchreal <- landingsreal <-
  array(0, c(Nyear, Nits), dimnames = list(year = simstartyear:simendyear, iter = 1:Nits))

Nreal <- Freal <- CNreal <- WSreal <- WCreal <-  WLreal <- Rreal <- 
  array(0, c(ages, Nyear, Nits), dimnames = list(age = (range(stk)[1]:range(stk)[2]),year = simstartyear:simendyear, iter = 1:Nits))

ssbperc <- Fbarperc <- catchperc <- landingsperc <-
  array(0, c(Nyear, Nits), dimnames = list(year = simstartyear:simendyear, iter = 1:Nits))

# F errors
Ferr[1, ] <- stats::rnorm(n = Nits, mean = 0, sd = 1) * Fcv/sqrt(1 - Fphi^2)
for (j in 2:Nyear) {
  Ferr[j, ] <- Fphi * Ferr[j - 1, ] + Fcv * stats::rnorm(n = Nits, mean = 0, sd = 1)
}

# SSB errors
SSBerr <- matrix(stats::rnorm(n = Nyear * Nits, mean = 0, sd = 1), 
                 ncol = Nits,
                 dimnames = list(year = simstartyear:simendyear, iter = 1:Nits)) * SSBcv

# random vectors for weight
rsam <- array(sample(1:ncol(weca), Nyear * Nits, TRUE), 
              c(Nyear, Nits),
              dimnames = list(year = simstartyear:simendyear, iter = 1:Nits))

# random vectors for selectivity
rsamsel <- array(sample(1:ncol(sel), Nyear * Nits, TRUE), 
                 c(Nyear, Nits),
                 dimnames = list(year = simstartyear:simendyear, iter = 1:Nits))

# Generate random weights
WCreal[]      <- c(weca[, c(rsam)])
WLreal[]      <- c(wela[, c(rsam)])

# Generate random ratio of landings to catch (Why ??)
Rreal[]      <- c(land.cat[, c(rsamsel)])

# Initial recruitment
R         <- mean(data$rec)

# Number of scans over Blim2
NBlim2   <- length(Blim2.scan)

# Set up objects for storing summary of simulated results (why 7?)
ssbs <- cats <- lans <- recs <- 
  array(0, c(7, NBlim2))

# Set up objects for storing a data
# ferr <- ssbsa <- catsa <- lansa <- recsa <- 
#   array(0, c(NBlim2, keep, Nits))

# first year to keep results
begin <- Nyear - keep + 1

# Generate matrix of residuals for SR
resids = array(stats::rnorm(Nits * (Nyear + 1), 0, SR$cv), c(Nits, Nyear + 1))

#  Autocorrelation in Recruitment Residuals:
if (rhologRec) {
  fittedlogRec <- do.call(cbind, lapply(c(1:nrow(fit$sr.sto)),
                                        function(i) {
                                          FUN <- match.fun(fit$sr.sto$model[i])
                                          FUN(fit$sr.sto[i, ], fit$rby$ssb)
                                        }))
  # Calculate lag 1 autocorrelation of residuals:
  rhologRec <- apply(log(fit$rby$rec) - fittedlogRec, 2,
                     function(x) {
                       stats::cor(x[-length(x)], x[-1])
                     })
  
  # Draw residuals according to AR(1) process:
  for (j in 2:(Nyear + 1)) {
    resids[, j] <- rhologRec * resids[, j - 1] + resids[,
                                                        j] * sqrt(1 - rhologRec^2)
  }
}

# Limit how extreme the Rec residuals can get:
lims = t(array(SR$cv, c(Nits, 2))) * recruitment.trim
for (k in 1:Nits) {
  resids[k, resids[k, ] > lims[1, k]] = lims[1, k]
}
for (k in 1:Nits) {
  resids[k, resids[k, ] < lims[2, k]] = lims[2, k]
}

# calculate F at Blim according to ICES MSY rule
FatBlim <- Fmsy * (Blim/Btrigger) 

# set up an FLStock object with the appropriate dimensions (alternative to all the matrices)
stk.all    <- FLCore::window(stk, start = 1990, end = simendyear)
stk.all    <- propagate(stk.all, Nits)
plot(stk.all)

# ----- SCANS ------------------------------------------------------

# Create dataframe for storing loop information 
loop.df <- data.frame()

# initialize empty data.frames for individual scans over Blim2
Cwy.df <- Fbarreal.df <- Freal.df <- Nreal.df <- WCreal.df <- ssbreal.df <- data.frame()

# Loop over NBlim2 (number of scans for Blim2)
# i <- 1
for (i in 1:NBlim2) {
  
  # Set Blim2 value
  Blim2 <- Blim2.scan[i]
  
  # --------------------------------------
  # Population in simulation year 1 (the intermediate or current year)
  # --------------------------------------
  
  # Zpre: Z that occurs before spawning
  Zpre <- (sel[, rsamsel[1, ]] * Fmsy * Fprop + M[, rsam[1,]] * Mprop)
  
  # compute survivors after the last assessment year
  N    <- stock.n(stk)[,ac(max(sel.years))]  * exp (-(harvest(stk)[,ac(max(sel.years))] + 
                                                            m(stk)[,ac(max(sel.years))]))
  
  # add year to the dimnames (last year + 1)
  dimnames(N)$year <- max(sel.years) + 1
  
  # plus group
  N[dim(N)[1], ]     <- N[dim(N)[1]-1 , ] + N[dim(N)[1] , ]  
  # N[dim(N)[1]-1 , ]  <- N[dim(N)[1]-1 , ] + N[dim(N)[1] , ]  # Note: this was wrong; changed max age - 1
  
  # shift all one age down (except plus group)
  N[c(-1, -dim(N)[1])]              <- N[1:(dim(N)[1]-2)] # N[-dim(N)[1] , ]

  # add recruitment
  N[1]               <- R
  
  # set Nreal for the first simulation year
  Nreal[, 1, ] <- c(N@.Data)
  
  # set real SSB for the first simulation year
  ssbreal[1, ] <- colSums(Mat[, rsam[1, ]] * Nreal[, 1, ] * west[, rsam[1, ]]/exp(Zpre)[])
  
  # set perceived SSB, catch and F for the first simulation year
  ssbperc[j,]    <- ssbreal[j, ] * exp(SSBerr[j, ]) 
  
  # calculate catchnumber in first year
  # Use CatchInFirstYear and selection pattern to calculate F at age and catch at age
  # stk.all@stock.n[,ac(simstartyear),,,] <- N
  # TACS        <- FLQuant(CatchInFirstYear,
  #                        dimnames=list(age="all",year=ac(simstartyear),unit=NA,
  #                                      season="all",area=1,iter=1))
  # fleet.harvest(stk,iYr=2017,TACS)
  
  # Temporary fix: set F in the intermediate year equal to the last year of assessment, scaled by the catch
  
  
  # update the loop data.frame
  loop.df <-
    bind_rows(loop.df, 
              data.frame(year = simstartyear,
                         iter = an(unlist(dimnames(ssbreal)["iter"])), 
                         Fnext= an(NA), Ferr = an(NA),
                         ssbs = ssbreal[1,] * exp(SSBerr[1, ]), SSBerr=SSBerr[1,], 
                         Blim2, Blim, Btrigger, Fmsy, FatBlim) )
  
  
  
  # --------------------------------------
  # simulation of year 2 until the end
  # --------------------------------------
  
  for (j in 2:Nyear) {
  # j <- 2
  
    y   <- simstartyear + j -1
    # SSB <- ssbreal[j - 1, ]
    
    if (process.error) {
      allrecs <- sapply(unique(SR$mod), function(mod) exp(match.fun(mod)(SR, ssbreal[j-1, ]) + resids[, j]))
    } else {
      allrecs <- sapply(unique(SR$mod), function(mod) exp(match.fun(mod)(SR, ssbreal[j - 1, ])))
    }
    
    select <- cbind(seq(Nits), as.numeric(factor(SR$mod,
                                                 levels = unique(SR$mod))))
    Nreal[1, j, ] <- allrecs[select]
    
    # ======================================================================
    # This is where the F for the current year is determined; i.e. the HCR
    
    # Tester
    # SSBs    <- seq(from=0, to=50000, length.out = 500)
    
    # get perceived SSBs from year before
    SSBs    <- ssbreal[j - 1, ] * exp(SSBerr[j-1, ]) 
    
    #initiate Fnext object
    Fnext   <- Fmsy * pmin(1, SSBs * exp(SSBerr[j-1,])) 
    
    # Temporary vectors above and below Blim2, Blim and Bpa
    tmpL_Blim2    <- c(SSBs <  Blim2)
    tmpG_Blim2    <- c(SSBs >= Blim2)
    tmpL_Blim     <- c(SSBs <  Blim); 
    tmpG_Blim     <- c(SSBs >= Blim); 
    tmpL_Btrigger <- c(SSBs <  Btrigger)
    tmpG_Btrigger <- c(SSBs >= Btrigger)
    
    # above Btrigger
    Fnext[tmpG_Btrigger] <- Fmsy
    
    # between Blim and Btrigger
    Fnext[tmpG_Blim & tmpL_Btrigger] <- 
      Fmsy * (SSBs[tmpG_Blim & tmpL_Btrigger]/Btrigger)
    
    # between Blim2 and Blim
    Fnext[tmpG_Blim2 & tmpL_Blim] <- 
      FatBlim * (1 - (Blim - SSBs[tmpG_Blim2 & tmpL_Blim]) / (Blim - Blim2))
    
    # below Blim2
    Fnext[tmpL_Blim2] <- 0
    
    # plot(SSBs, Fnext)
    
    # ======================================================================
    
    # Is this right? Apply the F error after going through the HCR??
    Fnext         <- exp(Ferr[j, ]) * Fnext
    
    # get a selection pattern for each simulation and apply this to get N
    Zpre          <- rep(Fnext, 
                         each = length(Fprop)) * Fprop * sel[, rsamsel[j, ]] + M[, rsam[j, ]] * Mprop
    
    # MP: Why is the the F in the year before??
    Freal[, j-1, ]     <- rep(Fnext, 
                         each = ages) * sel[, rsamsel[j - 1, ]]
    
    # Calculate stock numbers given F
    Nreal[-1, j, ]   <- Nreal[1:(ages - 1), j - 1, ] * 
                     exp(-Freal[1:(ages - 1), j - 1, ] - M[1:(ages - 1), rsam[j - 1, ]])
    
    # plus group
    Nreal[ages, j, ] <- Nreal[ages,j,] + 
                     Nreal[ages,j-1,] * exp(-Freal[ages, j-1, ] - M[ages, rsam[j-1, ]])
    
    # calculate SSB
    ssbreal[j, ]     <- apply(array(Mat[, rsam[j, ]] * 
                                   Nreal[, j, ] * 
                                   west[, rsam[j, ]]/exp(Zpre), c(ages, Nits)), 2, sum)
    
    # calculate catch
    CNreal[, j, ]     <- Nreal[, j-1, ] * Freal[, j-1, ]/
                                  (Freal[, j-1, ] + M[, rsam[j-1, ]]) * (1 - exp(-Freal[,j-1, ] - M[,rsam[j-1, ]]))
    
    
    # update the loop data.frame for the previous year (i.e. for F)
    loop.df <-
      bind_rows(loop.df, 
                data.frame(year = j + max(an(dimnames(ssb(stk))$year)) - 1,
                           iter = an(names(tmpG_Blim)),
                           Fnext= Fnext, Ferr = Ferr[j,],
                           Blim2, Blim, Btrigger, Fmsy, FatBlim ) )
    
    # update the loop data.frame for the current year (i.e. for other than F)
    loop.df <-
      bind_rows(loop.df, 
                data.frame(year = j + max(an(dimnames(ssb(stk))$year)),
                           iter = an(names(tmpG_Blim)), 
                           SSBs=ssbreal[j,] * exp(SSBerr[j, ]), SSBerr=SSBerr[j,], 
                           tmpL_Blim2, tmpG_Blim2, tmpL_Blim, tmpG_Blim, tmpL_Btrigger, tmpG_Btrigger, 
                           Blim2, Blim, Btrigger, Fmsy, FatBlim) )
    
  } # end of run over j (years) =========================
  
  # Create worm data.frames for storing results
  WCreal.df <- 
    as.data.frame(t(as.data.frame(WCreal))) %>%
    setNames(paste0("age", unlist(dimnames(WCreal)[1]))) %>% 
    rownames_to_column(., "rowname") %>% 
    separate(rowname, into=c("year","iter"), sep="\\.") %>% 
    mutate(
      variable = "WCreal",
      iter     = as.numeric(iter), 
      Blim2    = Blim2
    ) %>% 
    gather(key="age", value="value", 
           colnames(.)[3:((length(colnames(.)))-1)]) %>% 
    mutate(
      age      = gsub("age", "", age)
    ) %>% 
    bind_rows(WCreal.df, .)
  
  Nreal.df <- 
    as.data.frame(t(as.data.frame(Nreal))) %>%
    setNames(paste0("age", unlist(dimnames(Nreal)[1]))) %>% 
    rownames_to_column(., "rowname") %>% 
    separate(rowname, into=c("year","iter"), sep="\\.") %>% 
    mutate(
      variable = "Nreal",
      year     = as.numeric(year) +   max(an(dimnames(ssb(stk))$year)) - 1,
      iter     = as.numeric(iter), 
      Blim2    = Blim2
    ) %>% 
    gather(key="age", value="value", 
           colnames(.)[3:((length(colnames(.)))-1)]) %>% 
    mutate(
      age      = gsub("age", "", age)
    ) %>% 
    bind_rows(Nreal.df, .)
  
  Cwy.df <- 
    as.data.frame(t(as.data.frame(CNreal * WCreal))) %>% 
    setNames(paste0("age", unlist(dimnames(CNreal)[1]))) %>% 
    rownames_to_column(., "rowname") %>% 
    separate(rowname, into=c("year","iter"), sep="\\.") %>% 
    mutate(
      variable = "Cwy",
      iter     = as.numeric(iter), 
      Blim2    = Blim2
    ) %>% 
    gather(key="age", value="value", 
           colnames(.)[3:((length(colnames(.)))-2)]) %>% 
    group_by(year, iter, Blim2, variable) %>% 
    dplyr::summarize(value = sum(value)) %>% 
    bind_rows(Cwy.df, .)
  
  Freal.df <- 
    as.data.frame(t(as.data.frame(Freal))) %>%
    setNames(paste0("age", unlist(dimnames(Freal)[1]))) %>% 
    rownames_to_column(., "rowname") %>% 
    separate(rowname, into=c("year","iter"), sep="\\.") %>% 
    mutate(
      variable = "Freal",
      iter     = as.numeric(iter), 
      Blim2    = Blim2
    ) %>% 
    gather(key="age", value="value", 
           colnames(.)[3:((length(colnames(.)))-1)]) %>% 
    mutate(
      age      = gsub("age", "", age),
      value    = an(value)
    ) %>% 
    bind_rows(Freal.df, .)
  
  Fbarreal.df <-
    Freal.df %>% 
    filter(age %in% an(stk@range["minfbar"]): an(stk@range["maxfbar"])) %>% 
    group_by(iter, year, Blim2) %>% 
    dplyr::summarize(value = mean(value, na.rm=TRUE)) %>% 
    mutate(variable = "fbary") %>% 
    bind_rows(Fbarreal.df, .) %>% 
    group_by(year, iter, Blim2, variable) %>% 
    filter(row_number() == 1)
  
  ssbreal.df <- 
    as.data.frame(t(as.data.frame(ssbreal))) %>%
    rownames_to_column(., "iter") %>% 
    gather(key="year", value="value", 
           colnames(.)[2:(length(colnames(.)))]) %>% 
    mutate(
      variable = "ssbreal",
      age      = "all",
      Blim2    = Blim2
    ) %>% 
    mutate_at(c("iter","year"), funs(as.integer)) %>% 
    bind_rows(ssbreal.df, .)
  
} # End of loop over i (scans for Blim2) -----------------------------------------





ssbs.df <- 
  select(loop.df, year, iter, Blim2, value=SSBs) %>% 
  mutate_at(c("iter","year", "Blim2"), funs(as.integer)) %>% 
  mutate(variable="ssbs", age="all") 

Fs.df <-
  select(loop.df, year, iter, Blim2, value=Fnext) %>% 
  mutate_at(c("iter","year","Blim2"), funs(as.integer)) %>% 
  mutate(variable="fs", age="all") 

ssbs2.df <-
  ssbs.df %>% 
  group_by(year, Blim2, variable) %>% 
  dplyr::summarize(mean = mean(value, na.rm=TRUE),
                   sd   = sd(value, na.rm=TRUE),
                   n    = n()) %>% 
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) 

probbelowBlim.df <-
  ssbs.df %>% 
  group_by(year, Blim2, variable) %>% 
  mutate(percbelowBlim = (value<Blim)/(!is.na(value)) ) %>% 
  summarize(percbelowBlim = mean(percbelowBlim, na.rm=TRUE))



# plot SSB over different values of Blim2
ssbs.df %>%
  filter(!is.na(value)) %>% 
  data.frame() %>% 
  ggplot(aes(year, value, group=iter)) +
  geom_path(aes(colour=factor(iter))) +
  # geom_ribbon(data=ssbs2.df, aes(x=year, ymin=lower.ci, ymax=upper.ci), fill="red", alpha=0.3, inherit.aes = FALSE) +
  geom_hline(aes(yintercept = Blim), colour="black", linetype ="dotted") +
  geom_hline(aes(yintercept = Btrigger), colour="black", linetype ="dashed") +
  facet_wrap(~Blim2) +
  expand_limits(y=0) +
  labs(title="SSB")

# plot Catch weight over different values of Blim2
Cwy.df %>%
  filter(year >= 2017) %>% 
  mutate(Blim2 = as.integer(Blim2)) %>% 
  ggplot(aes(year, value, group=iter)) +
  geom_line(aes(colour=factor(iter))) +
  facet_wrap(~Blim2)  +
  expand_limits(y=0) +
  labs(title="Catch")

# plot F over different values of Blim2
Fbarreal.df %>%
  filter(year %in% 2017:2029) %>% 
  mutate(Blim2 = as.integer(Blim2)) %>% 
  ggplot(aes(year, value, group=iter)) +
  geom_line(aes(colour=factor(iter))) +
  facet_wrap(~Blim2)  +
  expand_limits(y=0) +
  labs(title="F")

# plot F against SSB
bind_rows(ssbreal.df, Fbarreal.df) %>%
  filter(year %in% 2017:2029) %>% 
  select(-age) %>% 
  mutate(Blim2 = as.integer(Blim2)) %>%
  group_by(year, iter, Blim2, variable) %>% 
  spread(key=variable, value=value) %>% 
  
  ggplot(aes(ssbreal, fbary, group=iter)) +
  geom_line(aes(colour=factor(iter))) +
  facet_wrap(~Blim2)  +
  expand_limits(y=0, x=0) +
  labs(title="F vs SSB")

probbelowBlim.df %>%
  ungroup() %>% 
  filter(year %in% 2018:2029) %>% 
  mutate(Blim2 = as.integer(Blim2)) %>% 
  
  ggplot(aes(year, percbelowBlim, group=Blim2)) +
  geom_line(aes(colour=factor(Blim2))) +
  geom_hline(aes(yintercept = 0.05), colour="blue", line.type="dashed") +
  expand_limits(y=0) +
  labs(title="perc below Blim")

bind_rows(ssbreal.df, ssbs.df) %>% 
  ggplot(aes(year, value, group=variable)) +
  geom_line(aes(colour=variable)) +
  geom_hline(aes(yintercept = Blim), colour="black", linetype ="dotted") +
  geom_hline(aes(yintercept = Btrigger), colour="black", linetype ="dashed") +
  facet_wrap(~iter)

bind_rows(Fbarreal.df, Fs.df) %>%
  ggplot(aes(year, value, group=variable)) +
  geom_line(aes(colour=variable)) +
  geom_hline(aes(yintercept = Fmsy), colour="black", linetype ="dashed") +
  facet_wrap(~iter)

# plot trends of averages below Blim, above Blim and above Btrigger
select(loop.df, year, iter, Blim2, SSBs, tmpL_Blim, tmpG_Blim, tmpG_Btrigger) %>% 
  mutate(
    tmpL_Blim     = ifelse(tmpL_Blim    ==TRUE, 1  , 0  ),
    tmpG_Blim     = ifelse(tmpG_Blim    ==TRUE, 1  , 0  ),
    tmpG_Btrigger = ifelse(tmpG_Btrigger==TRUE, 1  , 0  )
  ) %>% 
  gather(key=refpoint, value=value, tmpL_Blim:tmpG_Btrigger) %>% 
  group_by(year, Blim2, refpoint) %>% 
  dplyr::summarize(value = mean(value, na.rm=TRUE)) %>% 
  mutate(refpoint = factor(refpoint, levels=c("tmpL_Blim","tmpG_Blim","tmpG_Btrigger"))) %>% 
  
  ggplot(aes(year, value, group=Blim2)) +
  geom_line(aes(colour=factor(Blim2))) +
  facet_grid(refpoint~Blim2)


# Cw     <- CNreal * WCreal
# land   <- CNreal * Rreal * WLreal
# Lan    <- apply(land, 2:3, sum)
# Cat    <- apply(Cw, 2:3, sum)
# quants <- c( 0.05, 0.25, 0.5, 0.75, 0.95)
# 
# yrrs   <- an(dimnames(ssb(stk))$year)
# # yrrs   <- seq(min(yrrs),max(yrrs)+100,1)
# yrrs   <- seq(min(yrrs),max(yrrs)+Nyear,1)
# 
# ssbpast    <- as.data.frame(ssb(stk)@.Data)
# ssbres     <- as.data.frame(t(apply(ssbreal,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
# ssbpst     <- ssbres[1:dim(ssbpast)[2],]
# ssbpst[]   <- t(ssbpast)
# ssbres     <- rbind(ssbpst,ssbres)
# ssbres     <- cbind(yrrs,ssbres)
# ssbres$var <- "ssb"
# 
# 
# 
# # Risk to Blim
# # yrrs   <- an(dimnames(ssb(stk))$year)
# # yrrs   <- seq(min(yrrs),max(yrrs)+100,1)
# rskpast <- as.data.frame(ssb(stk)@.Data)
# rskpast[]<-0
# rskres<-rowSums (data.frame(ssbreal<Blim)) /  rowSums (data.frame(!is.na(ssbreal))) 
# rskpst<-rskres[1:dim(rskpast)[2]]
# rskpst[] <-0
# rskres  <-   c(rskpst,rskres)
# rskres  <-   data.frame(yyrs= yrrs,"5%" = NA , "25%"= NA , "50%" = rskres, "75%"= NA ,"95%"= NA , var= "risk")
# rskres$var <-    "risk"
# names(rskres) <- names(ssbres)
# 
# # Interannual Variation
# # yrrs   <- an(dimnames(ssb(stk))$year)
# # yrrs <- seq(min(yrrs),max(yrrs)+100,1)
# iavpast <- as.data.frame(ssb(stk)@.Data)
# iavpast[]<-0
# iavres<-apply(Cat, 2 ,FUN= function(x)  {(x[-1]-x[-length(x)])  / x[-1] }) 
# 
# 
# cc        <- as.data.frame(Cat)
# cd        <- apply(Cat, 2 ,FUN= function(x)  {(x[-1]-x[-length(x)])  / x[-1] })
# 
# # yrrs      <- an(dimnames(catch(stk))$year)
# # yrrs      <- seq(min(yrrs),max(yrrs)+100,1)
# 
# catchpast <- as.data.frame(catch(stk)@.Data)
# catchres<-as.data.frame(t(apply(Cat,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
# catchpst<-catchres[1:dim(catchpast)[2],]
# catchpst[] <-  t(catchpast)
# catchres  <-   rbind(catchpst,catchres)
# catchres  <-   cbind(yrrs,catchres)
# catchres$var <- "catch"
# catchres[catchres==0]<-NA
# 
# 
# yrrs   <- an(dimnames(landings(stk))$year)
# yrrs <- seq(min(yrrs),max(yrrs)+100,1)
# 
# landpast <- as.data.frame(landings(stk)@.Data)
# landres<-as.data.frame(t(apply(Lan,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
# landpst<-landres[1:dim(landpast)[2],]
# landpst[] <-  t(landpast)
# landres  <-   rbind(landpst,landres)
# landres  <-   cbind(yrrs,landres)
# landres$var <- "landings"
# landres[landres==0]<-NA
# 
# yrrs   <- an(dimnames(fbar(stk))$year)
# yrrs <- seq(min(yrrs),max(yrrs)+100,1)
# 
# Fbarpast <- as.data.frame(fbar(stk)@.Data)
# Fbarres<-as.data.frame(t(apply(Fbarreal,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
# Fbarpst<-Fbarres[1:dim(Fbarpast)[2],]
# Fbarpst[] <-  t(Fbarpast)
# Fbarres  <-   rbind(Fbarpst,Fbarres)
# Fbarres  <-   cbind(yrrs,Fbarres)
# Fbarres$var <- "Fbar"
# Fbarres[Fbarres==0]<-NA
# 
# #Store individual run results
# 
# simStks[[ac(Blim2)]] <- list()
# simStks[[ac(Blim2)]][["N"]]       <- Nreal
# simStks[[ac(Blim2)]][["F"]]       <- Freal
# simStks[[ac(Blim2)]][["catW"]]    <- WCreal
# simStks[[ac(Blim2)]][["lanW"]]    <- WLreal
# simStks[[ac(Blim2)]][["stkW"]]    <- west
# simStks[[ac(Blim2)]][["stkWran"]] <- rsam
# simStks[[ac(Blim2)]][["C"]]       <- CNreal
# simStks[[ac(Blim2)]][["L"]]       <- CNreal * Rreal
# simStks[[ac(Blim2)]][["percSSB"]] <- ssbpast
# simStks[[ac(Blim2)]][["intendF"]] <- Fbarreal
# 
# # plot(simStks[[ac(Blim2)]][["intendF"]])
# as.data.frame(Freal)
# res <- rbind(ssbres,rbind(catchres,rbind(landres,rbind(Fbarres,rskres))))
# names(res) <- c("years","Q.05","Q.25","Q.50","Q.75","Q.95","variable")
# 
# 
# return(list(sim=res,iavTAC = iavres, simStks = simStks) )

# }



