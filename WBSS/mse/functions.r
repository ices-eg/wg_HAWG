# -------------------------------------------------------------------------------------------
# functions.r
# 
# based on the WMR training course on Stock assessment (2017) with standard eqsim code
# Runs with 3.3.3
# -------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
# EQSIM_RUN2: 'standard eqsim'
# ---------------------------------------------------------------------------------------

eqsim_run2 <-   function (fit, bio.years = c(2008, 2012), bio.const = FALSE,
                sel.years = c(2008, 2012), sel.const = FALSE, Fscan = seq(0,
                1, len = 20), Fcv = 0, Fphi = 0, SSBcv = 0, rhologRec = FALSE,
                Blim, Bpa, recruitment.trim = c(3, -3), Ftarget = 0.2 , Btrigger = 0, Nrun = 200, Nits = 50 ,
                process.error = TRUE, verbose = TRUE, extreme.trim)

                 # fit = FIT;
                 # bio.years = c(2011, 2015);
                 # bio.const = FALSE;
                 # sel.years = c(2006, 2015);
                 # sel.const = FALSE;
                 # Fscan = seq(0.01,0.8 , len = 80);
                 # Fcv = 0.35; Fphi = 0.61;
                 # SSBcv = 0.31;
                 # rhologRec = F;
                 # Blim=1940000; Bpa=2570000;
                 # Ftarget = 0.2 ;
                 # Btrigger = 0;
                 # Nrun = 100;
                 # Nits =3 ;
                 # process.error = TRUE;
                 # verbose = TRUE
                 # recruitment.trim = c(3, -3)



{
    if (abs(Fphi) >= 1)
        stop("Fphi, the autocorelation parameter for log F should be between (-1, 1)")
    if ((recruitment.trim[1] + recruitment.trim[2]) > 0)
        stop("recruitment truncation must be between a high - low range")
    
    btyr1 <- bio.years[1]
    btyr2 <- bio.years[2]
    slyr1 <- sel.years[1]
    slyr2 <- sel.years[2]
    
    keep <- Nrun
    
    
    # stock recruitment estimates
    SR <- fit$sr.sto[sample(1:dim(fit$sr.sto)[1] , Nits , replace = F),]
    data <- fit$rby[, c("rec", "ssb", "year")]
    stk <- fit$stk
    stk.win <- FLCore::window(stk, start = btyr1, end = btyr2)
    stk.winsel <- FLCore::window(stk, start = slyr1, end = slyr2)
    
    littleHelper <- function(x, i) {
        x2 <- x
        x2[i] <- NA
        x2[] <- apply(x2, 1, mean, na.rm = TRUE)
        x[i] <- x2[i]
        return(x)
    }
    
    # weight in the stock
    west <- matrix(FLCore::stock.wt(stk.win), ncol = btyr2 -
        btyr1 + 1)
    i <- west == 0
    if (any(i))
        west <- littleHelper(west, i)
    weca <- matrix(FLCore::catch.wt(stk.win), ncol = btyr2 -
        btyr1 + 1)
    i <- weca == 0
    if (any(i))
        weca <- littleHelper(weca, i)
    wela <- matrix(FLCore::landings.wt(stk.win), ncol = btyr2 -
        btyr1 + 1)
    if (any(i))
        wela <- littleHelper(wela, i)
    Mat <- matrix(FLCore::mat(stk.win), ncol = btyr2 - btyr1 +
        1)
    M <- matrix(FLCore::m(stk.win), ncol = btyr2 - btyr1 + 1)
    landings <- matrix(FLCore::landings.n(stk.winsel), ncol = slyr2 -
        slyr1 + 1)
    catch <- matrix(FLCore::catch.n(stk.winsel), ncol = slyr2 -
        slyr1 + 1)
    sel <- matrix(FLCore::harvest(stk.winsel), ncol = slyr2 -
        slyr1 + 1)
    Fbar <- matrix(FLCore::fbar(stk.winsel), ncol = slyr2 - slyr1 +
        1)
    sel <- sweep(sel, 2, Fbar, "/")
    if (sel.const == TRUE) {
        sel[] <- apply(sel, 1, mean)
        landings[] <- apply(landings, 1, mean)
        catch[] <- apply(catch, 1, mean)
    }
    if (bio.const == TRUE) {
        west[] <- apply(west, 1, mean)
        weca[] <- apply(weca, 1, mean)
        wela[] <- apply(wela, 1, mean)
        Mat[] <- apply(Mat, 1, mean)
        M[] <- apply(M, 1, mean)
    }
    land.cat = landings/catch
    i <- is.na(land.cat)
    if (any(i))
        land.cat[i] <- 1
    Fprop <- apply(FLCore::harvest.spwn(stk.winsel), 1, mean)[drop = TRUE]
    Mprop <- apply(FLCore::m.spwn(stk.win), 1, mean)[drop = TRUE]
    Nmod <- nrow(SR)
    ages <- FLCore::dims(stk)$age
    ssby <- Freal <- Ferr <- array(0, c(Nrun, Nmod), dimnames = list(year = 1:Nrun,
        iter = 1:Nmod))
    Ny <- Fy <- WSy <- WCy <- Cy <- Wy <- Wl <- Ry <- array(0,
        c(ages, Nrun, Nmod), dimnames = list(age = (range(stk)[1]:range(stk)[2]),
            year = 1:Nrun, iter = 1:Nmod))
    Ferr[1, ] <- stats::rnorm(n = Nmod, mean = 0, sd = 1) * Fcv/sqrt(1 -
        Fphi^2)
    for (j in 2:Nrun) {
        Ferr[j, ] <- Fphi * Ferr[j - 1, ] + Fcv * stats::rnorm(n = Nmod,
            mean = 0, sd = 1)
    }
    SSBerr <- matrix(stats::rnorm(n = Nrun * Nmod, mean = 0,
        sd = 1), ncol = Nmod) * SSBcv
    rsam <- array(sample(1:ncol(weca), Nrun * Nmod, TRUE), c(Nrun,
        Nmod))
    rsamsel <- array(sample(1:ncol(sel), Nrun * Nmod, TRUE),
        c(Nrun, Nmod))
    Wy[] <- c(weca[, c(rsam)])
    Wl[] <- c(wela[, c(rsam)])
    Ry[] <- c(land.cat[, c(rsamsel)])
    R <- mean(data$rec)
    NF<-1
    ssbs <- cats <- lans <- recs <- array(0, c(7, NF))
    ferr <- ssbsa <- catsa <- lansa <- recsa <- array(0, c(NF,
        keep, Nmod))
    begin <- Nrun - keep + 1
    resids = array(stats::rnorm(Nmod * (Nrun + 1), 0, SR$cv),
        c(Nmod, Nrun + 1))
    if (rhologRec) {
        fittedlogRec <- do.call(cbind, lapply(c(1:nrow(fit$sr.sto)),
            function(i) {
                FUN <- match.fun(fit$sr.sto$model[i])
                FUN(fit$sr.sto[i, ], fit$rby$ssb)
            }))
        rhologRec <- apply(log(fit$rby$rec) - fittedlogRec, 2,
            function(x) {
                stats::cor(x[-length(x)], x[-1])
            })
        for (j in 2:(Nrun + 1)) {
            resids[, j] <- rhologRec * resids[, j - 1] + resids[,
                j] * sqrt(1 - rhologRec^2)
        }
    }
    lims = t(array(SR$cv, c(Nmod, 2))) * recruitment.trim
    for (k in 1:Nmod) {
        resids[k, resids[k, ] > lims[1, k]] = lims[1, k]
    }
    for (k in 1:Nmod) {
        resids[k, resids[k, ] < lims[2, k]] = lims[2, k]
    }

        Fbar <- Ftarget
        Zpre <- (sel[, rsamsel[1, ]] * Fbar * Fprop + M[, rsam[1,
            ]] * Mprop)
        Zpos <- (Fbar * (1 - Fprop) * sel[, rsamsel[1, ]] + M[,
            rsam[1, ]] * (1 - Mprop))
        Zcum <- c(0, cumsum(Fbar * sel[c(1:ages, rep(ages, 49 -
            ages)), rsamsel[1, ]] + M[c(1:ages, rep(ages, 49 -
            ages)), rsam[1, ]]))

        # compute survivors
        N <- stock.n(stk)[,ac(max(sel.years))]  * exp (-(harvest(stk)[,ac(max(sel.years))] + m(stk)[,ac(max(sel.years))]))
        N[dim(N)[1]-1 , ]   <-  N[dim(N)[1]-1 , ] + N[dim(N)[1] , ]
        N[-1] <-  N[-dim(N)[1] , ]
        N[1] <- R
            

        Ny[, 1, ] <- c(N@.Data)
        ssby[1, ] <- colSums(Mat[, rsam[1, ]] * Ny[, 1, ] * west[,
            rsam[1, ]]/exp(Zpre)[])
            
            
            
        for (j in 2:Nrun) {
            SSB <- ssby[j - 1, ]
            if (process.error) {
                allrecs <- sapply(unique(SR$mod), function(mod) exp(match.fun(mod)(SR,
                  SSB) + resids[, j]))
            }
            else {
                allrecs <- sapply(unique(SR$mod), function(mod) exp(match.fun(mod)(SR,
                  SSB)))
            }
            select <- cbind(seq(Nmod), as.numeric(factor(SR$mod,
                levels = unique(SR$mod))))
            Ny[1, j, ] <- allrecs[select]
            
            Fnext <- Fbar * pmin(1, SSB * exp(SSBerr[j, ])/Btrigger)
            
            Fnext <- exp(Ferr[j, ]) * Fnext
            Freal[j, ] <- Fnext
            Zpre <- rep(Fnext, each = length(Fprop)) * Fprop *
                sel[, rsamsel[j, ]] + M[, rsam[j, ]] * Mprop
            Fy[, j - 1, ] <- rep(Fnext, each = ages) * sel[,
                rsamsel[j - 1, ]]
            Ny[-1, j, ] <- Ny[1:(ages - 1), j - 1, ] * exp(-Fy[1:(ages -
                1), j - 1, ] - M[1:(ages - 1), rsam[j - 1, ]])
            Ny[ages, j, ] <- Ny[ages, j, ] + Ny[ages, j - 1,
                ] * exp(-Fy[ages, j - 1, ] - M[ages, rsam[j -
                1, ]])
            ssby[j, ] <- apply(array(Mat[, rsam[j, ]] * Ny[,
                j, ] * west[, rsam[j, ]]/exp(Zpre), c(ages, Nmod)),
                2, sum)
            Cy[, j, ] <- Ny[, j - 1, ] * Fy[, j - 1, ]/(Fy[,
                j - 1, ] + M[, rsam[j - 1, ]]) * (1 - exp(-Fy[,
                j - 1, ] - M[, rsam[j - 1, ]]))
        }
        Cw <- Cy * Wy
        land <- Cy * Ry * Wl
        Lan = apply(land, 2:3, sum)
        Cat <- apply(Cw, 2:3, sum)
        quants <- c( 0.05, 0.25, 0.5, 0.75, 0.95)

        
        

        yrrs   <- an(dimnames(ssb(stk))$year)
        yrrs <- seq(min(yrrs),max(yrrs)+100,1)
        
        ssbpast <- as.data.frame(ssb(stk)@.Data)
        ssbres<-as.data.frame(t(apply(ssby,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
        ssbpst<-ssbres[1:dim(ssbpast)[2],]
        ssbpst[] <-  t(ssbpast)
        ssbres  <-   rbind(ssbpst,ssbres)
        ssbres  <-   cbind(yrrs,ssbres)
        ssbres$var <- "ssb"


        yrrs   <- an(dimnames(ssb(stk))$year)
        yrrs <- seq(min(yrrs),max(yrrs)+100,1)
        rskpast <- as.data.frame(ssb(stk)@.Data)
        rskpast[]<-0
        rskres<-rowSums (data.frame(ssby<Blim)) /  rowSums (data.frame(!is.na(ssby))) 
        rskpst<-rskres[1:dim(rskpast)[2]]
        rskpst[] <-0
        rskres  <-   c(rskpst,rskres)
        rskres  <-   data.frame(yyrs= yrrs,"5%" = NA , "25%"= NA , "50%" = rskres, "75%"= NA ,"95%"= NA , var= "risk")
        rskres$var <-    "risk"
        names(rskres) <- names(ssbres)

        yrrs   <- an(dimnames(ssb(stk))$year)
        yrrs <- seq(min(yrrs),max(yrrs)+100,1)
        iavpast <- as.data.frame(ssb(stk)@.Data)
        iavpast[]<-0
        iavres<-apply(Cat, 2 ,FUN= function(x)  {(x[-1]-x[-length(x)])  / x[-1] }) 

        cc<-as.data.frame(Cat)
        cd<-apply(Cat, 2 ,FUN= function(x)  {(x[-1]-x[-length(x)])  / x[-1] })
        
        yrrs   <- an(dimnames(catch(stk))$year)
        yrrs <- seq(min(yrrs),max(yrrs)+100,1)

        catchpast <- as.data.frame(catch(stk)@.Data)
        catchres<-as.data.frame(t(apply(Cat,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
        catchpst<-catchres[1:dim(catchpast)[2],]
        catchpst[] <-  t(catchpast)
        catchres  <-   rbind(catchpst,catchres)
        catchres  <-   cbind(yrrs,catchres)
        catchres$var <- "catch"
        catchres[catchres==0]<-NA
        
        
        yrrs   <- an(dimnames(landings(stk))$year)
        yrrs <- seq(min(yrrs),max(yrrs)+100,1)

        landpast <- as.data.frame(landings(stk)@.Data)
        landres<-as.data.frame(t(apply(Lan,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
        landpst<-landres[1:dim(landpast)[2],]
        landpst[] <-  t(landpast)
        landres  <-   rbind(landpst,landres)
        landres  <-   cbind(yrrs,landres)
        landres$var <- "landings"
        landres[landres==0]<-NA

        yrrs   <- an(dimnames(fbar(stk))$year)
        yrrs <- seq(min(yrrs),max(yrrs)+100,1)

        Fbarpast <- as.data.frame(fbar(stk)@.Data)
        Fbarres<-as.data.frame(t(apply(Freal,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
        Fbarpst<-Fbarres[1:dim(Fbarpast)[2],]
        Fbarpst[] <-  t(Fbarpast)
        Fbarres  <-   rbind(Fbarpst,Fbarres)
        Fbarres  <-   cbind(yrrs,Fbarres)
        Fbarres$var <- "Fbar"
        Fbarres[Fbarres==0]<-NA

        
   res <- rbind(ssbres,rbind(catchres,rbind(landres,rbind(Fbarres,rskres))))
  names(res) <- c("years","Q.05","Q.25","Q.50","Q.75","Q.95","variable")
        

    return(list(sim=res,iavTAC = iavres) )
}

# ---------------------------------------------------------------------------------------
# EQSIM_RUN3
# new eqsim with HCR also applicable below Blim
# Martin Pastoors, 5/7/2018
# ---------------------------------------------------------------------------------------

eqsim_run3 <-   function (fit, 
                          bio.years = c(2008, 2012), 
                          bio.const = FALSE,
                          sel.years = c(2008, 2012), 
                          sel.const = FALSE, 
                          Fscan     = 0.2, 
                          Fcv       = 0, 
                          Fphi      = 0, 
                          SSBcv     = 0, 
                          rhologRec = FALSE,
                          Blim, 
                          Bpa, 
                          recruitment.trim = c(3, -3), 
                          Ftarget   = 0.2 , 
                          Btrigger  = 0, 
                          Bzero     = 0, 
                          Nrun      = 200, 
                          Nits      = 50 ,
                          process.error = TRUE, 
                          verbose = TRUE, 
                          extreme.trim)
  
# fit = FIT
# bio.years = c(2007, 2016)
# bio.const = FALSE
# sel.years = c(2014, 2016)
# sel.const = FALSE
# Fscan     = 0.25
# Ftarget   = 0.25
# Fcv       = 0.23
# Fphi      = 0.24
# SSBcv     = 0.31
# rhologRec = F
# Blim      = 26300
# Bpa       = 37000
# Btrigger  = 37000
# Bzero     = 20000
# Nrun      = 100
# Nits      = 500
# process.error = TRUE
# verbose = TRUE
# recruitment.trim = c(3, -3)



{
  if (abs(Fphi) >= 1)
    stop("Fphi, the autocorelation parameter for log F should be between (-1, 1)")
  if ((recruitment.trim[1] + recruitment.trim[2]) > 0)
    stop("recruitment truncation must be between a high - low range")
  
  btyr1 <- bio.years[1]
  btyr2 <- bio.years[2]
  slyr1 <- sel.years[1]
  slyr2 <- sel.years[2]
  
  keep <- Nrun
  
  # list to store individual runs afterwards
  simStks <- list()
  
  SR <- fit$sr.sto[sample(1:dim(fit$sr.sto)[1] , Nits , replace = F),]
  data <- fit$rby[, c("rec", "ssb", "year")]
  stk <- fit$stk
  stk.win <- FLCore::window(stk, start = btyr1, end = btyr2)
  stk.winsel <- FLCore::window(stk, start = slyr1, end = slyr2)
  
  littleHelper <- function(x, i) {
    x2 <- x
    x2[i] <- NA
    x2[] <- apply(x2, 1, mean, na.rm = TRUE)
    x[i] <- x2[i]
    return(x)
  }
  
  west <- matrix(FLCore::stock.wt(stk.win), ncol = btyr2 -
                   btyr1 + 1)
  i <- west == 0
  if (any(i))
    west <- littleHelper(west, i)
  weca <- matrix(FLCore::catch.wt(stk.win), ncol = btyr2 -
                   btyr1 + 1)
  i <- weca == 0
  if (any(i))
    weca <- littleHelper(weca, i)
  wela <- matrix(FLCore::landings.wt(stk.win), ncol = btyr2 -
                   btyr1 + 1)
  if (any(i))
    wela <- littleHelper(wela, i)
  Mat <- matrix(FLCore::mat(stk.win), ncol = btyr2 - btyr1 +
                  1)
  M <- matrix(FLCore::m(stk.win), ncol = btyr2 - btyr1 + 1)
  landings <- matrix(FLCore::landings.n(stk.winsel), ncol = slyr2 -
                       slyr1 + 1)
  catch <- matrix(FLCore::catch.n(stk.winsel), ncol = slyr2 -
                    slyr1 + 1)
  sel <- matrix(FLCore::harvest(stk.winsel), ncol = slyr2 -
                  slyr1 + 1)
  Fbar <- matrix(FLCore::fbar(stk.winsel), ncol = slyr2 - slyr1 +
                   1)
  sel <- sweep(sel, 2, Fbar, "/")
  if (sel.const == TRUE) {
    sel[] <- apply(sel, 1, mean)
    landings[] <- apply(landings, 1, mean)
    catch[] <- apply(catch, 1, mean)
  }
  if (bio.const == TRUE) {
    west[] <- apply(west, 1, mean)
    weca[] <- apply(weca, 1, mean)
    wela[] <- apply(wela, 1, mean)
    Mat[] <- apply(Mat, 1, mean)
    M[] <- apply(M, 1, mean)
  }
  land.cat = landings/catch
  i <- is.na(land.cat)
  if (any(i))
    land.cat[i] <- 1
  Fprop <- apply(FLCore::harvest.spwn(stk.winsel), 1, mean)[drop = TRUE]
  Mprop <- apply(FLCore::m.spwn(stk.win), 1, mean)[drop = TRUE]
  Nmod <- nrow(SR)
  ages <- FLCore::dims(stk)$age
  ssby <- Freal <- Ferr <- array(0, c(Nrun, Nmod), dimnames = list(year = 1:Nrun,
                                                                   iter = 1:Nmod))
  Ny <- Fy <- WSy <- WCy <- Cy <- Wy <- Wl <- Ry <- array(0,
                                                          c(ages, Nrun, Nmod), dimnames = list(age = (range(stk)[1]:range(stk)[2]),
                                                                                               year = 1:Nrun, iter = 1:Nmod))
  Ferr[1, ] <- stats::rnorm(n = Nmod, mean = 0, sd = 1) * Fcv/sqrt(1 -
                                                                     Fphi^2)
  for (j in 2:Nrun) {
    Ferr[j, ] <- Fphi * Ferr[j - 1, ] + Fcv * stats::rnorm(n = Nmod,
                                                           mean = 0, sd = 1)
  }
  SSBerr <- matrix(stats::rnorm(n = Nrun * Nmod, mean = 0,
                                sd = 1), ncol = Nmod) * SSBcv
  rsam <- array(sample(1:ncol(weca), Nrun * Nmod, TRUE), c(Nrun,
                                                           Nmod))
  rsamsel <- array(sample(1:ncol(sel), Nrun * Nmod, TRUE),
                   c(Nrun, Nmod))
  Wy[] <- c(weca[, c(rsam)])
  Wl[] <- c(wela[, c(rsam)])
  Ry[] <- c(land.cat[, c(rsamsel)])
  R <- mean(data$rec)
  NF<-1
  ssbs <- cats <- lans <- recs <- array(0, c(7, NF))
  ferr <- ssbsa <- catsa <- lansa <- recsa <- array(0, c(NF,
                                                         keep, Nmod))
  begin <- Nrun - keep + 1
  resids = array(stats::rnorm(Nmod * (Nrun + 1), 0, SR$cv),
                 c(Nmod, Nrun + 1))
  if (rhologRec) {
    fittedlogRec <- do.call(cbind, lapply(c(1:nrow(fit$sr.sto)),
                                          function(i) {
                                            FUN <- match.fun(fit$sr.sto$model[i])
                                            FUN(fit$sr.sto[i, ], fit$rby$ssb)
                                          }))
    rhologRec <- apply(log(fit$rby$rec) - fittedlogRec, 2,
                       function(x) {
                         stats::cor(x[-length(x)], x[-1])
                       })
    for (j in 2:(Nrun + 1)) {
      resids[, j] <- rhologRec * resids[, j - 1] + resids[,
                                                          j] * sqrt(1 - rhologRec^2)
    }
  }
  lims = t(array(SR$cv, c(Nmod, 2))) * recruitment.trim
  for (k in 1:Nmod) {
    resids[k, resids[k, ] > lims[1, k]] = lims[1, k]
  }
  for (k in 1:Nmod) {
    resids[k, resids[k, ] < lims[2, k]] = lims[2, k]
  }
  
  Fbar <- Ftarget
  Zpre <- (sel[, rsamsel[1, ]] * Fbar * Fprop + M[, rsam[1,
                                                         ]] * Mprop)
  Zpos <- (Fbar * (1 - Fprop) * sel[, rsamsel[1, ]] + M[,
                                                        rsam[1, ]] * (1 - Mprop))
  Zcum <- c(0, cumsum(Fbar * sel[c(1:ages, rep(ages, 49 -
                                                 ages)), rsamsel[1, ]] + M[c(1:ages, rep(ages, 49 -
                                                                                           ages)), rsam[1, ]]))
  
  # compute survivors
  # =======================================================================================
  
  N <- stock.n(stk)[,ac(max(sel.years))]  * exp (-(harvest(stk)[,ac(max(sel.years))] + m(stk)[,ac(max(sel.years))]))
  
  # Or: make it is bit more dramatic; divide survivors by three
  # N <- 0.33*stock.n(stk)[,ac(max(sel.years))]  * exp (-(harvest(stk)[,ac(max(sel.years))] + m(stk)[,ac(max(sel.years))]))
  # =======================================================================================
  
  N[dim(N)[1]-1 , ]   <-  N[dim(N)[1]-1 , ] + N[dim(N)[1] , ]
  N[-1] <-  N[-dim(N)[1] , ]
  N[1] <- R
  
  
  Ny[, 1, ] <- c(N@.Data)
  ssby[1, ] <- colSums(Mat[, rsam[1, ]] * Ny[, 1, ] * west[,
                                                           rsam[1, ]]/exp(Zpre)[])
  
  
  # j <- 2
  # Nrun is the number of years to run the simulation
  for (j in 2:Nrun) {
    
    SSB <- ssby[j - 1, ]
    
    if (process.error) {
      allrecs <- sapply(unique(SR$mod), function(mod) exp(match.fun(mod)(SR,
                                                                         SSB) + resids[, j]))
    } else {
      allrecs <- sapply(unique(SR$mod), function(mod) exp(match.fun(mod)(SR,
                                                                         SSB)))
    }
    select <- cbind(seq(Nmod), as.numeric(factor(SR$mod,
                                                 levels = unique(SR$mod))))
    Ny[1, j, ] <- allrecs[select]
    
    # ======================================================================
    # ======================================================================
    # This is where the F for next year is determined; i.e. the HCR
    
    # standard MSY rule: 
    # Fnext <- Fbar * pmin(1, SSB * exp(SSBerr[j, ])/Btrigger)
    
    # Tester
    # SSBs    <- seq(from=0, to=50000, length.out = 500)
    
    SSBs    <- SSB * exp(SSBerr[j-1, ]) # get perceived SSBs
    FatBlim <- Ftarget * (Blim/Btrigger) # calculate F at Blim according to MSY rule
    Fnext <- Ftarget * pmin(1, SSBs * exp(SSBerr[j-1,])) #initiate Fnext object
    
    # Temporary vectors above and below Bzero, Blim and Bpa
    tmpL_Bzero    <- c(SSBs <  Bzero)
    tmpG_Bzero    <- c(SSBs >= Bzero)
    tmpL_Blim     <- c(SSBs <  Blim); 
    tmpG_Blim     <- c(SSBs >= Blim); 
    tmpL_Btrigger <- c(SSBs <  Btrigger)
    tmpG_Btrigger <- c(SSBs >= Btrigger)
    
    # above Btrigger
    Fnext[tmpG_Btrigger] <- Ftarget
    
    # between Blim and Btrigger
    Fnext[tmpG_Blim & tmpL_Btrigger] <- 
      Ftarget * (SSBs[tmpG_Blim & tmpL_Btrigger]/Btrigger)
    
    # between Bzero and Blim
    Fnext[tmpG_Bzero & tmpL_Blim] <- 
      FatBlim * (1 - (Blim - SSBs[tmpG_Bzero & tmpL_Blim]) / (Blim - Bzero))

    # below Bzero
    Fnext[tmpL_Bzero] <- 0
    
    # plot(SSBs, Fnext)
    
    # ======================================================================
    # ======================================================================
    
    Fnext <- exp(Ferr[j, ]) * Fnext
    Freal[j, ] <- Fnext
    Zpre <- rep(Fnext, each = length(Fprop)) * Fprop *
      sel[, rsamsel[j, ]] + M[, rsam[j, ]] * Mprop
    Fy[, j - 1, ] <- rep(Fnext, each = ages) * sel[,
                                                   rsamsel[j - 1, ]]
    Ny[-1, j, ] <- Ny[1:(ages - 1), j - 1, ] * exp(-Fy[1:(ages -
                                                            1), j - 1, ] - M[1:(ages - 1), rsam[j - 1, ]])
    Ny[ages, j, ] <- Ny[ages, j, ] + Ny[ages, j - 1,
                                        ] * exp(-Fy[ages, j - 1, ] - M[ages, rsam[j -
                                                                                    1, ]])
    ssby[j, ] <- apply(array(Mat[, rsam[j, ]] * Ny[,
                                                   j, ] * west[, rsam[j, ]]/exp(Zpre), c(ages, Nmod)),
                       2, sum)
    Cy[, j, ] <- Ny[, j - 1, ] * Fy[, j - 1, ]/(Fy[,
                                                   j - 1, ] + M[, rsam[j - 1, ]]) * (1 - exp(-Fy[,
                                                                                                 j - 1, ] - M[, rsam[j - 1, ]]))
  } # end of run over j (years)
  
  Cw <- Cy * Wy
  land <- Cy * Ry * Wl
  Lan = apply(land, 2:3, sum)
  Cat <- apply(Cw, 2:3, sum)
  quants <- c( 0.05, 0.25, 0.5, 0.75, 0.95)
  
  
  
  
  yrrs   <- an(dimnames(ssb(stk))$year)
  yrrs <- seq(min(yrrs),max(yrrs)+100,1)
  
  ssbpast <- as.data.frame(ssb(stk)@.Data)
  ssbres<-as.data.frame(t(apply(ssby,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
  ssbpst<-ssbres[1:dim(ssbpast)[2],]
  ssbpst[] <-  t(ssbpast)
  ssbres  <-   rbind(ssbpst,ssbres)
  ssbres  <-   cbind(yrrs,ssbres)
  ssbres$var <- "ssb"
  
  
  yrrs   <- an(dimnames(ssb(stk))$year)
  yrrs <- seq(min(yrrs),max(yrrs)+100,1)
  rskpast <- as.data.frame(ssb(stk)@.Data)
  rskpast[]<-0
  rskres<-rowSums (data.frame(ssby<Blim)) /  rowSums (data.frame(!is.na(ssby))) 
  rskpst<-rskres[1:dim(rskpast)[2]]
  rskpst[] <-0
  rskres  <-   c(rskpst,rskres)
  rskres  <-   data.frame(yyrs= yrrs,"5%" = NA , "25%"= NA , "50%" = rskres, "75%"= NA ,"95%"= NA , var= "risk")
  rskres$var <-    "risk"
  names(rskres) <- names(ssbres)
  
  yrrs   <- an(dimnames(ssb(stk))$year)
  yrrs <- seq(min(yrrs),max(yrrs)+100,1)
  iavpast <- as.data.frame(ssb(stk)@.Data)
  iavpast[]<-0
  iavres<-apply(Cat, 2 ,FUN= function(x)  {(x[-1]-x[-length(x)])  / x[-1] }) 
  
  
  
  
  cc<-as.data.frame(Cat)
  cd<-apply(Cat, 2 ,FUN= function(x)  {(x[-1]-x[-length(x)])  / x[-1] })
  
  yrrs   <- an(dimnames(catch(stk))$year)
  yrrs <- seq(min(yrrs),max(yrrs)+100,1)
  
  catchpast <- as.data.frame(catch(stk)@.Data)
  catchres<-as.data.frame(t(apply(Cat,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
  catchpst<-catchres[1:dim(catchpast)[2],]
  catchpst[] <-  t(catchpast)
  catchres  <-   rbind(catchpst,catchres)
  catchres  <-   cbind(yrrs,catchres)
  catchres$var <- "catch"
  catchres[catchres==0]<-NA
  
  
  yrrs   <- an(dimnames(landings(stk))$year)
  yrrs <- seq(min(yrrs),max(yrrs)+100,1)
  
  landpast <- as.data.frame(landings(stk)@.Data)
  landres<-as.data.frame(t(apply(Lan,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
  landpst<-landres[1:dim(landpast)[2],]
  landpst[] <-  t(landpast)
  landres  <-   rbind(landpst,landres)
  landres  <-   cbind(yrrs,landres)
  landres$var <- "landings"
  landres[landres==0]<-NA
  
  yrrs   <- an(dimnames(fbar(stk))$year)
  yrrs <- seq(min(yrrs),max(yrrs)+100,1)
  
  Fbarpast <- as.data.frame(fbar(stk)@.Data)
  Fbarres<-as.data.frame(t(apply(Freal,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
  Fbarpst<-Fbarres[1:dim(Fbarpast)[2],]
  Fbarpst[] <-  t(Fbarpast)
  Fbarres  <-   rbind(Fbarpst,Fbarres)
  Fbarres  <-   cbind(yrrs,Fbarres)
  Fbarres$var <- "Fbar"
  Fbarres[Fbarres==0]<-NA
  
  #Store individual run results
  
  simStks[[ac(Bzero)]] <- list()
  simStks[[ac(Bzero)]][["N"]]       <- Ny
  simStks[[ac(Bzero)]][["F"]]       <- Fy
  simStks[[ac(Bzero)]][["catW"]]    <- Wy
  simStks[[ac(Bzero)]][["lanW"]]    <- Wl
  simStks[[ac(Bzero)]][["stkW"]]    <- west
  simStks[[ac(Bzero)]][["stkWran"]] <- rsam
  simStks[[ac(Bzero)]][["C"]]       <- Cy
  simStks[[ac(Bzero)]][["L"]]       <- Cy * Ry
  simStks[[ac(Bzero)]][["percSSB"]] <- ssbpast
  simStks[[ac(Bzero)]][["intendF"]] <- Freal
  
  # plot(simStks[[ac(Bzero)]][["intendF"]])
  
  res <- rbind(ssbres,rbind(catchres,rbind(landres,rbind(Fbarres,rskres))))
  names(res) <- c("years","Q.05","Q.25","Q.50","Q.75","Q.95","variable")
  
  
  return(list(sim=res,iavTAC = iavres, simStks = simStks) )
  
}



traject.plot <- function(res)
{
x11()
g <- ggplot(res , aes(years,Q.50))
g <- g + geom_ribbon(aes(ymin = Q.05 , ymax = Q.95 , fill = variable),alpha=0.3)
g <- g + geom_ribbon(aes(ymin = Q.25 , ymax = Q.75 , fill = variable),alpha=0.5)
g <- g + facet_grid (variable~., scales ="free")
g <- g + geom_line(aes(years,Q.50))
print(g)
}

traject.plot.mgt <- function(res)
{
x11()
g <- ggplot(res , aes(years,Q.50))
g <- g + geom_ribbon(aes(ymin = Q.05 , ymax = Q.95 , fill = variable),alpha=0.3)
g <- g + geom_ribbon(aes(ymin = Q.25 , ymax = Q.75 , fill = variable),alpha=0.5)
g <- g + facet_grid (variable~mgt, scales ="free")
g <- g + geom_line(aes(years,Q.50))
print(g)
}




diagnostics <- function(res,iavres)   
{
m1 <- aggregate(Q.50~mgt , FUN = mean ,data = res [res$variable == "landings" & res$year >2080 , ])
names(m1)[2] <- "value"
m1$variable <- "mean long-term landings"

m2 <- aggregate(Q.50~mgt , FUN = mean ,data = res [res$variable == "Fbar" & res$year >2080 , ])
names(m2)[2] <- "value"
m2$variable <- "mean long-term Fbar"

m3 <- aggregate(Q.50~mgt , FUN = mean ,data = res [res$variable == "ssb" & res$year >2080 , ])
names(m3)[2] <- "value"
m3$variable <- "mean long-term SSB"

m4 <- aggregate(Q.50~mgt , FUN = max ,data = res [res$variable == "risk" & res$year >2020 , ])
names(m4)[2] <- "value"
m4$variable <- "risk type 3"

m5 <-   lapply(iavres,function(x) mean(apply(abs(x),2,mean)))
m5 <- data.frame(unlist(m5))
m5$mgt <- rownames(m5)
m5$variable <- "TACiav"
names(m5)[1] <-"value"
m5 <- m5[,names(m3)]

m<-do.call("rbind", list(m1,m2,m3,m4,m5))

x11()
g <- ggplot(m,aes(x=mgt,y=value,fill=variable))
g <- g + geom_bar(position=position_dodge(), stat="identity")
g <- g + facet_grid (variable~mgt, scales ="free")
print(g)

return(m)
} 