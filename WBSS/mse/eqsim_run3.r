# ---------------------------------------------------------------------------------------
# EQSIM_RUN3
# new eqsim with HCR also applicable below Blim
# 05/07/2018 Martin Pastoors first coding
# 13/07/2018 Updated coding after talks with David Miller (i.e. Blue whiting examples)
# ---------------------------------------------------------------------------------------

fit       = FIT_SRRx
bio.years = c(2012, 2016)
bio.const = FALSE
sel.years = c(2012, 2016)
sel.const = FALSE
Ftarget   = 0.31
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
Nyear     = 25
Nits      = 20
Nworm     = 10
process.error = FALSE
random.vectors= FALSE
verbose = TRUE
recruitment.trim = c(3, -3)


# eqsim_run3 <-   function (fit, 
#                           bio.years = c(2008, 2012), 
#                           bio.const = FALSE,
#                           sel.years = c(2008, 2012), 
#                           sel.const = FALSE, 
#                           Fscan     = 0.2, 
#                           Fcv       = 0, 
#                           Fphi      = 0, 
#                           SSBcv     = 0, 
#                           rhologRec = FALSE,
#                           Blim, 
#                           Bpa, 
#                           recruitment.trim = c(3, -3), 
#                           Ftarget   = 0.2 , 
#                           Btrigger  = 0, 
#                           Blim2     = 0, 
#                           Nyear      = 200, 
#                           Nits      = 50 ,
#                           Nworm     = 10,
#                           process.error = TRUE, 
#                           verbose = TRUE, 
#                           extreme.trim)
#   
# {

  if (abs(Fphi) >= 1)
    stop("Fphi, the autocorelation parameter for log F should be between (-1, 1)")
  if ((recruitment.trim[1] + recruitment.trim[2]) > 0)
    stop("recruitment truncation must be between a high - low range")
  
  btyr1 <- bio.years[1]
  btyr2 <- bio.years[2]
  slyr1 <- sel.years[1]
  slyr2 <- sel.years[2]
  
  keep <- Nyear
  
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
  
  land.cat = landings/catch
  i <- is.na(land.cat)
  if (any(i))
    land.cat[i] <- 1
  Fprop <- apply(FLCore::harvest.spwn(stk.winsel), 1, mean)[drop = TRUE]
  Mprop <- apply(FLCore::m.spwn(stk.win), 1, mean)[drop = TRUE]
  Nmod <- nrow(SR)
  ages <- FLCore::dims(stk)$age
  ssby <- Freal <- Ferr <- array(0, c(Nyear, Nmod), dimnames = list(year = 1:Nyear,
                                                                   iter = 1:Nmod))
  Ny <- Fy <- WSy <- WCy <- Cy <- Wy <- Wl <- Ry <- array(0,
                                                          c(ages, Nyear, Nmod), dimnames = list(age = (range(stk)[1]:range(stk)[2]),
                                                                                               year = 1:Nyear, iter = 1:Nmod))
  
  # F errors
  Ferr[1, ] <- stats::rnorm(n = Nmod, mean = 0, sd = 1) * Fcv/sqrt(1 - Fphi^2)
  for (j in 2:Nyear) {
    Ferr[j, ] <- Fphi * Ferr[j - 1, ] + Fcv * stats::rnorm(n = Nmod, mean = 0, sd = 1)
  }
  
  # SSB errors
  SSBerr <- matrix(stats::rnorm(n = Nyear * Nmod, mean = 0, sd = 1), ncol = Nmod) * SSBcv
  
  # random vectors for weight
  rsam <- array(sample(1:ncol(weca), Nyear * Nmod, TRUE), c(Nyear, Nmod))

  # random vectors for selectivity
  rsamsel <- array(sample(1:ncol(sel), Nyear * Nmod, TRUE), c(Nyear, Nmod))
  
  # Weights
  Wy[] <- c(weca[, c(rsam)])
  Wl[] <- c(wela[, c(rsam)])
  Ry[] <- c(land.cat[, c(rsamsel)])
  R    <- mean(data$rec)
  
  # Number of scans over Blim2
  # NF   <- 1
  NF   <- length(Blim2.scan)
    
  ssbs <- cats <- lans <- recs <- array(0, c(7, NF))
  ferr <- ssbsa <- catsa <- lansa <- recsa <- array(0, c(NF, keep, Nmod))
  begin <- Nyear - keep + 1
  resids = array(stats::rnorm(Nmod * (Nyear + 1), 0, SR$cv), c(Nmod, Nyear + 1))
  
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
    for (j in 2:(Nyear + 1)) {
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
  
  
  # Create dataframe for storing loop information 
  loop.df <- data.frame()
  
  
  
  # ----- SCANS ------------------------------------------------------
  
  # initialize empty data.frames
  Cwy.df <- Fbary.df <- Fy.df <- Ny.df <- Wy.df <- ssby.df <- data.frame()
  
  # Loop over NF (number of scans for Blim2)
  # i <- 1
  for (i in 1:NF) {
    
    Blim2 <- Blim2.scan[i]
    
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
    
    # j <- 2
    # Nyear is the number of years to run the simulation
    for (j in 2:Nyear) {
      
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
      # This is where the F for next year is determined; i.e. the HCR
      
      # standard MSY rule: 
      # Fnext <- Fbar * pmin(1, SSB * exp(SSBerr[j, ])/Btrigger)
      
      # Tester
      # SSBs    <- seq(from=0, to=50000, length.out = 500)
      
      SSBs    <- SSB * exp(SSBerr[j-1, ]) # get perceived SSBs from year before
      FatBlim <- Ftarget * (Blim/Btrigger) # calculate F at Blim according to MSY rule
      Fnext   <- Ftarget * pmin(1, SSBs * exp(SSBerr[j-1,])) #initiate Fnext object
      
      # Temporary vectors above and below Blim2, Blim and Bpa
      tmpL_Blim2    <- c(SSBs <  Blim2)
      tmpG_Blim2    <- c(SSBs >= Blim2)
      tmpL_Blim     <- c(SSBs <  Blim); 
      tmpG_Blim     <- c(SSBs >= Blim); 
      tmpL_Btrigger <- c(SSBs <  Btrigger)
      tmpG_Btrigger <- c(SSBs >= Btrigger)
      
      # above Btrigger
      Fnext[tmpG_Btrigger] <- Ftarget
      
      # between Blim and Btrigger
      Fnext[tmpG_Blim & tmpL_Btrigger] <- 
        Ftarget * (SSBs[tmpG_Blim & tmpL_Btrigger]/Btrigger)
      
      # between Blim2 and Blim
      Fnext[tmpG_Blim2 & tmpL_Blim] <- 
        FatBlim * (1 - (Blim - SSBs[tmpG_Blim2 & tmpL_Blim]) / (Blim - Blim2))
      
      # below Blim2
      Fnext[tmpL_Blim2] <- 0
      
      # plot(SSBs, Fnext)
      
      # ======================================================================
      
      # Is this right? Apply the F error after going through the HCR??
      Fnext         <- exp(Ferr[j, ]) * Fnext
      
      Freal[j, ]    <- Fnext
      Zpre          <- rep(Fnext, 
                           each = length(Fprop)) * Fprop * sel[, rsamsel[j, ]] + M[, rsam[j, ]] * Mprop
      Fy[, j - 1, ] <- rep(Fnext, 
                           each = ages) * sel[, rsamsel[j - 1, ]]
      Ny[-1, j, ]   <- Ny[1:(ages - 1), j - 1, ] * 
        exp(-Fy[1:(ages - 1), j - 1, ] - M[1:(ages - 1), rsam[j - 1, ]])
      Ny[ages, j, ] <- Ny[ages,j,] + 
        Ny[ages,j-1,] * exp(-Fy[ages, j-1, ] - M[ages, rsam[j-1, ]])
      ssby[j, ]     <- apply(array(Mat[, rsam[j, ]] * 
                                     Ny[, j, ] * 
                                     west[, rsam[j, ]]/exp(Zpre), c(ages, Nmod)), 2, sum)
      Cy[, j, ]     <- Ny[, j-1, ] * Fy[, j-1, ]/
        (Fy[,j-1, ] + M[, rsam[j-1, ]]) * (1 - exp(-Fy[,j-1, ] - M[,rsam[j-1, ]]))
      
      loop.df <-
        bind_rows(loop.df, 
                  data.frame(year = j + max(an(dimnames(ssb(stk))$year)) - 1,
                             iter = names(tmpG_Blim), 
                             SSBs=ssby[j,] * exp(SSBerr[j, ]), Fnext, Ferr = Ferr[j,], SSBerr=SSBerr[j,], 
                             tmpL_Blim2, tmpG_Blim2, tmpL_Blim, tmpG_Blim, tmpL_Btrigger, tmpG_Btrigger, 
                             Blim2, Blim, Btrigger, Ftarget, FatBlim) )
      
      
    } # end of run over j (years) =========================
    
    # Create worm data.frames for storing results
    Wy.df <- 
      as.data.frame(t(as.data.frame(Wy))) %>%
      setNames(paste0("age", unlist(dimnames(Wy)[1]))) %>% 
      rownames_to_column(., "rowname") %>% 
      separate(rowname, into=c("year","iter"), sep="\\.") %>% 
      mutate(
        variable = "Wy",
        year     = as.numeric(year) +   max(an(dimnames(ssb(stk))$year)) - 1,
        iter     = as.numeric(iter), 
        Blim2    = Blim2
      ) %>% 
      gather(key="age", value="value", 
             colnames(.)[3:((length(colnames(.)))-1)]) %>% 
      mutate(
        age      = gsub("age", "", age)
      ) %>% 
      bind_rows(Wy.df, .)
    
    Ny.df <- 
      as.data.frame(t(as.data.frame(Ny))) %>%
      setNames(paste0("age", unlist(dimnames(Ny)[1]))) %>% 
      rownames_to_column(., "rowname") %>% 
      separate(rowname, into=c("year","iter"), sep="\\.") %>% 
      mutate(
        variable = "Ny",
        year     = as.numeric(year) +   max(an(dimnames(ssb(stk))$year)) - 1,
        iter     = as.numeric(iter), 
        Blim2    = Blim2
      ) %>% 
      gather(key="age", value="value", 
             colnames(.)[3:((length(colnames(.)))-1)]) %>% 
      mutate(
        age      = gsub("age", "", age)
      ) %>% 
      bind_rows(Ny.df, .)
    
    Cwy.df <- 
      as.data.frame(t(as.data.frame(Cy * Wy))) %>% 
      setNames(paste0("age", unlist(dimnames(Cy)[1]))) %>% 
      rownames_to_column(., "rowname") %>% 
      separate(rowname, into=c("year","iter"), sep="\\.") %>% 
      mutate(
        variable = "Cwy",
        year     = as.numeric(year) +   max(an(dimnames(ssb(stk))$year)) - 1,
        iter     = as.numeric(iter), 
        Blim2    = Blim2
      ) %>% 
      gather(key="age", value="value", 
             colnames(.)[3:((length(colnames(.)))-2)]) %>% 
      group_by(year, iter, Blim2, variable) %>% 
      dplyr::summarize(value = sum(value)) %>% 
      bind_rows(Cwy.df, .)
    
    Fy.df <- 
      as.data.frame(t(as.data.frame(Fy))) %>%
      setNames(paste0("age", unlist(dimnames(Fy)[1]))) %>% 
      rownames_to_column(., "rowname") %>% 
      separate(rowname, into=c("year","iter"), sep="\\.") %>% 
      mutate(
        variable = "Fy",
        year     = as.numeric(year) +   max(an(dimnames(ssb(stk))$year)) - 1,
        iter     = as.numeric(iter), 
        Blim2    = Blim2
      ) %>% 
      gather(key="age", value="value", 
             colnames(.)[3:((length(colnames(.)))-1)]) %>% 
      mutate(
        age      = gsub("age", "", age),
        value    = an(value)
      ) %>% 
      bind_rows(Fy.df, .)
    
    Fbary.df <-
      Fy.df %>% 
      filter(age %in% an(stk@range["minfbar"]): an(stk@range["maxfbar"])) %>% 
      group_by(iter, year, Blim2) %>% 
      dplyr::summarize(value = mean(value, na.rm=TRUE)) %>% 
      mutate(variable = "fbary") %>% 
      bind_rows(Fbary.df, .)
    
    ssby.df <- 
      as.data.frame(t(as.data.frame(ssby))) %>%
      setNames(paste0("year", unlist(dimnames(ssby)[1]))) %>% 
      rownames_to_column(., "iter") %>% 
      gather(key="year", value="value", 
             colnames(.)[2:(length(colnames(.)))]) %>% 
      mutate(
        variable = "ssby",
        age      = "all",
        year     = an(gsub("year", "", year)) + max(an(dimnames(ssb(stk))$year)) - 1, 
        Blim2    = Blim2
      ) %>% 
      mutate_at(c("iter","year"), funs(as.integer)) %>% 
      bind_rows(ssby.df, .)

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

  
  
  # plot SSB over different values of Blim2
  ssbs.df %>%
    ggplot(aes(year, value, group=iter)) +
    geom_line(aes(colour=factor(iter))) +
    geom_ribbon(data=ssbs2.df, aes(x=year, ymin=lower.ci, ymax=upper.ci), fill="red", alpha=0.3, inherit.aes = FALSE) +
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
  Fbary.df %>%
    filter(year >= 2017) %>% 
    mutate(Blim2 = as.integer(Blim2)) %>% 
    ggplot(aes(year, value, group=iter)) +
    geom_line(aes(colour=factor(iter))) +
    facet_wrap(~Blim2)  +
    expand_limits(y=0) +
    labs(title="F")
  
  bind_rows(ssby.df, ssbs.df) %>% 
    ggplot(aes(year, value, group=variable)) +
    geom_line(aes(colour=variable)) +
    geom_hline(aes(yintercept = Blim), colour="black", linetype ="dotted") +
    geom_hline(aes(yintercept = Btrigger), colour="black", linetype ="dashed") +
    facet_wrap(~iter)
  
  bind_rows(Fbary.df, Fs.df) %>%
    ggplot(aes(year, value, group=variable)) +
    geom_line(aes(colour=variable)) +
    geom_hline(aes(yintercept = Ftarget), colour="black", linetype ="dashed") +
    facet_wrap(~iter)
  
  # plot below Blim and Below Btrigger
  select(loop.df, year, iter, SSBs, tmpL_Blim2, tmpL_Blim, tmpL_Btrigger) %>% 
    mutate(
      tmpL_Blim2    = ifelse(tmpL_Blim2   ==TRUE, 1  , 0  ),
      tmpL_Blim     = ifelse(tmpL_Blim    ==TRUE, 1.1, 0.1),
      tmpL_Btrigger = ifelse(tmpL_Btrigger==TRUE, 1.2, 0.2)
    ) %>% 
    ggplot(aes(year, tmpL_Blim2)) +
    geom_line(colour="blue") +
    geom_line(aes(y=tmpL_Blim), colour="red") +
    geom_line(aes(y=tmpL_Btrigger), colour="green") +
    facet_wrap(~iter)
  

  # Cw     <- Cy * Wy
  # land   <- Cy * Ry * Wl
  # Lan    <- apply(land, 2:3, sum)
  # Cat    <- apply(Cw, 2:3, sum)
  # quants <- c( 0.05, 0.25, 0.5, 0.75, 0.95)
  # 
  # yrrs   <- an(dimnames(ssb(stk))$year)
  # # yrrs   <- seq(min(yrrs),max(yrrs)+100,1)
  # yrrs   <- seq(min(yrrs),max(yrrs)+Nyear,1)
  # 
  # ssbpast    <- as.data.frame(ssb(stk)@.Data)
  # ssbres     <- as.data.frame(t(apply(ssby,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
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
  # rskres<-rowSums (data.frame(ssby<Blim)) /  rowSums (data.frame(!is.na(ssby))) 
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
  # Fbarres<-as.data.frame(t(apply(Freal,1,function (x) { stats::quantile(x, c( 0.05, 0.25, 0.5, 0.75, 0.95))})))
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
  # simStks[[ac(Blim2)]][["N"]]       <- Ny
  # simStks[[ac(Blim2)]][["F"]]       <- Fy
  # simStks[[ac(Blim2)]][["catW"]]    <- Wy
  # simStks[[ac(Blim2)]][["lanW"]]    <- Wl
  # simStks[[ac(Blim2)]][["stkW"]]    <- west
  # simStks[[ac(Blim2)]][["stkWran"]] <- rsam
  # simStks[[ac(Blim2)]][["C"]]       <- Cy
  # simStks[[ac(Blim2)]][["L"]]       <- Cy * Ry
  # simStks[[ac(Blim2)]][["percSSB"]] <- ssbpast
  # simStks[[ac(Blim2)]][["intendF"]] <- Freal
  # 
  # # plot(simStks[[ac(Blim2)]][["intendF"]])
  # as.data.frame(Fy)
  # res <- rbind(ssbres,rbind(catchres,rbind(landres,rbind(Fbarres,rskres))))
  # names(res) <- c("years","Q.05","Q.25","Q.50","Q.75","Q.95","variable")
  # 
  # 
  # return(list(sim=res,iavTAC = iavres, simStks = simStks) )
  
# }



