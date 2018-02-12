retroStckNSAS <- function(stock,indices,retroTun,control,retro){
    stck.min.yr <- stock@range["minyear"]
    stck.max.yr <- stock@range["maxyear"]
    year.range <- (stck.max.yr-retro):stck.max.yr
    # Calculate hangover years - but only consider positive hangovers
    hangover.yrs <- sapply(indices,function(x) dims(x)$maxyear) - dims(stock)$maxyear
    hangover.yrs <- ifelse(hangover.yrs < 0,0,hangover.yrs)
    # Check year.range is sensible
    if(min(year.range) < stck.min.yr || max(year.range) > stck.max.yr)
      stop("Year range outside stock object range")
    # ---------- Run that retrospective -------------
    cat("Running retrospective...\n")
    res <- new("FLSAMs")
    for (yr in rev(year.range)){  #yr is the year in which the assessment is being simulated
      Stock <- trim(stock, year=stck.min.yr:yr)
      Indices.temp<-indices
      for (j in 1:length(indices)) {
        idx.min.yr <- dims(indices[[j]])$minyear
        if (yr < idx.min.yr) {stop(paste("Year requested (",yr,
              ") is earlier than the first year (",
              idx.min.yr,") of the ",indices[[j]]@name," index.",sep="")) }
        idx.max.yr <- min(dims(indices[[j]])$maxyear, yr+hangover.yrs[j])
        if(!names(indices)[j] %in% names(retroTuns)){
          Indices.temp[[j]] <- trim(indices[[j]],year=idx.min.yr:idx.max.yr)
        } else {
          Indices.temp[[j]] <- trim(indices[[j]],year=idx.min.yr:idx.max.yr)
          Indices.temp[[j]]@index[] <- t(subset(retroTuns[[names(indices)[j]]],iround==yr)[,grep("age",colnames(retroTuns[[names(indices)[j]]]))])
        }
      }
      control@name <- as.character(yr)
      control@range["maxyear"] <- max(Stock@range["maxyear"],
                max(sapply(Indices.temp,function(x) max(x@range[c("maxyear")]))))
      assess  <- try(FLSAM(Stock, Indices.temp,control))
      if(class(assess)=="try-error") {
        warning(sprintf("Retrospective for year %i failed\n",yr))
      } else {
        assess@desc    <-  paste(as.character(yr), "Retrospective")
        res[[as.character(yr)]] <- assess
      }
    }
    res@desc   <- paste("Retrospective analysis from object", stock@desc)
    return(res)
  }


retroStcksNSAS <- function(stock,indices,retroTuns,control,retro){
    stck.min.yr <- min(unlist(lapply(stock,function(x){return(x@range["minyear"])})))
    stck.max.yr <- max(unlist(lapply(stock,function(x){return(x@range["maxyear"])})))
    stcks.range <- lapply(stock,function(x){return(x@range[c("minyear","maxyear")])})
    trim.stock  <- which.max(do.call(rbind,stcks.range)[,"maxyear"])

    year.range <- (stck.max.yr-retro):stck.max.yr
    if(min(year.range)<= stcks.range[[trim.stock]]["minyear"])
      stop("Running retrospective beyond combination of sum and residual fleets")
    # Calculate hangover years - but only consider positive hangovers
    hangover.yrs <- sapply(indices,function(x) dims(x)$maxyear) - stck.max.yr
    hangover.yrs <- ifelse(hangover.yrs < 0,0,hangover.yrs)
    # Check year.range is sensible
    if(min(year.range) < stck.min.yr || max(year.range) > stck.max.yr)
      stop("Year range outside stock object range")
    # ---------- Run that retrospective -------------
    cat("Running retrospective...\n")
    res <- new("FLSAMs")
    for (yr in rev(year.range)){  #yr is the year in which the assessment is being simulated
      Stock <- new("FLStocks")
      for(iStk in 1:length(stock)){
        if(iStk == trim.stock)
          Stock[[iStk]] <- trim(stock[[trim.stock]], year=stcks.range[[trim.stock]]["minyear"]:yr)
        if(iStk != trim.stock)
          Stock[[iStk]] <- stock[[iStk]]
      }
      names(Stock) <- names(stock)
      Indices.temp<-indices
      for (j in 1:length(indices)) {
        idx.min.yr <- dims(indices[[j]])$minyear
        if (yr < idx.min.yr) {stop(paste("Year requested (",yr,
              ") is earlier than the first year (",
              idx.min.yr,") of the ",indices[[j]]@name," index.",sep="")) }
        idx.max.yr <- min(dims(indices[[j]])$maxyear, yr+hangover.yrs[j])
        if(!names(indices)[j] %in% names(retroTuns)){
          Indices.temp[[j]] <- trim(indices[[j]],year=idx.min.yr:idx.max.yr)
        } else {
          Indices.temp[[j]] <- trim(indices[[j]],year=idx.min.yr:idx.max.yr)
          Indices.temp[[j]]@index[] <- t(subset(retroTuns[[names(indices)[j]]],iround==yr)[,grep("age",colnames(retroTuns[[names(indices)[j]]]))])
        }
      }
      control@name <- as.character(yr)
      control@range["maxyear"] <- max(max(unlist(lapply(Stock,function(x){return(x@range["maxyear"])}))),
                max(sapply(Indices.temp,function(x) max(x@range[c("maxyear")]))))
      assess  <- try(FLSAM(Stock, Indices.temp,control))
      if(class(assess)=="try-error") {
        warning(sprintf("Retrospective for year %i failed\n",yr))
      } else {
        assess@desc    <-  paste(as.character(yr), "Retrospective")
        res[[as.character(yr)]] <- assess
      }
    }
    res@desc   <- paste("Retrospective analysis from object", stock@desc)
    return(res)
  }