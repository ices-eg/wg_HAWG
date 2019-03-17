#Calculate 7 year Mohn's Rho for a retro FLStocks

span <- 7
ref.year <- 2015
retro.fbar  <- lapply(retro,fbar)
if(ref.year>max(an(names(retro)),na.rm=T)) stop("Reference year set higher than the latest assessment year")
if(span >= length(retro)){
  span <- length(retro)-1
  warning("Span longer than number of retro years available")
}
retro.fbar  <- retro.fbar[which(names(retro.fbar) %in% ref.year:(ref.year-span))]
rho         <- data.frame(rho=NA, year=c((ref.year-span):ref.year))
retro.fbar  <- lapply(retro.fbar, as.data.frame)
termFs      <- do.call(rbind,lapply(as.list(names(retro.fbar)),
                                    function(x){return(retro.fbar[[ac(x)]][which(retro.fbar[[ac(x)]]$year==an(x)),])}))
refFs       <- retro.fbar[[ac(ref.year)]]
refFs       <- refFs[which(refFs$year %in% termFs$year),]
colnames(refFs) <- c("a","year","b","c","d","e","refvalue")
combFs      <- merge(refFs,termFs,by="year")
combFs      <- combFs[order(combFs$year),]
rhos        <- (1-combFs$data/combFs$refvalue)*100
rho$rho     <- rhos