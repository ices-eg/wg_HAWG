# modified from msy::eqsr_fit to allow shifting in S-R time series

eqsr_fit_shift <- 
function (stk, nsamp = 5000, models = c("ricker", "segreg", "bevholt"), 
    method = "Buckland", id.sr = NULL, remove.years = NULL, delta = 1.3, 
    nburn = 10000, rshift = 0) 
{
    dms <- FLCore::dims(stk)
    rage <- dms$min
    if (rage == 0) {
        x = FLCore::stock.n(stk)[1, drop = TRUE]
    }
    else {
        x = c(FLCore::stock.n(stk)[1, -seq(rage), drop = TRUE], 
            rep(NA, rage))
    }
    if (rshift > 0){
        x = c(FLCore::stock.n(stk)[1, -seq(rshift), drop = TRUE], 
            rep(NA, rshift))
       
    } else { NULL }        
    rby <- data.frame(year = with(dms, minyear:maxyear), rec = x, 
        ssb = FLCore::ssb(stk)[drop = TRUE], fbar = FLCore::fbar(stk)[drop = TRUE], 
        landings = FLCore::landings(stk)[drop = TRUE], catch = FLCore::catch(stk)[drop = TRUE])
    # print(rby)
    row.names(rby) <- NULL
    rby <- rby[!is.na(rby$rec), ]
    data <- rby[, 1:3]
    if (!is.null(remove.years)) {
        data$ssb[data$year %in% remove.years] <- NA
    }
    data <- data[complete.cases(data), ]
    if (is.null(id.sr)) 
        id.sr <- FLCore::name(stk)
    method <- match.arg(method, c("Buckland", "Simmonds", "King", "Cadigan"))
    if (!is.character(models)) 
        stop("models arg should be character vector giving names of stock recruit models")
    if (method == "Buckland") {
        return(c(eqsr_Buckland(data, nsamp, models), list(stk = stk, 
            rby = rby, id.sr = id.sr)))
    }
    else {
        cat("The", method, "is not ready yet!  Working on it!\n")
    }
}
