# @param stk FLStock object
# @param nsamp Number of samples (iterations)
# @param models A character vector containing sr-models to use. User can set
# any combination of "ricker","segreg","bevholt".
# @param method A character vector. Currently only "Buckland" is implemented.
# @param id.sr A character vector specifying an id for the stock recruitment
# model. If not specified (default) the slot "name" in the FLStock is used.
# @param remove.years A vector specifying the years to remove
# @param delta A value, used in method "Simmonds" (not implemented)
# @param nburn An integer, used in method Simmonds (not implemented)
# @return A list containing the following objects:
# \itemize{
# \item fit data.frame containing the alpha (a), beta (b), cv and model names.
# The number of rows correspond to the value set in nsamp in the function call.
# \item pred A vector of predicted recruitment values. The length of the vector
# corresponds to the value set in nsamp in the function call.
# \item fits The parameters in the stock recruitment model corresponding to the
# "best fit" of any given model.
# \item data data.frame containing the recruitment (rec), spawning stock
# biomass (ssb) and year used in the fitting of the data.
# \item stknam A character vector containing stock name
# \item stk FLStock object, same as provided as input by the user.
# }
# @author Colin Millar \email{colin.millar@@jrc.ec.europa.eu}
# @export

### function that fits segmented regression with a breakpoint at Blim
#Blim<-33000
segreg3 <- function(ab, ssb) log(ifelse(ssb >= 33000, ab$a * 33000, ab$a * ssb))

#eqsr_fit <- function(stk, nsamp = 5000, models = c("ricker","segreg","bevholt"), 
#                     method = "Buckland",
#                     id.sr = NULL, remove.years = NULL, delta = 1.3, nburn = 10000) 
#{

  eqsr_fit <- function(stk, nsamp = 5000, models = c("segreg3"), 
                       method = "Buckland",
                       id.sr = NULL, remove.years = NULL, delta = 1.3, nburn = 10000) 
  {
    
  
  
  dms <- dims(stk)
  rage <- dms $ min
  if (rage == 0)
  { x = stock.n(stk)[1,drop=TRUE]
  } else {
    x = c(stock.n(stk)[1,-seq(rage),drop=TRUE],rep(NA,rage))
  }

  #rby <- data.frame(year = with(dms, minyear:maxyear),
  #                  rec = x,
  #                  ssb = ssb(stk)[drop=TRUE],
  #                  fbar = fbar(stk)[drop=TRUE],
  #                  landings=landings(stk)[drop=TRUE],
  #                  catch=catch(stk)[drop=TRUE])

  #2015 assessment
  rby <- data.frame(year = 1958:2012,
                    rec = c(364794,  392084,  837339,  405494, 1380210,  421391,  741245,  774993,  907207,  469158,  254830,  824154,
                            282493,  327066,  163005,  204165,  227463,  186562,  148045,  282152,  168531,  465638,  724151,  783337,  664944,  640184,
                            650742, 1193220,  474983,  574641,  503264,  208551,  958184,  359652,  766083,  717939,  351797,  370511,  247178,  473462,
                            459347,  482831,  441827,  140515,  360061, 1079510,  373947,  833003,  355107, 1322000, 1022150, 1357600,  948465,  530679,
                            1157860),
                    ssb = c(168371.89, 169048.91, 169040.79, 145389.23, 148090.45, 138616.13, 159325.75, 168973.41, 168580.59, 163933.23, 168963.82,
                            153468.77, 115538.64, 106438.21,  96450.04,  71804.65,  55605.86,  44007.30,  39465.74,  39656.27,  38540.51,  39417.49,
                            37495.05,  41860.76,  61264.15,  83907.90,  86580.09,  90330.15,  99703.86, 112289.09, 113690.58, 101256.26,  93525.20,
                            77581.95,  76304.08,  77975.33,  84757.92,  87474.09,  76756.81,  64828.40,  52111.41,  45868.25,  44854.29,  43497.81,
                            50212.84,  36492.48,  33776.85,  51413.88,  65865.60,  71656.40,  88984.24, 108808.80, 126328.79, 144370.79, 140019.90),
                    fbar = window(fbar(stk),1958,2012)[drop=TRUE],
                    landings=window(landings(stk),1958,2012)[drop=TRUE],
                    catch=window(catch(stk),1958,2012)[drop=TRUE])
  
  
  
  #2016 assessment - vectors pasted in to shift recruitment time series by two years
#   rby <- data.frame(year = 1958:2013,
#                     rec = c(364697,392024,837547,405668,1379970,421470,741379,775032,907348,469420,254953,824165,
#                             282498,327070,163084,204147,227419,186588,148132,282208,168544,465723,724061,783387,
#                             664825,640118,650690,1193310,475132,574803,503581,208741,958670,360045,766900,719177,
#                             352675,371770,248567,478120,468872,497692,457861,146665,383169,1169010,408108,917257,
#                             395932,1465500,1169110,1541530,1104530,588701,539982,366128),
#                     ssb = c(168268.83,169117.02,169075.90,145407.26,148111.88,138637.90,159338.39,168979.51,168597.02,
#                             163952.99,168961.61,153503.24,115556.46,106460.29,96454.78,71810.93,55614.95,44010.20,39468.06,
#                             39655.77,38539.81,39424.08,37501.90,41868.97,61269.77,83904.37,86564.38,90315.36,99688.99,
#                             112270.88,113676.36,101264.83,93547.88,77611.79,76344.92,78022.12,84838.61,87590.82,76897.78,
#                             65000.12,52326.52,46213.05,45520.35,44611.82,51969.56,38175.42,35897.03,55666.52,71951.98,79008.41,
#                             98796.66,121278.32,142058.66,164242.24,162163.99,155459.66),
#                     fbar = window(fbar(stk),1958,2013)[drop=TRUE],
#                     landings=window(landings(stk),1958,2013)[drop=TRUE],
#                     catch=window(catch(stk),1958,2013)[drop=TRUE])
  
  row.names(rby) <- NULL
  rby <- rby[!is.na(rby$rec),]
  # This is for stuff later down the pipes
  data <- rby[,1:3] 
  
  # EINAR: strange that here only the ssb is set to as NA
  #        question how this affect what happens further down the line
  if (!is.null(remove.years)) {
    data $ ssb[data $ year %in% remove.years] <- NA
  }
  #--------------------------------------------------------
  # tidy data - remove nas
  #--------------------------------------------------------
  data <- data[complete.cases(data),]
  
  if (is.null(id.sr)) id.sr <- name(stk)
  
  #method <- match.arg(method, c("Buckland","Simmonds","King","Cadigan"))
  #if (!is.character(models)) stop("models arg should be character vector giving names of stock recruit models")
  
  if (method == "Buckland") {
    #return(c(eqsr_Buckland(data, nsamp, models), list(stk = stk,rby=rby, id.sr=id.sr)))
    #return(c(eqsr_Buckland(data,nsamp=1000,models=c("Segreg")), list(stk = stk,rby=rby, id.sr=id.sr)))
    return(c(eqsr_Buckland(data,nsamp=1000,models=c("segreg3")), list(stk = stk,rby=rby, id.sr=id.sr)))
  } else
  {
    cat("The", method, "is not ready yet!  Working on it!\n")
  }
}
