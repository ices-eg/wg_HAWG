
noHERAS.tun <- wbss.tun[-1]
noHERAS.ctrl<- drop.from.control(wbss.ctrl,fleet="HERAS")
path        <- "D:/Downloads/noHERAS/"
params2pin(wbss.sam,save.dir=path,
                    drop.param.names=as.list(c("logFpar","logSdLogObs")),
                    drop.params.index=list(na.omit(unique(wbss.ctrl@catchabilities["HERAS",])),
                                           na.omit(unique(wbss.ctrl@obs.vars["HERAS",]))))
FLR2SAM(wbss,noHERAS.tun,noHERAS.ctrl,run.dir=path)
runSAM(noHERAS.ctrl,run.dir=path,use.pin=T)
noHERAS.sam <- SAM2FLR(noHERAS.ctrl,run.dir=path)


noGERAS.tun <- wbss.tun[-2]
noGERAS.ctrl<- drop.from.control(wbss.ctrl,fleet="GerAS")
path        <- "D:/Downloads/noGERAS/"
params2pin(wbss.sam,save.dir=path,
                    drop.param.names=as.list(c("logFpar","logSdLogObs")),
                    drop.params.index=list(na.omit(unique(wbss.ctrl@catchabilities["GerAS",])),
                                           na.omit(unique(wbss.ctrl@obs.vars["GerAS",]))))
FLR2SAM(wbss,noGERAS.tun,noGERAS.ctrl,run.dir=path)
runSAM(noGERAS.ctrl,run.dir=path,use.pin=T)
noGERAS.sam <- SAM2FLR(noGERAS.ctrl,run.dir=path)

noN20.tun <- wbss.tun[-3]
noN20.ctrl<- drop.from.control(wbss.ctrl,fleet="N20")
path        <- "D:/Downloads/noN20/"
params2pin(wbss.sam,save.dir=path,
                    drop.param.names=as.list(c("logFpar","logSdLogObs")),
                    drop.params.index=list(na.omit(unique(wbss.ctrl@catchabilities["N20",])),
                                           na.omit(unique(wbss.ctrl@obs.vars["N20",]))))
FLR2SAM(wbss,noN20.tun,noN20.ctrl,run.dir=path)
runSAM(noN20.ctrl,run.dir=path,use.pin=T)
noN20.sam <- SAM2FLR(noN20.ctrl,run.dir=path)

noIBTSQ1.tun <- wbss.tun[-4]
noIBTSQ1.ctrl<- drop.from.control(wbss.ctrl,fleet="IBTS Q1")
path        <- "D:/Downloads/noIBTSQ1/"
params2pin(wbss.sam,save.dir=path,
                    drop.param.names=as.list(c("logFpar","logSdLogObs")),
                    drop.params.index=list(na.omit(unique(wbss.ctrl@catchabilities["IBTS Q1",])),
                                           na.omit(unique(wbss.ctrl@obs.vars["IBTS Q1",]))))
FLR2SAM(wbss,noIBTSQ1.tun,noIBTSQ1.ctrl,run.dir=path)
runSAM(noIBTSQ1.ctrl,run.dir=path,use.pin=T)
noIBTSQ1.sam <- SAM2FLR(noIBTSQ1.ctrl,run.dir=path)

noIBTSQ3.tun <- wbss.tun[-5]
noIBTSQ3.ctrl<- drop.from.control(wbss.ctrl,fleet="IBTS Q3")
path        <- "D:/Downloads/noIBTSQ3/"
params2pin(wbss.sam,save.dir=path,
                    drop.param.names=as.list(c("logFpar","logSdLogObs")),
                    drop.params.index=list(na.omit(unique(wbss.ctrl@catchabilities["IBTS Q3",])),
                                           na.omit(unique(wbss.ctrl@obs.vars["IBTS Q3",]))))
FLR2SAM(wbss,noIBTSQ3.tun,noIBTSQ3.ctrl,run.dir=path)
runSAM(noIBTSQ3.ctrl,run.dir=path,use.pin=T)
noIBTSQ3.sam <- SAM2FLR(noIBTSQ3.ctrl,run.dir=path)

res <- FLSAMs(noHERAS=noHERAS.sam,noGerAS=noGERAS.sam,noN20=noN20.sam,noIBTSQ1=noIBTSQ1.sam,noIBTSQ3=noIBTSQ3.sam)
plot(res)


params2pin <- function(object,start=NULL,end=NULL,save.dir=tempdir(),drop.param.names=NULL,drop.params.index=NULL){

  #---------------------------------------------------
  # Book keeping
  #---------------------------------------------------
  if(!is(object,"FLSAM")) stop("Supplied object must be an FLSAM")
  run.time  <- Sys.time()
  strt      <- ifelse(is.null(start)==T,range(object)["minyear"],start)
  nd        <- ifelse(is.null(end)==T  ,range(object)["maxyear"],end)

  #---------------------------------------------------
  # Truncate data if necessary
  #---------------------------------------------------

  #-Extract stock.n and harvest values from SAM object
  n         <- log(window(object@stock.n,strt,nd))
  f         <- log(window(object@harvest[!duplicated(object@control@states[names(which(object@control@fleets==0)),]),],strt,nd))

  #-Define how many parameters are estimated
  n.states  <- length(unique(object@control@states[names(which(object@control@fleets==0)),]))
  ages      <- dims(object)$age

  idxNs     <- c(mapply(seq,from=seq(1,length(n)+length(f),ages+n.states),
                            to  =seq(1,length(n)+length(f),ages+n.states)+ages-1,
                            by  =1))
  idxFs     <- c(mapply(seq,from=seq(1,length(n)+length(f),ages+n.states)+ages,
                            to  =seq(1,length(n)+length(f),ages+n.states)+n.states+ages-1,
                            by  =1))

  #-Create vector with combined n and f values
  u         <- numeric(length(n)+length(f))
  u[idxNs]  <- n
  u[idxFs]  <- f
  U         <- data.frame(name="U",value=u,std.dev=NA)

  #-Merge U's with other parameters (even if truncated)
  if(any(is.na(object@control@power.law.exps)==F) & object@control@scaleNoYears==0){
    par.list<- c("logFpar","logQpow","logSdLogFsta","logSdLogN","logSdLogObs","rec_loga",
                  "rec_logb","rho","logScaleSSB","logPowSSB","logSdSSB","U")
  } else {
    if(object@control@scaleNoYears!=0 & any(is.na(object@control@power.law.exps)==T)){
      par.list <- c("logFpar","logSdLogFsta","logSdLogN","logSdLogObs","rec_loga",
                    "rec_logb","rho","logScale","logScaleSSB","logPowSSB","logSdSSB","U")
    } else {
      par.list <- c("logFpar","logSdLogFsta","logSdLogN","logSdLogObs","rec_loga",
                    "rec_logb","rho","logScaleSSB","logPowSSB","logSdSSB","U")
      }
    }

  pars      <- object@params
  uidx      <- which(pars$name == "U")
  parsTop   <- pars[1:(uidx[1]-1),]
  pars      <- rbind(parsTop,U,if(nrow(pars)>max(uidx)){pars[(rev(uidx)[1]+1):nrow(pars),]})
  if(is.null(drop.params.names)==F){
    for(i in 1:length(drop.param.names))
      pars    <- pars[-which(pars$name == drop.param.names[[i]])[drop.params.index[[i]]],]
  }

  #---------------------------------------------------
  # Create pin file
  #---------------------------------------------------
  if(identical(object@control@sam.binary,character(0))){
    pin.file  <- file.path(save.dir,"sam.pin")
  } else {
    pin.file  <- file.path(save.dir,paste(tolower(strsplit(rev(strsplit(object@control@sam.binary,"/")[[1]])[1],".exe")[[1]]),".pin",sep=""))
  }
  unlink(pin.file)    #Get rid of anything that is already there
  for(iPar in par.list){
    #Get pars to write
    par.vals <- pars[pars$name==iPar,"value"]
    if(length(par.vals)==0) par.vals <- 0
    #Write to file
    cat("#",iPar,": \n ",file=pin.file,append=TRUE,sep="")
    cat(par.vals," \n ",file=pin.file,append=TRUE)
  }
invisible(NULL)}

