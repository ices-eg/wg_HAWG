################################################################################
# Code to do the short term forecast exploration for North Sea herring
# using R 3.4.3
# Last updated 14/02/2018
#
# To DO:
#
#
#
### ======================================================================================================
### Initialise system, including convenience functions and title display
### ======================================================================================================

rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

library(FLEDA)
library(FLCore)
library(FLash)
# library(FLSAM)

#-Load the libraries needed
library(MASS)#7.2-47
library(minpack.lm)

### ======================================================================================================


#- Set a path here
try(setwd("D:/git/wg_HAWG/NSAS/benchmark/R/2_newM"),silent=TRUE)

### ======================================================================================================
data.source  <-  file.path(".","stf")      #Data source
output.dir   <-  file.path(".","stf")       #Output directory
output.base  <-  file.path(output.dir)

### ======================================================================================================
### Read in the data
### ======================================================================================================


####------Here load in assessmnet .Rdata instead of running through all the rest

load(file.path("D:/GIT/wg_HAWG/NSAS/benchmark/results/2_newM/NSH.RData"))
stk<-NSH

### ======================================================================================================
### Projections
### ======================================================================================================

## three year forecast
## Define years

## THIS NEEDS TO RUN IN THE 32 version of R

library(FLAssess)

#Define years
TaY <- dims(NSH)$maxyear   #Terminal assessment year (2016 in 2017 HAWG)
ImY <- TaY+1                #Intermediate Year (TAC year - 2017 in 2017 HAWG)
AdY <- TaY+2                #Advice year (Advice year - 2018 in 2017 HAWG)
CtY <- TaY+3                #Continuation year - not of major concern but used in calculations in places
tbl.yrs     <- as.character(c(ImY:my))   #Years to report in the output table

#In NSH - use geometric mean of last 3 years from final SAM.out file from assessment
rec <- exp(mean(log((trim(NSH@stock.n, age=0,year=2014:2016))))) 

## use geomean stock recruit
NSH.srr <- list(model="geomean",params=FLPar(rec))


#Expand stock object (adds three years for the forecast to the stock object)
NSH.proj <- stf(NSH,nyears=10,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE)

# Project stock forward
NSH.proj@stock.n[ac(1:8),ac(ImY)]  <- 
  NSH@stock.n[ac(0:7),ac(TaY)] * 
  exp(-NSH@harvest[ac(0:7),ac(TaY)]-NSH@m[ac(0:7),ac(TaY)])

# Plus group
NSH.proj@stock.n[ac(8),ac(ImY)]    <- 
  NSH.proj@stock.n[ac(8),ac(ImY)] + NSH@stock.n[ac(8),ac(TaY)] * 
  exp(-NSH@harvest[ac(8),ac(TaY)]-NSH@m[ac(8),ac(TaY)])

NSH.proj@stock.n[1,as.character(c(ImY:dims(NSH.proj)$maxyear))] <- rec


# check values
NSH.proj@stock.n

#Define some constants
#intermediate year catch (be 2017 in hawg 2017 forecast)
  
ImY.catch <- 527500 

#advice year catch (year after assessmnet)
AdY.catch <- 527500   #another rollover

numFmsy <- 0.33
numFmgt <- 0.26
numFpa  <- 0.34
numFlim <- 0.39
numFImY <- 0.26
my      <- dims(NSH.proj)$maxyear
fy      <- my - ImY 
  
#Setup options
options.l <- list(
  # Zero catch
  # "Catch(2018) = Zero"=
  #   fwdControl(data.frame(year=c(ImY,AdY,CtY),
  #                         quantity="catch",
  #                         val=c(ImY.catch,0,0))),
  
  # #Intermediate year catch equal TAC, followed by -15% Catch reduction
  # "Catch(2018) = 2017 Monitoring TAC -15%"=
  #   fwdControl(data.frame(year=c(ImY,AdY,CtY),
  #                         quantity=c("catch","catch","f"),
  #                         rel=c(NA,NA,AdY),
  #                         val=c(ImY.catch,AdY.catch*0.85,NA))),
  
  # #Intermediate year catch equal TAC, followed by +0% Catch increase
  # "Catch(2018) = 2017 TAC"=
  #   fwdControl(data.frame(year=c(ImY,AdY,CtY),
  #                         quantity=c("catch","catch","f"),
  #                         rel=c(NA,NA,AdY),
  #                         val=c(ImY.catch,AdY.catch*1,NA))),
  
  # #Intermediate year catch equal TAC, followed by +15% Catch increase
  # "Catch(2018) = 2017 TAC +15%"=
  #   fwdControl(data.frame(year=c(ImY,AdY,CtY),
  #                         quantity=c("catch","catch","f"),
  #                         rel=c(NA,NA,AdY),
  #                         val=c(ImY.catch,AdY.catch*1.15,NA))),
  
  # #Intermediate year catch equal TAC, followed by +30% Catch increase
  # "Catch(2018) = 2017 TAC +30%"=
  #   fwdControl(data.frame(year=c(ImY,AdY,CtY),
  #                         quantity=c("catch","catch","f"),
  #                         rel=c(NA,NA,AdY),
  #                         val=c(ImY.catch,AdY.catch*1.30,NA))),
  
  # #Intermediate year catch equal TAC, followed by -30% Catch reduction
  # "Catch(2018) = 2017 TAC -30%"=
  #   fwdControl(data.frame(year=c(ImY,AdY,CtY),
  #                         quantity=c("catch","catch","f"),
  #                         rel=c(NA,NA,AdY),
  #                         val=c(ImY.catch,AdY.catch*0.70,NA))),
  
  # #Intermediate year catch equal TAC, followed by Fbar=Fpa(0.18)
  # "Fbar(2018) = Fpa"=
  #   fwdControl(data.frame(year=c(ImY,AdY,CtY),
  #                         quantity=c("catch","f","f"),
  #                         rel=c(NA,NA,AdY),
  #                         val=c(ImY.catch,numFpa,NA))),
  
  #Intermediate year catch equal TAC, followed Fbar = Fmsy (0.26)
  "Fbar(2018) = Fmsy"=
    fwdControl(data.frame(
      year     = c(ImY:my),
      quantity = c("catch", rep("f",fy) ),
      rel      = c(NA,AdY:my),
      val      = c(ImY.catch, rep(numFmsy, fy))))
  
  # #Intermediate year catch equal TAC, followed Fbar = Fmgt (0.23)
  # "Fbar(2018) = Fmgt"=
  #   fwdControl(data.frame(year=c(ImY,AdY,CtY),
  #                         quantity=c("catch","f","f"),
  #                         rel=c(NA,NA,AdY),
  #                         val=c(ImY.catch,numFmgt,NA))),
  # #Intermediate year catch equal TAC, followed Fbar = Flim (0.30)
  # "Fbar(2018) = Flim"=
  #   fwdControl(data.frame(year=c(ImY,AdY,CtY),
  #                         quantity=c("catch","f","f"),
  #                         rel=c(NA,NA,AdY),
  #                         val=c(ImY.catch,numFlim,NA))),
  # #Intermediate year catch equal TAC, followed Fbar = F
  # "Fbar(2018) = F (2017)"=
  #   fwdControl(data.frame(year=c(ImY,AdY,CtY),
  #                         quantity=c("catch","f","f"),
  #                         rel=c(NA,NA,AdY),
  #                         val=c(ImY.catch,numFImY,NA))),
  # #need to run forecast first to get predicted ssb(2017)
  # #Then, Fbar(2018)estimated F from predicted ssb(2017)/410000(Blim) x Flim(0.16) of NSH stock reference points  
  # "Fbar(2018) = 0.052"=
  #   fwdControl(data.frame(year=c(ImY,AdY,CtY),
  #                        quantity=c("catch","f","f"),
  #                         rel=c(NA,NA,AdY),
  #                         val=c(ImY.catch,0.05164488,NA)))
) #End options list


#Multi-options table
fmult.targs  <- seq(0,2,by=0.1)

mult.opts.l <- lapply(as.list(fmult.targs),function(fmult) {
  fwdControl(data.frame(year     = c(ImY,AdY,CtY),
                        quantity = c("catch","f","f"),
                        rel      = c(NA,ImY,AdY),
                        val      = c(ImY.catch,fmult,1)))
})

names(mult.opts.l) <- sprintf("Fmult(2017) = %4.3f",fmult.targs)

# run the forecast options
NSH.options   <- lapply(options.l,function(ctrl) {fwd(NSH.proj,ctrl=ctrl,sr=NSH.srr)}) # runs the forecast

# run the multioption scanning
NSH.mult.opts <- lapply(mult.opts.l,function(ctrl) {fwd(NSH.proj,ctrl=ctrl,sr=NSH.srr)}) # run 

# write out the input file
input.tbl.file <- file.path(output.dir,"options - input.csv",sep="")
write.table(NULL,file=input.tbl.file,col.names=FALSE,row.names=FALSE)
input.tbl.list <- list(N="stock.n",M="m",Mat="mat",PF="harvest.spwn",
                       PM="m.spwn",SWt="stock.wt",Sel="harvest",CWt="catch.wt")
for(yr in c(ImY:my)){
  col.dat <- sapply(input.tbl.list,function(slt) slot(NSH.proj,slt)[,as.character(yr),drop=TRUE])
  write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
  write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

#Detailed options table
options.file <-file.path(output.dir,"options - details.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(NSH.options)) {
  opt <- names(NSH.options)[i]
  stk <- NSH.options[[opt]]
  
  #Now the F and N by age
  nums.by.age <- stk@stock.n[,tbl.yrs,drop=TRUE]
  colnames(nums.by.age) <- sprintf("N(%s)",tbl.yrs)
  f.by.age    <- stk@harvest[,tbl.yrs,drop=TRUE]
  colnames(f.by.age) <- sprintf("F(%s)",tbl.yrs)
  age.tbl     <- cbind(Age=rownames(f.by.age),N=nums.by.age,F=f.by.age)
  
  #And now the summary tbl
  sum.tbl     <- cbind(Year=tbl.yrs,SSB=ssb(stk)[,tbl.yrs],
                       F.bar=fbar(stk)[,tbl.yrs],Yield=computeCatch(stk)[,tbl.yrs])
  
  #Now, bind it all together
  sum.tbl.padding <- matrix("",nrow=nrow(age.tbl)-nrow(sum.tbl),ncol=ncol(sum.tbl))
  comb.tbl    <- cbind(age.tbl," ",rbind(sum.tbl,sum.tbl.padding))
  
  #And write it - hdr first, then the rest
  write.table(sprintf("%s). %s",letters[i],opt),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
  write.table(t(colnames(comb.tbl)),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
  write.table(comb.tbl,options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
  write.table(c(""),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
}

#Options summary table
opt.sum.tbl <- function(stcks,fname) {
  options.sum.tbl <- sapply(as.list(1:length(stcks)),function(i) {
    opt <- names(stcks)[i]
    stk <- stcks[[opt]]
    
    #Build up the summary
    sum.tbl     <- data.frame(Rationale=opt,
                              F.ImY=fbar(stk)[,as.character(ImY),drop=TRUE],
                              Catch.ImY=computeCatch(stk)[,as.character(ImY),drop=TRUE],
                              SSB.ImY=ssb(stk)[,as.character(ImY),drop=TRUE],
                              F.AdY=fbar(stk)[,as.character(AdY),drop=TRUE],
                              Catch.AdY=computeCatch(stk)[,as.character(AdY),drop=TRUE],
                              SSB.AdY=ssb(stk)[,as.character(AdY),drop=TRUE],
                              SSB.CtY=ssb(stk)[,as.character(CtY),drop=TRUE])
  })
  
  options.sum.tbl <- t(options.sum.tbl)
  colnames(options.sum.tbl) <- c("Rationale",
                                 sprintf("Fbar (%i)",ImY),sprintf("Catch (%i)",ImY),sprintf("SSB (%i)",ImY),
                                 sprintf("Fbar (%i)",AdY),sprintf("Catch (%i)",AdY),sprintf("SSB (%i)",AdY),
                                 sprintf("SSB (%i)",CtY))
  write.csv(options.sum.tbl,file=fname,row.names=FALSE)
}
opt.sum.tbl(stcks=NSH.options,fname=file.path(output.base,"options - summary.csv",sep="."))
opt.sum.tbl(stcks=NSH.mult.opts,fname=file.path(output.base,"multi-options - summary.csv",sep="."))


#How to call ssb for one of the options
plot(ssb(NSH.options[[1]]))
plot(catch(NSH.options[[1]]))


