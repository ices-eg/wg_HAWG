################################################################################
# Code to do the short term forecast for VIa Herring
# using R 3.1.3
# 
# Last updated 18/Mar/2017 8:02
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

#-Load the libraries needed
library(MASS)#7.2-47
library(minpack.lm)

### ======================================================================================================


#- Set a path here
path<-"C:/Users/Lusseaus/Documents/ICES Working Groups/HAWG/Assesments/hawg/wg_HAWG/VIa/"
try(setwd(path))


### ======================================================================================================
data.source  <-  file.path(".","stf")      #Data source
output.dir   <-  file.path(".","stf")       #Output directory
output.base  <-  file.path(output.dir)
### ======================================================================================================
### Read in the data
### ======================================================================================================


####------Here load in assessmnet .Rdata instead of running through all the rest

load("C:/Users/Lusseaus/Documents/ICES Working Groups/HAWG/Assesments/hawg/wg_HAWG/VIa/results/2017_VIaHerring.Rdata")

stk<-MSH

### ======================================================================================================
### Projections
### ======================================================================================================

## three year forecast
## Define years

library(FLAssess)
#Define years
TaY <- dims(MSH)$maxyear   #Terminal assessment year (2016 in 2017 HAWG)
ImY <- TaY+1                #Intermediate Year (TAC year - 2017 in 2017 HAWG)
AdY <- TaY+2                #Advice year (Advice year - 2018 in 2017 HAWG)
CtY <- TaY+3                #Continuation year - not of major concern but used in calculations in places
tbl.yrs     <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table


#In MSH - use geometric mean of last 5 years (2012 - 2016) from final SAM.out file from assessment

rec <- 733366 #update each year or code to take rec 2012 onwards.

## use geomean stock recruit
MSH.srr <- list(model="geomean",params=FLPar(rec))


#Expand stock object (adds three years for the forecast to the stock object)

MSH.proj <- stf(MSH,nyears=3,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE)

MSH.proj@stock.n[ac(2:9),ac(ImY)]  <- MSH@stock.n[ac(1:8),ac(TaY)] * exp(-MSH@harvest[ac(1:8),ac(TaY)]-MSH@m[ac(1:8),ac(TaY)])

MSH.proj@stock.n[ac(9),ac(ImY)]    <- MSH.proj@stock.n[ac(9),ac(ImY)] + MSH@stock.n[ac(9),ac(TaY)] * exp(-MSH@harvest[ac(9),ac(TaY)]-MSH@m[ac(9),ac(TaY)])

MSH.proj@stock.n[1,as.character(c(ImY,AdY,CtY))] <- rec

# check values
MSH.proj@stock.n

#Define some constants
#intermediate year catch (be 2017 in hawg 2017 forecast)
  
ImY.catch <- 5800 #using 5800t in 2017 as this has pre-liminarily been agreed at EC December council

#advice year catch (year after assessmnet)
AdY.catch <- 5800   #another rollover

numFmsy <- 0.16
numFmgt <- 0.05
numFpa <- 0.18
numFlim<- 0.30
numFImY<-0.041209155

#Setup options
options.l <- list(#Zero catch
  "Catch(2018) = Zero"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity="catch",
                          val=c(ImY.catch,0,0))),
  #Intermediate year catch equal TAC, followed by -15% Catch reduction
  "Catch(2018) = 2017 Monitoring TAC -15%"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,AdY.catch*0.85,NA))),
  #Intermediate year catch equal TAC, followed by +0% Catch increase
  "Catch(2018) = 2017 TAC"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,AdY.catch*1,NA))),
  #Intermediate year catch equal TAC, followed by +15% Catch increase
  "Catch(2018) = 2017 TAC +15%"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,AdY.catch*1.15,NA))),
  #Intermediate year catch equal TAC, followed by +30% Catch increase
  "Catch(2018) = 2017 TAC +30%"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,AdY.catch*1.30,NA))),
  #Intermediate year catch equal TAC, followed by -30% Catch reduction
  "Catch(2018) = 2017 TAC -30%"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,AdY.catch*0.70,NA))),
  #Intermediate year catch equal TAC, followed by Fbar=Fpa(0.18)
  "Fbar(2018) = Fpa"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,numFpa,NA))),
  #Intermediate year catch equal TAC, followed Fbar = Fmsy (0.26)
  "Fbar(2018) = Fmsy"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,numFmsy,NA))),
  #Intermediate year catch equal TAC, followed Fbar = Fmgt (0.23)
  "Fbar(2018) = Fmgt"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,numFmgt,NA))),
  #Intermediate year catch equal TAC, followed Fbar = Flim (0.30)
  "Fbar(2018) = Flim"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,numFlim,NA))),
  #Intermediate year catch equal TAC, followed Fbar = F
  "Fbar(2018) = F (2017)"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,numFImY,NA))),
  #need to run forecast first to get predicted ssb(2017)
  #Then, Fbar(2018)estimated F from predicted ssb(2017)/410000(Blim) x Flim(0.16) of MSH stock reference points  
  "Fbar(2018) = 0.052"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                         quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,0.05164488,NA)))
) #End options list


#Multi-options table
fmult.targs  <- seq(0,2,by=0.1)
mult.opts.l <- lapply(as.list(fmult.targs),function(fmult) {
  fwdControl(data.frame(year=c(ImY,AdY,CtY),
                        quantity=c("catch","f","f"),
                        rel=c(NA,ImY,AdY),
                        val=c(ImY.catch,fmult,1)))
})
names(mult.opts.l) <- sprintf("Fmult(2017) = %4.3f",fmult.targs)

#Calculate options
MSH.options   <- lapply(options.l,function(ctrl) {fwd(MSH.proj,ctrl=ctrl,sr=MSH.srr)}) # runs the forecast
MSH.mult.opts <- lapply(mult.opts.l,function(ctrl) {fwd(MSH.proj,ctrl=ctrl,sr=MSH.srr)}) # run 

input.tbl.file <-file.path(output.dir,"options - input.csv",sep=".")
write.table(NULL,file=input.tbl.file,col.names=FALSE,row.names=FALSE)
input.tbl.list <- list(N="stock.n",M="m",Mat="mat",PF="harvest.spwn",
                       PM="m.spwn",SWt="stock.wt",Sel="harvest",CWt="catch.wt")
for(yr in c(ImY,AdY,CtY)){
  col.dat <- sapply(input.tbl.list,function(slt) slot(MSH.proj,slt)[,as.character(yr),drop=TRUE])
  write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
  write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

#Detailed options table
options.file <-file.path(output.dir,"options - details.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(MSH.options)) {
  opt <- names(MSH.options)[i]
  stk <- MSH.options[[opt]]
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
opt.sum.tbl(stcks=MSH.options,fname=file.path(output.base,"options - summary.csv",sep="."))
opt.sum.tbl(stcks=MSH.mult.opts,fname=file.path(output.base,"multi-options - summary.csv",sep="."))


#How to call ssb for one of the options
ssb(MSH.options[[1]])


