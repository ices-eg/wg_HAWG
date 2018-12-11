################################################################################
# Code to do the short term forecast for North Sea turbot
# using R 3.4.3
# 
# Last updated 18/Mar/2018
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
library(FLAssess)
library(FLasher) #x64
library(FLash)   #i386

#-Load the libraries needed
library(MASS)#7.3-47
library(minpack.lm)# 1.2-1

### ======================================================================================================


#- Set a path here
path    <- "D:/Repository/Turbot/IBP_2018/ShortTermForecast/"   # folder were the code is and where results will be saved (in a subfolder)
try(setwd(path))


### ======================================================================================================
output.dir   <-  file.path(".")       #Output directory
### ======================================================================================================
### Read in the data
### ======================================================================================================
load("../Final_run_2018/Final_2018.RData")
TUR@harvest <- TUR.sam@harvest
TUR@stock.n <- TUR.sam@stock.n
stk         <- TUR

### ======================================================================================================
### Projections
### ======================================================================================================

## three year forecast
## Define years

library(FLAssess)
#Define years
TaY <- dims(TUR)$maxyear   #Terminal assessment year (2017 in 2018 HAWG)
ImY <- TaY+1                #Intermediate Year (TAC year - 2018 in 2018 HAWG)
AdY <- TaY+2                #Advice year (Advice year - 2019 in 2018 HAWG)
CtY <- TaY+3                #Continuation year - not of major concern but used in calculations in places
maA <- range(TUR)["max"]
miA <- range(TUR)["min"]
tbl.yrs     <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table


#In TUR - use geometric mean of last years (2013 - 2017) from final SAM.out file from assessment
TUR.srr <- list(model="geomean",params=FLPar(exp(mean(log(rec(TUR))))))

#Expand stock object (adds three years for the forecast to the stock object)
TUR.proj <- stf(TUR,nyears=3,wts.nyears=1,arith.mean=TRUE,na.rm=TRUE)

#take 3-year mean on selectivity & update stock numbers at age
TUR.proj@harvest[,as.character(c(ImY,AdY,CtY))] <- yearMeans(TUR.proj@harvest[,ac((TaY-2):TaY)])
TUR.proj@stock.n[ac((miA+1):maA),ac(ImY)]       <- TUR@stock.n[ac(miA:(maA-1)),ac(TaY)] * exp(-TUR@harvest[ac(miA:(maA-1)),ac(TaY)]-TUR@m[ac(miA:(maA-1)),ac(TaY)])
TUR.proj@stock.n[ac(maA),ac(ImY)]               <- TUR.proj@stock.n[ac(maA),ac(ImY)] + TUR@stock.n[ac(maA),ac(TaY)] * exp(-TUR@harvest[ac(maA),ac(TaY)]-TUR@m[ac(maA),ac(TaY)])
TUR.proj@stock.n[1,as.character(c(ImY,AdY,CtY))]<- c(TUR.srr$params$a)

# check values
TUR.proj@stock.n

#Define some constants
#intermediate year catch (be 2018 in hawg 2018 forecast)

ImY.catch <- 4616.3  #5-year average proportion of catch of TUR from TUR+BLL TAC multiplied with TAC 2018
ImY.F     <- round(fbar(TUR)[,ac(TaY)],2)
AdY.catch <- 4952

numFmsy   <- 0.36
numFpa    <- 0.43
numFlim   <- 0.61
numFsq    <- round(fbar(TUR)[,ac(TaY)],2)
numFupper <- 0.48
numFlower <- 0.25
numBlim   <- 2974
numBpa    <- 4163
numBtrig  <- 6353


#Setup options
options.l <- list(#Zero catch
  "Catch(2019) = Zero"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("f","catch","catch"),
                          val=c(ImY.F,0,0))),
  #Intermediate year catch equal TAC, followed by +0% Catch increase
  "Catch(2019) = 2018 TAC"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("f","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.F,AdY.catch*1,NA))),
  #Intermediate year catch equal TAC, followed by +15% Catch increase
  "Catch(2019) = 2018 TAC +15%"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("f","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.F,AdY.catch*1.15,NA))),
  #Intermediate year catch equal TAC, followed by -30% Catch reduction
  "Catch(2019) = 2018 TAC -15%"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("f","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.F,AdY.catch*0.85,NA))),
  #Intermediate year catch equal TAC, followed by Fbar=Fpa(0.18)
  "Fbar(2019) = Fpa"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("f","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.F,numFpa,NA))),
  #Intermediate year status quo F, followed by Fbar=Fpa
  "Fbar(2019) = Fupper"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("f","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.F,numFupper,NA))),
  #Intermediate year status quo F, followed by Fbar=Fpa
  "Fbar(2019) = Flower"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("f","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.F,numFlower,NA))),
  #Intermediate year catch equal TAC, followed Fbar = Fmsy (0.26)
  "Fbar(2019) = Fmsy"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("f","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.F,numFmsy,NA))),
  #Intermediate year catch equal TAC, followed Fbar = Flim (0.30)
  "Fbar(2019) = Flim"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("f","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.F,numFlim,NA))),
  #Intermediate year catch equal TAC, followed Fbar = Blim
  "Fbar(2019) = Blim"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("f","ssb","ssb"),
                          rel=c(NA,NA,NA),
                          val=c(ImY.F,numBlim,numBlim))),
  #Intermediate year catch equal TAC, followed Fbar = Bpa
  "Fbar(2019) = Bpa"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("f","ssb","ssb"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.F,numBpa,numBpa))),
  #Intermediate year catch equal TAC, followed Fbar = Btrig
  "Fbar(2019) = Btrig"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("f","ssb","ssb"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.F,numBtrig,numBtrig)))
  
  #Then, Fbar(2019)estimated F from predicted ssb(2018)/410000(Blim) x Flim(0.16) of TUR stock reference points
) #End options list


#Multi-options table
fmult.targs  <- seq(0,2,by=0.1)
mult.opts.l <- lapply(as.list(fmult.targs),function(fmult) {
  fwdControl(data.frame(year=c(ImY,AdY,CtY),
                        quantity=c("catch","f","f"),
                        rel=c(NA,ImY,AdY),
                        val=c(ImY.catch,fmult,1)))
})
names(mult.opts.l) <- sprintf("Fmult(2018) = %4.3f",fmult.targs)

#Calculate options
TUR.options   <- lapply(options.l,function(ctrl) {fwd(TUR.proj,ctrl=ctrl,sr=TUR.srr)}) # runs the forecast
TUR.mult.opts <- lapply(mult.opts.l,function(ctrl) {fwd(TUR.proj,ctrl=ctrl,sr=TUR.srr)}) # run

input.tbl.file <-file.path(path,"options - input - rbp.csv",sep=".")
write.table(NULL,file=input.tbl.file,col.names=FALSE,row.names=FALSE)
input.tbl.list <- list(N="stock.n",M="m",Mat="mat",PF="harvest.spwn",
                       PM="m.spwn",SWt="stock.wt",Sel="harvest",CWt="catch.wt")
for(yr in c(ImY,AdY,CtY)){
  col.dat <- sapply(input.tbl.list,function(slt) slot(TUR.proj,slt)[,as.character(yr),drop=TRUE])
  write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
  write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

#Detailed options table
options.file <-file.path(path,"options - details - rbp.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(TUR.options)) {
  opt <- names(TUR.options)[i]
  stk <- TUR.options[[opt]]
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
opt.sum.tbl(stcks=TUR.options,fname=file.path(path,"options - summary - rbp.csv",sep="."))
opt.sum.tbl(stcks=TUR.mult.opts,fname=file.path(path,"multi-options - summary - rbp.csv",sep="."))


#How to call ssb for one of the options
ssb(TUR.options[[1]])