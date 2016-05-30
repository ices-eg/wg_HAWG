################################################################################
# Code to do the short term forecast for Celtic Sea Herring
#
# 2016

#install.packages("FLCore", repos="http://flr-project.org/R")

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


#- Perhaps, set a path here

#path<-"W:/PERSONAL/Afra/HAWG 2015/Celtic Sea/2015 Forecast/FLR/Final assessment new nat mor/"
#path<-"W:/PERSONAL/Afra/HAWG 2015/Celtic Sea/2015 Forecast/FLR/Final Run with all figures 24_03/"
path<-"C:/ICES/HAWG2016/Forecast/2016"

try(setwd(path))

#try(setwd("./stf/"))

#path  <- getwd()

### ======================================================================================================
data.source  <-  file.path("stf")      #Data source
output.dir   <-  file.path("stf")       #Output directory
output.base  <-  file.path(output.dir)
### ======================================================================================================

### Read in the data
### ======================================================================================================

CSH<- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)
#Set no discards
CSH@catch.n                <- CSH@landings.n
CSH@catch                  <- CSH@landings
CSH@catch.wt               <- CSH@landings.wt
units(CSH)[1:17]           <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))



#Set fbar
range(CSH)[c("minfbar","maxfbar")] <- c(2,5)

#Set plus group
CSH<- setPlusGroup(CSH,CSH@range["max"])

#Set stock object name - this is propagated through into the figure titles
CSH@name    <- "Celtic Sea Herring"

### ======================================================================================================
### Read in the index
### ======================================================================================================

#Load and modify all index data
CSH.tun   <- readFLIndices(file.path(data.source, "fleet.txt"))
#CSH.tun   <- readFLIndices(file.path(data.source, "fleet_inc2015.txt"))
#CSH.tun   <- readFLIndices(file.path(data.source, "fleet_inc2014_15.txt"))

#Set names, and parameters etc
names(CSH.tun) <-  gsub(":.*$","",names(CSH.tun))
CSH.tun   <- lapply(CSH.tun,function(idx) {
                idx@type 	     <- 	"number"
          		idx@index.var[]  <-	1
                idx@range["plusgroup"] <- NA
          		return(idx)})


names(CSH.tun)[1] <- c("Celtic Sea Herring Acoustic")

#internal consistency plot
#plot(CSH.tun[["Celtic Sea Herring Acoustic"]],type="internal", main="Celtic Sea Herring Acoustic")
 
#===============================================================================

###- Load the assessment output objects from the ASAP assessment
### Add to the CSH FLStock object


CSH@stock.n<-readVPAFile(file.path(data.source, "N.txt"))

CSH@harvest<-readVPAFile(file.path(data.source, "F.txt"))
units(CSH@harvest)<-'f'

#look ssb at spawning time - compare with stock summary table
ssb(CSH)


#stk <- CSH
summary(CSH)
 


stk<-CSH

### ======================================================================================================
### Projections
### ======================================================================================================

## three year forecast
#Define years

library(FLAssess)
#Define years
TaY <- dims(CSH)$maxyear   #Terminal assessment year
ImY <- TaY+1                #Intermediate Year
AdY <- TaY+2                #Advice year
CtY <- TaY+3                #Continuation year - not of major concern but used in calculations in places
tbl.yrs     <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table



# use breakpoint from julios stock recruitment
#rec <- 541287 #2015 figure
#2016 update
rec <- 537900

## use geomean stock recruit
CSH.srr <- list(model="geomean",params=FLPar(rec))


#Expand stock object

CSH.proj <- stf(CSH,nyears=3,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE)

CSH.proj@stock.n[ac(2:9),ac(ImY)]  <- CSH@stock.n[ac(1:8),ac(TaY)] * exp(-CSH@harvest[ac(1:8),ac(TaY)]-CSH@m[ac(1:8),ac(TaY)])

CSH.proj@stock.n[ac(9),ac(ImY)]    <- CSH.proj@stock.n[ac(9),ac(ImY)] + CSH@stock.n[ac(9),ac(TaY)] * exp(-CSH@harvest[ac(9),ac(TaY)]-CSH@m[ac(9),ac(TaY)])

CSH.proj@stock.n[1,as.character(c(ImY,AdY,CtY))] <- rec

# check values
CSH.proj@stock.n


#Define some constants
#intermediate year catch
#ImY.catch <- 18356  
ImY.catch <- 18590

#advice year catch
#AdY.catch <- 15652 #2015 TAC
AdY.catch <- 15442

numFmsy <- 0.26
numFmgt <- 0.23
numFpa <- 0.37
numFlim <- 0.61

#Setup options
options.l <- list(#Zero catch
  "Catch(2017) = Zero"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity="catch",
                          val=c(ImY.catch,0,0))),
  #Intermediate year catch equal TAC, followed by -15% Catch reduction
  "Catch(2017) = 2016 TAC -15%"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,AdY.catch*0.85,NA))),
  #Intermediate year catch equal TAC, followed by +0% Catch increase
  "Catch(2017) = 2016 TAC"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,AdY.catch*1,NA))),
  #Intermediate year catch equal TAC, followed by +15% Catch increase
  "Catch(2017) = 2016 TAC +15%"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,AdY.catch*1.15,NA))),
  #Intermediate year catch equal TAC, followed by +30% Catch increase
  "Catch(2017) = 2016 TAC +30%"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,AdY.catch*1.30,NA))),
  #Intermediate year catch equal TAC, followed by -30% Catch reduction
  "Catch(2017) = 2016 TAC -30%"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","catch","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,AdY.catch*0.70,NA))),
  #Intermediate year catch equal TAC, followed Fbar = Fmsy (0.26)
  "Fbar(2017) = Fmsy"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,numFmsy,NA))),
  #Intermediate year catch equal TAC, followed Fbar = Fmgt (0.23)
  "Fbar(2017) = Fmgt"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,numFmgt,NA))),
  #Intermediate year catch equal TAC, followed Fbar = Fpa (0.37)
  "Fbar(2017) = Fpa"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,numFpa,NA))),
  #Intermediate year catch equal TAC, followed Fbar = Flim (0.61)
  "Fbar(2017) = Flim"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,numFlim,NA))),
  
  "Fbar(2017) = 0.18"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          rel=c(NA,NA,AdY),
                          val=c(ImY.catch,0.18,NA)))
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
CSH.options   <- lapply(options.l,function(ctrl) {fwd(CSH.proj,ctrl=ctrl,sr=CSH.srr)})
CSH.mult.opts <- lapply(mult.opts.l,function(ctrl) {fwd(CSH.proj,ctrl=ctrl,sr=CSH.srr)})

input.tbl.file <-file.path(output.dir,"options - input.csv",sep=".")
write.table(NULL,file=input.tbl.file,col.names=FALSE,row.names=FALSE)
input.tbl.list <- list(N="stock.n",M="m",Mat="mat",PF="harvest.spwn",
                       PM="m.spwn",SWt="stock.wt",Sel="harvest",CWt="catch.wt")
for(yr in c(ImY,AdY,CtY)){
  col.dat <- sapply(input.tbl.list,function(slt) slot(CSH.proj,slt)[,as.character(yr),drop=TRUE])
  write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
  write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

#Detailed options table
options.file <-file.path(output.dir,"options - details.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(CSH.options)) {
  opt <- names(CSH.options)[i]
  stk <- CSH.options[[opt]]
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
opt.sum.tbl(stcks=CSH.options,fname=file.path(output.base,"options - summary.csv",sep="."))
opt.sum.tbl(stcks=CSH.mult.opts,fname=file.path(output.base,"multi-options - summary.csv",sep="."))




