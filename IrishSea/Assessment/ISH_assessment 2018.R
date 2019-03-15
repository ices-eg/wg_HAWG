
library(FLCore)
library(FLSAM)
library(FLEDA)

rm(list=ls())

#- Set paths
my.path<-file.path("C:","Work","HAWG2018","SAM")
output.dir              <- file.path(my.path,"results")
data.source         <- file.path(my.path,"data")    #Data source, not code or package source!!!

#- Setup stock, tun and control
scenario<- list(); scenario$trimTS   <- T
#source(file.path(my.path,"benchmark/setupStockTun.r"))
source(file.path(my.path,"setupStockTunIII.r"))
source(file.path(my.path,"setupControl.r"))
ISH.ctrl@logN.vars[] <- 1
ISH.ctrl@f.vars["catch",] <- c(1,1,2,2,2,3,4,4)
#ISH_bench.ctrl@catchabilities["AC(VIIaN)",] <- c(1,1,2,3,3,4,4,4)
ISH.ctrl@obs.vars["catch",] <- c(1,2,2,2,3,3,3,3)
ISH.ctrl@obs.vars["AC(VIIaN)",] <- c(4,rep(5,4),rep(6,3))
#- Run final model
#only need the run dir setup below if sam.exe name is longer than 4 characters
ISH.ctrl@sam.binary <- file.path(my.path,"sam.exe")
ISH.sam       <- FLSAM(ISH,ISH.tun,ISH.ctrl)#,run.dir="C:/Work/HAWG2018/rundir")
ISH.sam       <- SAM2FLR(ctrl=ISH.ctrl,admb.stem="sam",run.dir="C:/Work/HAWG2018/rundir")
#ISH.retro       <- FLSAM(ISH,ISH.tun,ISH.ctrl,run.dir="C:/Work/HAWG2018/rundir",5)
#ISH.retro     <- SAM2FLR(ctrl=ISH.ctrl,admb.stem="sam",run.dir="C:/Work/HAWG2018/rundir/")
#ISH.retro      <- retro(ISH,ISH.tun,ISH.ctrl,1)

#save results
name(ISH.sam) <- "ISH_assessment 2018"
ISH@stock.n <- ISH.sam@stock.n
ISH@harvest <- ISH.sam@harvest

save(ISH,ISH,ISH.tun,ISH.ctrl,ISH.sam,file=file.path(output.dir,paste(name(ISH.sam),".RData",sep="")))
save(ISH.retro,file=file.path(output.dir,paste(name(ISH.sam),"_retro.RData",sep="")))

#quick checks
obs.var(ISH.sam)
plot(ISH.sam)
ISH.sam@nlogl
ssb(ISH)<-ssb(ISH.sam)$value
fbar(ISH.sam)$value

#setwd("C:\\Work\\HAWG2018\\")
#load("RETRO_ISH.rdata")

plot(ISH.retro)

##############
##############
##############

my.path<-file.path("C:/Work/HAWG2018/SAM/")
output.dir<- file.path(my.path,"results")
source(file.path(my.path,"_Common","HAWG_Common_module.r"))
# Need to have following list of r files in same folder: "Stacked Area plot.r", "WriteIcaSum.r",
#"writeStandardOutput.r","Taylor_diagram.r"
# Also need to load package(doBy) and (reShape)

#Setup plots
pdf(file.path(output.dir,paste("ISH.pdf",sep="")))
### ============================================================================
### Input data
### ============================================================================


# Plot the mature and immature part of the stock
print(mat.immat.ratio(ISH))

# Plot the overlay of tuning series
#print(overlayTimeseries(lapply(ISH.tun,index),nyrs=10,ages=0:4)) #not much change in ages 5+

# Plot the overlay by year and age
print(surveyTimeseries(ISH.tun))


#plot(tmp.tun,type="pairwise")
plot(ISH.tun[["AC(VIIaN)"]],type="internal")


print(stacked.area.plot(data~year| unit, as.data.frame(pay(ISH@catch.n)),groups="age",main="Proportion of Catch numbers at age",ylim=c(-0.01,1.01),xlim=c(range(ISH)["minyear"]-2,range(ISH)["maxyear"]+1),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(ISH.tun[["AC(VIIaN)"]]@index)),groups="age",main="Proportion of Acoustic index at age",ylim=c(-0.01,1.01),xlim=c(1993.5,range(ISH.tun)["maxyear"]+0.5),xlab="years",col=gray(9:0/9)))

# Plot the time series of weight in the stock and catch in the stock
timeseries(window(ISH,1961,range(ISH)["maxyear"]),slot="stock.wt")
timeseries(window(ISH,1961,range(ISH)["maxyear"]),slot="catch.wt")
timeseries(window(ISH,2000,range(ISH)["maxyear"]),slot="harvest")
timeseries(window(ISH,1990,range(ISH)["maxyear"]),slot="mat")

# Plot the time series of the surveys
timeseries(ISH.tun[["AC(VIIaN)"]],slot="index")
# need to run function below to get rid of age labels for NINEL as oppose to source code - need to reload source code if doing more timeseries plots
timeseries <- function(stck.,slot.){
  assign("stck.",stck.,envir=.GlobalEnv);assign("slot.",slot.,envir=.GlobalEnv);
  print(xyplot(data~year,data=slot(stck.,slot.),
               groups=age,type="b",
               xlab="Year",ylab=paste("Time series of",slot.,ifelse(units(slot(stck.,slot.))=="NA","",paste("(",units(slot(stck.,slot.)),")",sep=""))),
               main=paste(stck.@name,"timeseries of",slot.)))}

#Time series of west by cohort
west.by.cohort      <- as.data.frame(FLCohort(window(ISH@stock.wt,1980,range(ISH)["maxyear"])))
west.by.cohort      <- subset(west.by.cohort,!is.na(west.by.cohort$data))
west.by.cohort$year <- west.by.cohort$age + west.by.cohort$cohort
west.cohort.plot    <- xyplot(data~year,data=west.by.cohort,
                              groups=cohort,
                              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
                              type="b",
                              xlab="Year",ylab="Weight in the stock (kg)",
                              main=paste(ISH@name,"Weight in the stock by cohort"),
                              par.settings=list(superpose.symbol=list(pch=as.character(unique(west.by.cohort$cohort)%%10),cex=1.25)),
                              panel=function(...) {
                                panel.grid(h=-1,v=-1)
                                panel.xyplot(...)
                              })
print(west.cohort.plot)

### ============================================================================
### Model fit
### ============================================================================

#Survey fits
residual.diagnostics(ISH.sam)

# Plot the harvest pattern at age as a proportion over time & the stock.n as proportion over time
print(stacked.area.plot(data~year| unit, as.data.frame(pay(ISH.sam@harvest)),groups="age",main="Proportion of harvest pressure at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(ISH.sam@stock.n)),groups="age",main="Proportion of Stock numbers at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

#Plot result
print(plot(ISH.sam))

#Plot uncertainties as a function of time
CV.yrs <- ssb(ISH.sam)$year
CV.dat <- cbind(SSB=ssb(ISH.sam)$CV,
                Fbar=fbar(ISH.sam)$CV,Rec=rec(ISH.sam)$CV)
matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
        xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")

#Plot catchabilities values
catch <- catchabilities(ISH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,subset(catch,fleet=="AC(VIIaN)"),
             scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
             type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
             main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

#Plot obs_variance (weightings)
obv <- obs.var(ISH.sam)
obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
obv <- obv[order(obv$value),]
bp <- barplot(obv$value,ylab="Observation Variance",
              main="Observation variances by data source",col=factor(obv$fleet),ylim=c(0,1.0))
axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
     pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

#Plot fishery selectivity pattern over time
sel.pat <- merge(f(ISH.sam),fbar(ISH.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
print(xyplot(sel ~ age|sprintf("%i's",floor((year+2)/5)*5),sel.pat,
             groups=year,type="l",as.table=TRUE,
             scale=list(alternating=FALSE),
             main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar"))

#Plot correlation matrix
cor.plot(ISH.sam)

#Plot otholith
#otolith(ISH.sam,year = 2017,n=10000)

###accessing the numbers...viewing it
slotNames(ISH.sam) # list slot names
ISH.sam@nlogl # to view slot "nlogl"= Objective function value
ISH.sam@params
dev.off()

### ======================================================================================================
### Document Assessment
### ======================================================================================================
log.msg     <-  function(string) {cat(string);}
log.msg("\nISH Final Assessment\n=====================\n")
log.msg("GENERATING DOCUMENTATION...\n")
## ======================================================================================================
### Document Assessment
### ======================================================================================================
#source(file.path(my.path,"_Common","FLSAM.out.r")) no longer needed being inbedded in FLSAM
#output.dir          <-  file.path(".","results")                #Output directory
log.msg("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt           <- options("width","scipen")
options("width"=80,"scipen"=1000)

sam.out.file      <- FLSAM.out(ISH,ISH.tun,ISH.sam,format="TABLE 7.6.3.%i Irish Sea Herring.")
write(sam.out.file,file=file.path(output.dir,"ISH_sam.out",sep=".")) #or could create output.base
options("width"=old.opt$width,"scipen"=old.opt$scipen)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
#writeFLStock(ISH,output.file=file.path(output.dir))
writeFLStock(ISH,file.path(output.dir,"hawg_her-nirs.ypr"),type="YPR")

stockSummaryTable <- cbind(rec(ISH.sam)$year,
                           rec(ISH.sam)$value,      rec(ISH.sam)$lbnd,    rec(ISH.sam)$ubnd,
                           tsb(ISH.sam)$value,      tsb(ISH.sam)$lbnd,    tsb(ISH.sam)$ubnd,
                           ssb(ISH.sam)$value,      ssb(ISH.sam)$lbnd,    ssb(ISH.sam)$ubnd,
                           catch(ISH.sam)$value,    catch(ISH.sam)$lbnd,  catch(ISH.sam)$ubnd,
                           catch(ISH.sam)$value / ssb(ISH.sam)$value, catch(ISH.sam)$lbnd / ssb(ISH.sam)$lbnd, catch(ISH.sam)$ubnd / ssb(ISH.sam)$ubnd,
                           fbar(ISH.sam)$value,     fbar(ISH.sam)$lbnd,   fbar(ISH.sam)$ubnd,
                           c(sop(ISH)))
colnames(stockSummaryTable) <-
  c("Year",paste(rep(c("Recruits Age 1 (Thousands)","Total biomass (tonnes)","Spawing biomass (tonnes)",
                       "Landings (tonnes)","Yield / SSB (ratio)","Mean F ages 4-6"),each=3),c("Mean","Low","High")),"SoP (%)")
write.csv(stockSummaryTable,file=file.path(output.dir,"stockSummaryTable.csv"))
options("width"=old.opt$width,"scipen"=old.opt$scipen)

################################################################################
################################################################################
### ============================================================================
### Short term forecast
### ============================================================================
### ============================================================================
### Input data
###=============================================================================
################################################################################
################################################################################

library(FLAssess)
library(FLash)
setwd("C:\\Work\\HAWG2018\\SAM\\Results")
load("ISH_assessment 2018.Rdata")

#survivors
dmns                <- dims(ISH@stock.n)
gm.recruitmentEstimate <- exp(mean(log(ISH@stock.n[1,as.character((ISH@range['maxyear']-11):(ISH@range['maxyear']-2)),,,,])))
survivors           <- FLQuant(c(gm.recruitmentEstimate,stock.n(ISH)[,ac(ISH@range['maxyear'])] * exp(-harvest(ISH[,ac(ISH@range['maxyear'])])-m(ISH[,ac(2012)]))),
                               dimnames=list(ages=dmns$min:(dmns$max+1),year=ISH@range['maxyear']+1,unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter))

## plusgroup
survivors[ac(dmns$max),ac(ISH@range['maxyear']+1)] <- quantSums(survivors[ac(dmns$max:(dmns$max+1)),ac(ISH@range['maxyear']+1)])
survivors           <- survivors[ac(dmns$min:dmns$max),]

write.csv(ISH.sam@params,file=file.path(output.dir,"ISHsam parameters.csv"))

#############
#############
#############
#survivors
dmns <- dims(ISH@stock.n)
gm.recruitmentEstimate <- exp(mean(log(ISH@stock.n[1,as.character((ISH@range['maxyear']-11):(ISH@range['maxyear']-2)),,,,])))
survivors <- FLQuant(c(gm.recruitmentEstimate,stock.n(ISH)[,ac(ISH@range['maxyear'])] * exp(-harvest(ISH[,ac(ISH@range['maxyear'])])-m(ISH[,ac(2015)]))),
                     dimnames=list(ages=dmns$min:(dmns$max+1),year=ISH@range['maxyear']+1,unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter))



## plusgroup
survivors[ac(dmns$max),ac(ISH@range['maxyear']+1)] <- quantSums(survivors[ac(dmns$max:(dmns$max+1)),ac(ISH@range['maxyear']+1)])
survivors           <- survivors[ac(dmns$min:dmns$max),]

write.csv(ISH.sam@params,file=file.path(output.dir,"ISHsam parameters.csv"))

### ======================================================================================================
### Projections
### ======================================================================================================
FnPrint("CALCULATING PROJECTIONS...\n")

#Define years
TaY <- dims(ISH)$maxyear   #Terminal assessment year
ImY <- TaY+1                      #Intermediate Year
AdY <- TaY+2                      #Advice year
CtY <- TaY+3                      #Continuation year - not of major concern but used in calculations in places
tbl.yrs <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table

#Deal with recruitment - a geometric mean of the 10 years prior to the terminal assessment year
dmns                <- dims(ISH@stock.n)
rec.years <- ((ISH@range['maxyear']-11):(ISH@range['maxyear']-2));
gm.recs  <- exp(mean(log(rec(ISH)[,as.character(rec.years)])));
ISH.srr <- list(model="geomean",params=FLPar(gm.recs));

#Expand stock object
ISH.proj <- stf(ISH,nyears=4,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE);
#ISH.proj@stock.n[-7,ac(ImY)]   <- FLQuant(c(gm.recs,stock.n(ISH)[,ac(ISH@range['maxyear'])] * exp(-harvest(ISH[,ac(ISH@range['maxyear'])])-m(ISH[,ac(2012)]))),
#                               dimnames=list(ages=dmns$min:(dmns$max+1),year=ISH@range['maxyear']+1,unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter))
#
survivors           <- FLQuant(c(gm.recs,stock.n(ISH)[,ac(ISH@range['maxyear'])] * exp(-harvest(ISH[,ac(ISH@range['maxyear'])])-m(ISH[,ac(TaY)]))),
                               dimnames=list(ages=dmns$min:(dmns$max+1),year=ISH@range['maxyear']+1,unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter))

## plusgroup
survivors[ac(dmns$max),ac(ISH@range['maxyear']+1)] <- quantSums(survivors[ac(dmns$max:(dmns$max+1)),ac(ISH@range['maxyear']+1)])
survivors           <- survivors[ac(dmns$min:dmns$max),]


#ISH.proj@stock.n[-1,ac(ImY)]  <- setPlusGroup(ISH@stock.n[,ac(TaY)] * exp(-ISH@harvest[,ac(TaY)] - ISH@m[,ac(TaY)]),7)

ISH.proj@stock.n[,ac(ImY)]<-survivors
ISH.proj@stock.n[1,as.character(c(ImY,AdY,CtY))] <- gm.recs;

#Define some constants 2011
#ImY.catch <- 16196;
#AdY.catch <- 13200;
#numFmsy <- 0.25;

#For 2017
ImY.catch <- 7016;  
#2017 real advice year
numFmsy <- 0.266;
numFlim <- 0.397;
numFpa <- 0.286;

#Setup options

options.l <- list("Catch = Fmsy"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,numFmsy,numFmsy))),
                  #Zero catch
                  "Catch = Zero"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity="catch",
                                          val=c(ImY.catch,0,0))),
                  # TAC, -15% 
                  "Catch = TAC -15%"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,ImY.catch*0.85,NA))),
                  #TAC sq
                  "Catch = TAC sq"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,ImY.catch*1,NA))),
                  #TAC +15%
                  "Catch = TAC +15%"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,ImY.catch*1.15,NA))), 
                  #Fsq
                  "F = F sq"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,fbar(ISH)[,ac(TaY)],fbar(ISH)[,ac(TaY)]))),
                  #Flim
                  "F = Flim"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,numFlim,numFlim))),
                  #Fpa
                  "F = Fpa"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,numFpa,numFpa))),
                  
                  
                  "SSB = Bpa"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","ssb","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,11831,NA))),
                  
                  "SSB = Blim"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","ssb","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,8500,NA))),
                  "SSB = Btrigger"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","ssb","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,11831,NA))),
                  
                  
                  #TAC +15%
                  "Catch = TAC +15%"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,ImY.catch*1.15,NA)))) #End options list
#End options list



#Multi-options table
fmult.targs  <- seq(0,2,by=0.1)
mult.opts.l <- lapply(as.list(fmult.targs),function(fmult) {
  fwdControl(data.frame(year=c(ImY,AdY,CtY),
                        quantity=c("catch","f","f"),
                        rel=c(NA,ImY,AdY),
                        val=c(ImY.catch,fmult,NA)))
})
names(mult.opts.l) <- sprintf("Fmult(2013) = %4.3f",fmult.targs)

#Calculate options
ISH.options   <- lapply(options.l,function(ctrl) {fwd(ISH.proj,ctrl=ctrl,sr=ISH.srr)})
ISH.mult.opts <- lapply(mult.opts.l,function(ctrl) {fwd(ISH.proj,ctrl=ctrl,sr=ISH.srr)})


### ======================================================================================================
### Write Options Tables
### ======================================================================================================
#FnPrint("WRITING OPTIONS TABLES...\n")
output.base <- "C:\\Work\\HAWG2018\\SAM\\results\\"

#Document input settings
input.tbl.file <-paste(output.base,"options - input.csv",sep="")
write.table(NULL,file=input.tbl.file,col.names=FALSE,row.names=FALSE)
input.tbl.list <- list(N="stock.n",M="m",Mat="mat",PF="harvest.spwn",
                       PM="m.spwn",SWt="stock.wt",Sel="harvest",CWt="catch.wt")
for(yr in c(ImY,AdY,CtY)){
  col.dat <- sapply(input.tbl.list,function(slt) slot(ISH.proj,slt)[,as.character(yr),drop=TRUE])
  write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
  write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

#Detailed options table
options.file <-paste(output.base,"options - details.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(ISH.options)) {
  opt <- names(ISH.options)[i]
  stk <- ISH.options[[opt]]
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
opt.sum.tbl(stcks=ISH.options,fname=paste(output.base,"options - summary.csv",sep="."))
opt.sum.tbl(stcks=ISH.mult.opts,fname=paste(output.base,"multi-options - summary.csv",sep="."))

