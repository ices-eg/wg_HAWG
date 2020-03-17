##R 3.2.0

library(FLCore)
library(FLSAM)


rm(list=ls())
#C:\Users\Matt Lundy\Documents\GIT_HUB\wg_HAWG\IrishSea\UpdateAssessment\SAM
#- Set paths
my.path<-file.path("C:","Users","Matt Lundy","Documents", "GIT_HUB", "wg_HAWG", "IrishSea","UpdateAssessment","SAM")
output.dir              <- file.path(my.path,"results")
data.source         <- file.path(my.path,"data")    #Data source, not code or package source!!!

#- Setup stock, tun and control
scenario<- list(); scenario$trimTS   <- T
source(file.path(my.path,"setupStockTunIII.r"))
source(file.path(my.path,"setupControl.r"))
ISH.ctrl@logN.vars[] <- 1
ISH.ctrl@f.vars["catch",] <- c(1,1,2,2,2,3,4,4)
ISH.ctrl@obs.vars["catch",] <- c(1,2,2,2,3,3,3,3)
ISH.ctrl@obs.vars["AC(VIIaN)",] <- c(4,rep(5,4),rep(6,3))

#- Run final model
#only need the run dir setup below if sam.exe name is longer than 4 characters
ISH.ctrl@sam.binary <- file.path(my.path,"sam.exe")
ISH.sam       <- FLSAM(ISH,ISH.tun,ISH.ctrl)
ISH.sam       <- SAM2FLR(ctrl=ISH.ctrl,admb.stem="sam")

#save results
name(ISH.sam) <- "ISH_assessment 2020"
ISH@stock.n <- ISH.sam@stock.n
ISH@harvest <- ISH.sam@harvest

save(ISH,ISH,ISH.tun,ISH.ctrl,ISH.sam,file=file.path(output.dir,paste(name(ISH.sam),".RData",sep="")))

obs.var(ISH.sam)
plot(ISH.sam)
ISH.sam@nlogl
fbar(ISH.sam)$value
ssb(ISH.sam)$value
##############
##############
##############

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
