################################################################################
# NSH_SAM Assessment
#
# $Rev: 697 $
# $Date: 2012-02-10 09:52:28 +0100 (vr, 10 feb 2012) $
#
# Author: HAWG model devlopment group
#
# Performs the "Final" assessment for NSAS assessment 
#
# Developed with:
#   - R version 2.13.0
#   - FLCore 2.4
#
# To be done:
#
# Notes: Have fun running this assessment!
#
################################################################################

### ============================================================================
### ============================================================================
### ============================================================================
### Setup
### ============================================================================
### ============================================================================
### ============================================================================

rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nNSH Final Assessment\n=====================\n")

# local path

path <- "C:/Users/brune001/my git files/wg_HAWG/NSAS/benchmark/"



try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
output.dir          <-  file.path(".","results/1_newFprop/")                #figures directory
output.base         <-  file.path(output.dir,"NSH Assessment")  #Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  10                                      #Number of years for which to run the retrospective
.libPaths("C:/software/Rpackages")


### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA); library(FLBRP)
source(file.path("R/1_newFprop/setupAssessmentObjects_newFprop.r"))
source(file.path("R/1_newFprop/setupControlObject_newFprop.r"))
path <- "C:/Users/brune001/my git files/wg_HAWG/NSAS/"
try(setwd(path),silent=TRUE)
source(file.path("../_Common/HAWG_Common_module.r"))
source(file.path("retroResidual.R"))
source(file.path("retro_param.R"))
path <- "C:/Users/brune001/my git files/wg_HAWG/NSAS/benchmark/"
try(setwd(path),silent=TRUE)

### ============================================================================
### ============================================================================
### ============================================================================
### Run the assessment
### ============================================================================
### ============================================================================
### ============================================================================

#Perform assessment
NSH.sam <- FLSAM(NSH,NSH.tun,NSH.ctrl)
name(NSH.sam) <- "newFprop"

#Update stock object
NSH       <- NSH + NSH.sam
NSH@stock <- computeStock(NSH)
name(NSH) <- "newFprop"

# Save results
save(NSH,NSH.tun,NSH.ctrl,NSH.sam,file=file.path(output.dir,"NSH.RData",sep=""))

### ============================================================================
### ============================================================================
### ============================================================================
### Plots
### ============================================================================
### ============================================================================
### ============================================================================

#Setup plots
pdf(file.path(output.dir,paste(name(NSH.sam),".pdf",sep="")))
png(file.path(output.dir,paste(name(NSH.sam),"figures - %02d.png")),units = "px", height=800,width=672, bg = "white")

### ============================================================================
### Input data
### ============================================================================
#
##figure - TACs and catches
#TACs          <- read.csv(file.path(".","data","historic data","TAC-historic.csv"))
#TAC.plot.dat  <- data.frame(year=rep(TACs$year,each=2)+c(-0.5,0.5),TAC=rep(rowSums(TACs[,c("Agreed_A","Bycatch_B")],na.rm=T),each=2))
#catch         <- as.data.frame(NSH@catch[,ac(TACs$year)]/1e3)
#plot(0,0,pch=NA,xlab="Year",ylab="Catch",xlim=range(c(catch$year,TAC.plot.dat$year)),ylim=range(c(0,TAC.plot.dat$TAC,catch$data)),cex.lab=1.2,cex.axis=1.1,font=2)
#rect(catch$year-0.5,0,catch$year+0.5,catch$data,col="grey")
#lines(TAC.plot.dat,lwd=3)
#legend("topright",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",pt.bg="grey",pt.cex=c(2),box.lty=0)
#box()
#title(main=paste(NSH@name,"Catch and TAC"))
#

## === Plot the overlay of tuning series ===
## figure - plot of time series: HERAS, IBTS0, IBTSQ1 by cohort.
#print(overlayTimeseries(lapply(NSH.tun,index),nyrs=20,ages=0:1))
## figure - plot of time series: IBTS0, IBTSQ1 by cohort.
#print(overlayTimeseries(FLQuants(IBTS0=NSH.tun[["IBTS0"]]@index,IBTSQ1=NSH.tun[["IBTS-Q1"]]@index),nyrs=20,ages=0:1))
#  
## figure - time series of all data by age
#print(surveyTimeseries(NSH.tun))
#
## figure - internal consistency HERAS, plot survey index versus each other
#tmp.tun <- NSH.tun; dmns <- dimnames(tmp.tun[["IBTS0"]]@index); tmp.tun[["IBTS0"]] <- FLIndex(FLQuant(NA,dimnames=list(age=1,year=ac(an(dmns$year)+1),unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter)))
#tmp.tun[["IBTS0"]]@index <- FLQuant(NSH.tun[["IBTS0"]]@index@.Data,dimnames=list(age=1,year=ac(an(dmns$year)+1),unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter))
##plot(tmp.tun,type="pairwise")
#plot(NSH.tun[["HERAS"]],type="internal")
#
## === Plot the proportion of catch and weight in numbers and weight to see if the catch is representative for the stock build-up ===
## figure - catch number at age
#print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@catch.n)),groups="age",main="Proportion of Catch numbers at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
## figure - proportion of stock weight at age
#print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@stock.wt)),groups="age",main="Proportion of Stock weight at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
## figure - proportion of catch weight at age
#print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@catch.wt)),groups="age",main="Proportion of Catch weight at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
#
## figure - acoustic index at age
#print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH.tun[["HERAS"]]@index)),groups="age",main="Proportion of Acoustic index at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
#
## figure - proportion of natural mortality at age
#  print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@m)),groups="age",main="Proportion of natural mortality at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
#
## === Plot the time series of weight in the stock and catch in the stock ===
## figure - times series for each age, stock
#timeseries(window(NSH,1975,range(NSH)["maxyear"]),slot="stock.wt")
## figure - times series for each age, catches
#timeseries(window(NSH,1975,range(NSH)["maxyear"]),slot="catch.wt")
## figure - times series for each age, harvest
#timeseries(window(NSH,2000,range(NSH)["maxyear"]),slot="harvest")
## figure - times series for each age, maturity
#timeseries(window(NSH,1990,range(NSH)["maxyear"]),slot="mat")
## figure - times series for each age, mortality
#timeseries(window(NSH,1947,range(NSH)["maxyear"]),slot="m")
#
## === Plot the time series of the surveys ===
## figure - times series, SCAI
#timeseries(NSH.tun[["SCAI"]],slot="index")
## figure - times series, IBTSQ1
#timeseries(NSH.tun[["IBTS-Q1"]],slot="index")
## figure - times series, IBTS0
#timeseries(NSH.tun[["IBTS0"]],slot="index")
#
## figure - times series, weight in stock following each cohort
#west.by.cohort      <- as.data.frame(FLCohort(window(NSH@stock.wt,1980,range(NSH)["maxyear"])))
#west.by.cohort      <- subset(west.by.cohort,!is.na(west.by.cohort$data))
#west.by.cohort$year <- west.by.cohort$age + west.by.cohort$cohort
#west.cohort.plot    <- xyplot(data~year,data=west.by.cohort,
#                              groups=cohort,
#                              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
#                              type="b",
#                              xlab="Year",ylab="Weight in the stock (kg)",
#                              main=paste(NSH@name,"Weight in the stock by cohort"),
#                              par.settings=list(superpose.symbol=list(pch=as.character(unique(west.by.cohort$cohort)%%10),cex=1.25)),
#                              panel=function(...) {
#                                panel.grid(h=-1,v=-1)
#                                panel.xyplot(...)
#                              })
#print(west.cohort.plot)
#
### ============================================================================
### Model fit
### ============================================================================

# figure - residual plots at each age for each time series
residual.diagnostics(NSH.sam)

# figure - proportion of harvest pressure at age
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@harvest)),groups="age",main="Proportion of harvest pressure at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
# figure - proportion of stock numbers pressure at age
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@stock.n)),groups="age",main="Proportion of Stock numbers at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

# figure - assessment result, spawning stock biomass, fishing mortality, recruitment
print(plot(NSH.sam,futureYrs=F))

# figure - uncertainties as a function of time
CV.yrs <- ssb(NSH.sam)$year
CV.dat <- cbind(SSB=ssb(NSH.sam)$CV,
                Fbar=fbar(NSH.sam)$CV,Rec=rec(NSH.sam)$CV)
matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
        xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")

# figure - catchabilities at age from HERAS
catch <- catchabilities(NSH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
             scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
             type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
             subset=fleet %in% c("HERAS"),
             main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

# figure - variance by data source
obv <- obs.var(NSH.sam)
obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
obv <- obv[order(obv$value),]
bp <- barplot(obv$value,ylab="Observation Variance",
              main="Observation variances by data source",col=factor(obv$fleet))
axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

# figure - variance vs uncertainty for each data source
plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

# figure - fishing age selectivity per year
sel.pat <- merge(f(NSH.sam),fbar(NSH.sam),
               by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
print(xyplot(sel ~ age|sprintf("%i's",floor((year+2)/5)*5),sel.pat,
             groups=year,type="l",as.table=TRUE,
             scale=list(alternating=FALSE),
             main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar"))

# figure - correlation matrix of model parameters
cor.plot(NSH.sam)

# figure - catch residuals per year per age
dat <- subset(residuals(NSH.sam),fleet=="catch")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year Catch",
panel=function(...){
    lst <- list(...)
    panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
})

# figure - acosutic index residuals per year per age  
dat <- subset(residuals(NSH.sam),fleet=="HERAS")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year HERAS",
panel=function(...){
    lst <- list(...)
    panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
})


# figure - otholith. Warning, this takes very long!
otolith(NSH.sam,n=1000)
#
#### ============================================================================
#### Management
#### ============================================================================
#
## figure - fishing mortality vs SSB, management plan
#plot(x=c(0,0.8,1.5,2.6),y=c(0.1,0.1,0.26,0.26),type="l",ylim=c(0,0.4),lwd=2,xlab="SSB in million tonnes",ylab="Fbar",cex.lab=1.3,main="Management plan North Sea Herring")
#abline(v=0.8,col="red",lwd=2,lty=2)
#abline(v=1.0,col="blue",lwd=2,lty=2)
#abline(v=1.5,col="darkgreen",lwd=2,lty=2)
#text(0.8,0,labels=expression(B[lim]),col="red",cex=1.3,pos=2)
#text(1.0,0,labels=expression(B[pa]),col="blue",cex=1.3,pos=2)
#text(1.5,0,labels=expression(B[trigger]),col="darkgreen",cex=1.3,pos=4)
#
#points(y=fbar(NSH[,ac(2005:2016)]), x=(ssb(NSH[,ac(2005:2016)])/1e6),pch=19)
#lines(y=fbar(NSH[,ac(2005:2016)]),  x=(ssb(NSH[,ac(2005:2016)])/1e6))
#text(y=fbar(NSH[,ac(2005:2016)]),   x=(ssb(NSH[,ac(2005:2016)])/1e6),labels=ac(2005:2016),pos=3,cex=0.7)
##
#### ============================================================================
#### Reference points
#### ============================================================================
#library(FLBRP)
#ref. <- brp(FLBRP(NSH,fbar=seq(0,1,length.out=101),nyears=3))
#print(refpts(ref.))
#
#NSH.SRR <- FLSR(
#	rec = rec(NSH)[,ac((range(NSH)["minyear"]+1): range(NSH)["maxyear"])],
#	ssb = ssb(NSH)[,ac((range(NSH)["minyear"])  :(range(NSH)["maxyear"]-1))],
#	model='segreg')
#NSH.SRR <- fmle(NSH.SRR)
#plot(NSH.SRR)
#
## figure - recruitment vs SSB
#newData <- predict(NSH.SRR,ssb=FLQuant(seq(0,max(ssb(NSH)),length.out=200)))
#yrange  <- range(pretty(c(0,range(rec(NSH)))))/1e6; xrange <- range(pretty(c(0,range(ssb(NSH)))))/1e6
#plot(y=newData/1e6,x=seq(0,max(ssb(NSH)),length.out=200)/1e6,type="l",lwd=2,
#     xlab="SSB (million tonnes)",ylab="Recruitment (billions)",xlim=xrange,ylim=yrange,
#     las=1,cex.lab=1.3,cex.axis=1.1,xaxs="i",yaxs="i")
#points(y=rec(NSH)/1e6,x=ssb(NSH)/1e6)
#
# closing pdf and png print
dev.off()
dev.off()

### ============================================================================
### ============================================================================
### ============================================================================
### Document Assessment
### ============================================================================
### ============================================================================
### ============================================================================
#
#log.msg("GENERATING DOCUMENTATION...\n")
##Document the run with alternative table numbering and a reduced width
old.opt           <- options("width","scipen")
options("width"=75,"scipen"=1000)
#
##2013 fix
NSH.sam@control@sam.binary <- "character()"
sam.out.file      <- FLSAM.out(NSH,NSH.tun,NSH.sam,format="TABLE 2.6.3.%i North Sea Herring.")
write(sam.out.file,file=paste(output.base,"sam.out",sep="."))
options("width"=old.opt$width,"scipen"=old.opt$scipen)
#
##And finally, write the results out in the lowestoft VPA format for further analysis
writeFLStock(NSH,output.file=file.path(output.dir,"NSAS_47d3_"))
writeFLStock(NSH,file.path(output.dir,"hawg_her-47d3.ypr"),type="YPR")
writeFLStock(wbss,file.path(output.dir,"hawg_her-IIIa.ypr"),type="YPR")
##Prepare standard graph table
#NSH.brp <- brp(FLBRP(NSH,sr=NSH.SRR,fbar=seq(0,1,length.out=100),refpts=refpts()))
## Calculate the spawners in number
#spawners                          <- colSums(NSH.brp@stock.n * sweep(exp(sweep(-sweep(NSH.brp@harvest,c(1,3:6),NSH.brp@harvest.spwn,"*"),
#                                             c(1,3:6),NSH.brp@m*NSH.brp@m.spwn,"-")),c(1,3:6),NSH.brp@mat,"*"))
## Put all the standard input in a dataframe in columns
#standardGraphTable                <- cbind(NSH.brp@fbar,yield(NSH.brp),ssb(NSH.brp),rec(NSH.brp),yield(NSH.brp)/rec(NSH.brp),
#                                           ssb(NSH.brp)/rec(NSH.brp),spawners,landings(NSH.brp))
#standardGraphTable                <- data.frame(standardGraphTable)
#colnames(standardGraphTable)      <- c("Fbar","Yield","SSB","Recruits","Yield.Recruit","SSB.Recruit","Spawners","Landings")
## Round some values
#standardGraphTable$Fbar           <- round(an(ac(standardGraphTable$Fbar)),3)
#standardGraphTable$Yield          <- round(an(ac(standardGraphTable$Yield)))
#standardGraphTable$SSB            <- round(an(ac(standardGraphTable$SSB)))
#standardGraphTable$Recruits       <- round(an(ac(standardGraphTable$Recruits)))
#standardGraphTable$Yield.Recruit  <- round(an(ac(standardGraphTable$Yield.Recruit)),4)
#standardGraphTable$SSB.Recruit    <- round(an(ac(standardGraphTable$SSB.Recruit)),3)
#standardGraphTable$Spawners       <- round(an(ac(standardGraphTable$Spawners)))
#standardGraphTable$Landings       <- round(an(ac(standardGraphTable$Landings)))
#standardGraphTable                <- rbind(c(paste("Ages ",range(stck.)["minfbar"],"-",range(stck.)["maxfbar"],sep=""),
#                                           "Tonnes","Tonnes","Number","","","Number","Tonnes"),standardGraphTable)
## Write the standard graph to file and the reference points as well
#write.table(standardGraphTable,file=file.path(output.dir,"standardGraphTable.csv"),col.names=T,row.names=F,sep=",")
#
stockSummaryTable <- cbind(rec(NSH.sam)$year,
                           rec(NSH.sam)$value,      rec(NSH.sam)$lbnd,    rec(NSH.sam)$ubnd,
                           tsb(NSH.sam)$value,      tsb(NSH.sam)$lbnd,    tsb(NSH.sam)$ubnd,
                           ssb(NSH.sam)$value,      ssb(NSH.sam)$lbnd,    ssb(NSH.sam)$ubnd,
                           catch(NSH.sam)$value,    catch(NSH.sam)$lbnd,  catch(NSH.sam)$ubnd,
                           catch(NSH.sam)$value / ssb(NSH.sam)$value, catch(NSH.sam)$lbnd / ssb(NSH.sam)$lbnd, catch(NSH.sam)$ubnd / ssb(NSH.sam)$ubnd,
                           fbar(NSH.sam)$value,     fbar(NSH.sam)$lbnd,   fbar(NSH.sam)$ubnd,
                           c(quantMeans(harvest(NSH.sam)[ac(0:1),])),
                           c(sop(NSH),NA),
                           c(catch(NSH),NA))
colnames(stockSummaryTable) <-
                     c("Year",paste(rep(c("Recruits Age 0 (Thousands)","Total biomass (tonnes)","Spawing biomass (tonnes)",
                       "Landings (tonnes)","Yield / SSB (ratio)","Mean F ages 2-6"),each=3),c("Mean","Low","High")),"Mean F ages 0-1","SoP (%)","WG Catch")
stockSummaryTable[nrow(stockSummaryTable),] <- NA
stockSummaryTable[nrow(stockSummaryTable),"Spawing biomass (tonnes) Mean"] <- 2271364
stockSummaryTable[nrow(stockSummaryTable),2:4] <- c(rec(NSH.sam)$value[nrow(rec(NSH.sam))],rec(NSH.sam)$lbnd[nrow(rec(NSH.sam))],rec(NSH.sam)$ubnd[nrow(rec(NSH.sam))])
write.csv(stockSummaryTable,file=file.path(output.dir,paste(name(NSH),"stockSummaryTable.csv",sep="_")))
options("width"=old.opt$width,"scipen"=old.opt$scipen)

### ============================================================================
### Finish
### ============================================================================
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))

### ============================================================================
### ============================================================================
### ============================================================================
### run retrospective - takes time!!
### ============================================================================
### ============================================================================
### ============================================================================

### ============================================================================
### run retrospective
### ============================================================================
NSH.retro <- retro(NSH,NSH.tun,NSH.ctrl,retro=3)
save(NSH.retro,file=file.path(output.dir,"NSHretro.RData",sep=""))

### ============================================================================
### figures
### ============================================================================
#Setup plots
pdf(file.path(output.dir,paste(name(NSH.sam),"_retro.pdf",sep="")))
png(file.path(output.dir,paste(name(NSH.sam),"figures_retro - %02d.png")),units = "px", height=800,width=672, bg = "white")

# figure - retrospective for SSB, Fbar, rec
print(plot(NSH.retro,futureYrs=F))

#print(retroResiduals(   NSH.retro,"HERAS",2000:2014))
#print(retroResiduals(   NSH.retro,"IBTS Q1",1991:2014))
#print(retroResiduals(   NSH.retro,"IBTS Q3",1991:2014))
#print(retroResiduals(   NSH.retro,"catch",1991:2014))
#print(retroSelectivity( NSH.retro,2003:2014))

#print(lapply(NSH.retro,retroParam))
# closing pdf and png print
dev.off()
dev.off()