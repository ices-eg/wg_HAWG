################################################################################
# WBSS SAM assessment in FLR
#
# 15 March 2017
# HAWG working group
#
################################################################################

rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nWBSS Final Assessment\n=====================\n")

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
output.dir          <-  file.path(".","results")                #Output directory
output.base         <-  file.path(output.dir,"WBSS Assessment") #Output base filename, including directory. Other output filenames are built by appending onto this one
#n.retro.years       <-  5                                       #Number of years for which to run the retrospective


### ============================================================================
### Setup assessment
### ============================================================================
#Import externals
library(FLSAM); library(FLEDA)
source(file.path("setupAssessmentObjects.R"))
## source(file.path("setupControlObject.R"))
## source(file.path("setupControlObject04.R"))
source(file.path("setupControlObject_warmUp.R"))
source(file.path("..","_Common","HAWG_Common_module.r"))
source(file.path("..","_Common","plot_stock_age_distribution.R"))
source(file.path("retro_param.R"))

## preliminary run to get initial values
stk.sam.prel <- FLSAM(stk, wbss.tun, stk.ctrl)
rm(stk.ctrl)

### ============================================================================
### Run the assessment
### ============================================================================

# Perform assessment
source(file.path("setupControlObject.R"))
stk.sam <- FLSAM(stk, wbss.tun, stk.ctrl, pin.sam=stk.sam.prel)


name(stk.sam) <- "WBSS herring"

# Update stock obj
stk <- stk + stk.sam

wbss <- stk
wbss.sam <- stk.sam
wbss.ctrl <- stk.ctrl

# Save results
save.image(file.path(output.dir,paste("WBSS.RData",sep="")))


### ============================================================================
### Plots
### ============================================================================
#Setup plots
## library(Cairo)
## png(paste(output.base,"figures - %02d.png"),units = "px", height=500,width=600,pointsize = 24, bg = "white")
png(paste(output.base,"figures - %02d.png"),units = "px", height=500,width=550,pointsize = 12, bg = "white")
## Cairo(paste(output.base,"figures - %02d.png"),units = "px", height=500,width=550,pointsize = 12, bg = "white")
#pdf(file.path(output.dir,paste("WBSS_sam.pdf",sep="")))

  ### ============================================================================
  ### Input data
  ### ============================================================================

#Catch and TAC
catch.dat <- read.csv(file.path(".","data","Herring catches by area.csv"))
catch.dat$data <- catch.dat$data/1000
xlims <- range(pretty(catch.dat$year))
par(mfrow=c(3,1),mar=c(0,0,1,0),oma=c(5,4,4,2))
plot.dat <- subset(catch.dat,area=="Div. IIIa")
IIIa.catches <- tapply(plot.dat[plot.dat$quantity=="Catch","data"],plot.dat[plot.dat$quantity=="Catch","year"],sum)
IIIa.catches <- data.frame(year=as.numeric(names(IIIa.catches)),data=IIIa.catches)
plot(0,0,pch=NA,xaxt="n",xlim=xlims,ylim=range(pretty(c(0,IIIa.catches$data))),ylab="Catch (kt)",xlab="",xpd=NA)
grid()
rect(IIIa.catches$year-0.5,0,IIIa.catches$year+0.5,IIIa.catches$data,col="lightgrey")
plot.dat <- subset(catch.dat,area=="Div. IIIa"&stock=="WBSS")
rect(plot.dat$year-0.5,0,plot.dat$year+0.5,plot.dat$data,col="darkgrey")
IIIa.TACs <- subset(catch.dat, area=="Div. IIIa"&quantity=="TAC")
plot.dat <- data.frame(year=rep(IIIa.TACs$year,each=2)+c(-0.5,0.5),TAC=rep(IIIa.TACs$data,each=2))
lines(plot.dat,lwd=5)
legend("topright",legend=c("WBSS Catch in IIIa","NSAS Catch in IIIa","Div. IIIa TAC"),lwd=c(1,1,5),lty=c(NA,NA,1),pch=c(22,22,NA),
col="black",pt.bg=c("darkgrey","lightgrey",NA),pt.cex=c(2),bg="white")
axis(1,labels=FALSE)

plot.dat <- subset(catch.dat,area=="Sub-div. 22-24" & quantity=="Catch")
plot(0,0,pch=NA,xaxt="n",xlim=xlims,ylim=range(pretty(c(0,plot.dat$data))),ylab="Catch (kt)",xlab="",xpd=NA)
grid()
rect(plot.dat$year-0.5,0,plot.dat$year+0.5,plot.dat$data,col="darkgrey")
sd22.TACs <- subset(catch.dat,area=="Sub-div. 22-24" & quantity=="TAC")
plot.dat <- data.frame(year=rep(sd22.TACs$year,each=2)+c(-0.5,0.5),TAC=rep(sd22.TACs$data,each=2))
lines(plot.dat,lwd=5)
legend("topright",legend=c("WBSS Catch in SD. 22-24","SD. 22-24 TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),
col="black",pt.bg=c("darkgrey",NA),pt.cex=c(2),bg="white")
axis(1,labels=FALSE)

plot.dat <- subset(catch.dat,quantity=="Catch" & stock=="WBSS")
WBSS.catches <- tapply(plot.dat[plot.dat$quantity=="Catch","data"],plot.dat[plot.dat$quantity=="Catch","year"],sum)
WBSS.catches <- data.frame(year=as.numeric(names(WBSS.catches)),data=WBSS.catches)
plot(0,0,pch=NA,xaxt="n",xlim=xlims,ylim=range(pretty(c(0,WBSS.catches$data))),ylab="Catch (kt)",xlab="",xpd=NA)
grid()
rect(WBSS.catches$year-0.5,0,WBSS.catches$year+0.5,WBSS.catches$data,col="darkgrey")
legend("topright",legend=c("Total WBSS Catch"),pch=c(22,NA),pt.bg=c("darkgrey"),pt.cex=c(2),bg="white")
axis(1,labels=TRUE)

## title(main=paste(wbss@name,"Catch and TAC"),outer=TRUE)
title(xlab="Year",xpd=NA)



# Plot the mature and immature part of the stock
  print(mat.immat.ratio(wbss))
  
  # Plot the overlay of tuning series
  print(overlayTimeseries(lapply(wbss.tun,index),nyrs=10,ages=0:1))
  
  # Plot the overlay by year and age
  print(surveyTimeseries(wbss.tun))

  ## #Plot survey index versus each other
  ##   tmp.tun <- wbss.tun; dmns <- dimnames(tmp.tun[["IBTS0"]]@index); tmp.tun[["IBTS0"]] <- FLIndex(FLQuant(NA,dimnames=list(age=1,year=ac(an(dmns$year)+1),unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter)))
  ##   tmp.tun[["IBTS0"]]@index <- FLQuant(wbss.tun[["IBTS0"]]@index@.Data,dimnames=list(age=1,year=ac(an(dmns$year)+1),unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter))
  ## #plot(tmp.tun,type="pairwise")
  ## plot(wbss.tun[["HERAS"]],type="internal")

  # Plot the proportion of catch and weight in numbers and weight to see if the catch is representative for the stock build-up
  print(stacked.area.plot(data~year| unit, as.data.frame(pay(wbss@catch.n)),groups="age",main="Proportion of Catch numbers at age",ylim=c(-0.01,1.01),xlab="years",col=gray(8:0/8)))
  print(stacked.area.plot(data~year| unit, as.data.frame(pay(wbss@stock.wt)),groups="age",main="Proportion of Stock weight at age",ylim=c(-0.01,1.01),xlab="years",col=gray(8:0/8)))
  print(stacked.area.plot(data~year| unit, as.data.frame(pay(wbss@catch.wt)),groups="age",main="Proportion of Catch weight at age",ylim=c(-0.01,1.01),xlab="years",col=gray(8:0/8)))

  # Plot the proportion of catch in numbers in the indices to see if the indices are having specific yearclass trends
  print(stacked.area.plot(data~year| unit, as.data.frame(pay(wbss.tun[["HERAS"]]@index)),groups="age",main="Proportion of Acoustic index at age",ylim=c(-0.01,1.01),xlab="years",col=gray(8:0/8)))

  # Plot the proportion of natural mortality
  print(stacked.area.plot(data~year| unit, as.data.frame(pay(wbss@m)),groups="age",main="Proportion of natural at age",ylim=c(-0.01,1.01),xlab="years",col=gray(8:0/8)))

  # Plot the time series of weight in the stock and catch in the stock
  timeseries(window(wbss,1991,range(wbss)["maxyear"]),slot="stock.wt")
  timeseries(window(wbss,1991,range(wbss)["maxyear"]),slot="catch.wt")
  timeseries(window(wbss,1991,range(wbss)["maxyear"]),slot="harvest")
  timeseries(window(wbss,1991,range(wbss)["maxyear"]),slot="mat")
  timeseries(window(wbss,1991,range(wbss)["maxyear"]),slot="m")

  # Plot the time series of the surveys
  timeseries(wbss.tun[["HERAS"]],slot="index")
  timeseries(wbss.tun[["GerAS"]],slot="index")
  timeseries(wbss.tun[["N20"]],slot="index")
  timeseries(wbss.tun[["IBTS Q1"]],slot="index")
  timeseries(wbss.tun[["IBTS Q3"]],slot="index")

  #Time series of west by cohort
  west.by.cohort      <- as.data.frame(FLCohort(window(wbss@stock.wt,1991,range(wbss)["maxyear"])))
  west.by.cohort      <- subset(west.by.cohort,!is.na(west.by.cohort$data))
  west.by.cohort$year <- west.by.cohort$age + west.by.cohort$cohort
  west.cohort.plot    <- xyplot(data~year,data=west.by.cohort,
                                groups=cohort,
                                auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
                                type="b",
                                xlab="Year",ylab="Weight in the stock (kg)",
                                main=paste(wbss@name,"Weight in the stock by cohort"),
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
  residual.diagnostics(wbss.sam)

  # Plot the harvest pattern at age as a proportion over time & the stock.n as proportion over time
  print(stacked.area.plot(data~year| unit, as.data.frame(pay(wbss@harvest)),groups="age",main="Proportion of harvest pressure at age",ylim=c(-0.01,1.01),xlab="years",col=gray(8:0/8)))
  print(stacked.area.plot(data~year| unit, as.data.frame(pay(wbss@stock.n)),groups="age",main="Proportion of Stock numbers at age",ylim=c(-0.01,1.01),xlab="years",col=gray(8:0/8)))

  #Plot result
  print(plot(wbss.sam))

  #Plot uncertainties as a function of time
  CV.yrs <- ssb(wbss.sam)$year
  CV.dat <- cbind(SSB=ssb(wbss.sam)$CV,
                     Fbar=fbar(wbss.sam)$CV,Rec=rec(wbss.sam)$CV)
  par(mfrow=c(1,1))
  matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
      xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
  legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")

  #Plot obs_variance (weightings)
  obv <- obs.var(wbss.sam)
  obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
  obv <- obv[order(obv$value),]
  par(mfrow=c(1,1))
  bp <- barplot(obv$value,ylab="Observation Variance",
         main="Observation variances by data source",col=factor(obv$fleet))
  axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
  legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

  par(mfrow=c(1,1))
  plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
    pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
  text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

  #Plot catchabilities values
  catch <- catchabilities(wbss.sam)
  print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
            scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
            type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
            subset=fleet %in% c("N20") ==FALSE,
            main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

  #Plot fishery selectivity pattern over time
  sel.pat <- merge(f(wbss.sam),fbar(wbss.sam),
               by="year",suffixes=c(".f",".fbar"))
  sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
  sel.pat$age <- as.numeric(as.character(sel.pat$age))
  print(xyplot(sel ~ age|sprintf("%i's",floor((year)/5)*5),sel.pat,
           groups=year,type="l",as.table=TRUE,
           scale=list(alternating=FALSE),
           main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar"))

  print(xyplot(sel ~ age|as.factor(year),sel.pat[sel.pat$year>2010,],
           groups=year,type="b",as.table=TRUE,
           scale=list(alternating=FALSE),
           main="Selectivity of the Fishery by Year",xlab="Age",ylab="F/Fbar"))

  #Plot correlation matrix
  cor.plot(wbss.sam)

  #Plot otholith  ***can be time consuming!!!
  otolith(wbss.sam, 2016, n=10000)

  # Taylor diagram
  #taylor.diagram(wbss,common.basis=FALSE)

  # Plot the estimated age distribution following cohorts
  adist.plot(stk=wbss, xlab="Year", ylab="Number of fish")

  # Plot params estimates with uncertainty
  par(mfrow=c(1,1), mai=rep(0.3,4), omi=rep(0.1,4))
  print(lapply(FLSAMs(wbss.sam),retroParam))

  # S-R scatter plot
  x <- ssb(wbss.sam)$value ; y <- rec(wbss.sam)$value ; yr <- ssb(wbss.sam)$year
  par(mfrow=c(1,1))
  plot(x[-length(x)], y[-length(x)], pch=19, xlim=c(0,max(x)), ylim=c(0,max(y)), xlab="SSB", ylab="Recruits")
  points(x[length(x)], y[length(x)], pch=4, cex=1.5, lwd=2)
  legend("bottomright", c(paste(yr[1],"-",yr[length(yr)-1]),yr[length(yr)]), pch=c(19,4))

# bubble plot std.residuals
  res <- residuals(wbss.sam)
  res <- tapply(res$std.res, list(age=res$age, year=res$year, unit=res$fleet), mean)
  res <- FLQuant(res, dimnames=dimnames(res))
  bubbles(age~year|unit, data=res, bub.scale=4, col=c("black","red"))



### ======================================================================================================
### Document Assessment
### ======================================================================================================
log.msg("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt           <- options("width","scipen")
options("width"=old.opt$width,"scipen"=old.opt$scipen)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(wbss,output.file=output.base)

#write the input and model configuration tabls as presented in the report
sink(paste(output.dir,"WBSS_tab_3.6.1-8",sep="/"))
print("TABLE 3.6.1 WBSS HERRING. CATCH IN NUMBER") ; wbss@catch.n
print("TABLE 3.6.2 WBSS HERRING. WEIGHTS AT AGE IN THE CATCH") ; wbss@catch.wt
print("TABLE 3.6.3 WBSS HERRING. WEIGHTS AT AGE IN THE STOCK") ; wbss@stock.wt
print("TABLE 3.6.4 WBSS HERRING. NATURAL MORTALITY") ; wbss@m
print("TABLE 3.6.5 WBSS HERRING. PROPORTION MATURE") ; wbss@mat
print("TABLE 3.6.6 WBSS HERRING. FRACTION OF HARVEST BEFORE SPAWNING") ; wbss@harvest.spwn
print("TABLE 3.6.7 WBSS HERRING. FRACTION OF NATURAL MORTALITY BEFORE SPAWNING") ; wbss@m.spwn
print("TABLE 3.6.8 WBSS HERRING. SURVEY INDICES")
lapply(wbss.tun,function(x){x@range ; x@index})
sink()

sink(paste(output.dir,"WBSS_tab_3.6.9-21",sep="/"))
print("TABLE 3.6.9 WBSS HERRING. STOCK OBJECT CONFIGURATION") ; wbss@range

print("TABLE 3.6.10 WBSS HERRING. FLICA CONFIGURATION SETTINGS") ; wbss.sam@control

print("TABLE 3.6.11 WBSS HERRING. FLR, R SOFTWARE VERSIONS") ; packageDescription("FLSAM")

print("TABLE 3.6.12 WBSS HERRING. STOCK SUMMARY")
cbind("Year"=wbss@range["minyear"]:wbss@range["maxyear"], "Recruitment"=rec(wbss), "TSB"=tsb(wbss), "SSB"=ssb(wbss), "F3-6"=fbar(wbss), "Landings"=wbss@landings)

print("TABLE 3.6.13 WBSS HERRING. ESTIMATED FISHING MORTALITY") ; wbss@harvest

print("TABLE 3.6.14 WBSS HERRING. ESTIMATED POPULATION ABUNDANCE") ; wbss@stock.n # manually add ImY from Tab 3.6.24, excl. age0

print("TABLE 3.6.15 WBSS HERRING. SURVIVORS AFTER TERMINAL YEAR")
#wbss.option[[1]]@stock.n[,as.character(ImY)] # it needs 1 year projection to be run

print("TABLE 3.6.16 WBSS HERRING. FITTED SELECTION PATTERN")
tmp <- as.data.frame(harvest(wbss))
tapply(tmp$data, list(tmp$age, tmp$year), mean)/t(matrix(rep(as.numeric(fbar(wbss)),9),ncol=9))

print("TABLE 3.6.17 WBSS HERRING. PREDICTED CATCH IN NUMBERS")
tmp <- wbss.sam@residuals[wbss.sam@residuals$fleet=="catch",]
round(tapply(exp(tmp$log.mdl), list(tmp$age, tmp$year), mean))

print("TABLE 3.6.18-22 WBSS HERRING. SURVEY RESIDUALS")
paste("HERAS standardized residuals")
tmp <- wbss.sam@residuals[wbss.sam@residuals$fleet=="HERAS",]
round(tapply(tmp$std.res, list(tmp$age, tmp$year), mean),4)
paste("GerAs standardized residuals")
tmp <- wbss.sam@residuals[wbss.sam@residuals$fleet=="GerAS",]
round(tapply(tmp$std.res, list(tmp$age, tmp$year), mean),4)
paste("N20 standardized residuals")
tmp <- wbss.sam@residuals[wbss.sam@residuals$fleet=="N20",]
round(tapply(tmp$std.res, list(tmp$age, tmp$year), mean),4)
paste("IBTS Q1 standardized residuals")
tmp <- wbss.sam@residuals[wbss.sam@residuals$fleet=="IBTS Q1",]
round(tapply(tmp$std.res, list(tmp$age, tmp$year), mean),4)
paste("IBTS Q3 standardized residuals")
tmp <- wbss.sam@residuals[wbss.sam@residuals$fleet=="IBTS Q3",]
round(tapply(tmp$std.res, list(tmp$age, tmp$year), mean),4)


print("TABLE 3.6.23 WBSS HERRING. FIT PARAMETERS ") ; wbss.sam@params[1:wbss.sam@nopar,]

## print("") ; wbss@
## print("") ; wbss@
sink()


print("SAVE DATA FOR ADVICE STANDARD GRAPHS")
out <- cbind(rec(wbss.sam)[,c("year","lbnd","value","ubnd")],
             tsb(wbss.sam)[,c("lbnd","value","ubnd")],
             ssb(wbss.sam)[,c("lbnd","value","ubnd")],
             as.numeric(wbss@catch),
             NA,NA,NA,NA,NA,
             fbar(wbss.sam)[,c("lbnd","value","ubnd")],
             catch(wbss.sam)[,c("lbnd","value","ubnd")])

colnames(out) <- c("Year","Low_Recruitment","Recruitment","High_Recruitment","Low_TBiomass","TBiomass","High_TBiomass","Low_SSB","SSB","High_SSB","Catches","Landings","Discards","IBC","Unallocated_Removals","YieldSSB","Low_F","F","High_F","Low_Catch","Catch","High_Catch")
out[1:3,]
write.table(out,paste(output.dir,"WBSS_data_standard_graph.txt",sep="/"), col.names=T, row.names=F, sep="\t")




#And for incorporation into the standard graphs
#writeFLStock(wbss,file.path(output.dir,"hawg_wbss.sum"),type="ICAsum")
writeFLStock(wbss,file.path(output.dir,"hawg_wbss.ypr"),type="YPR")
#The YPR curves based on the same values as the projection - therefore use WBSS.proj
#writeStandardOutput(wbss,wbss.sr,wbss.retro,recImY=wbss.ica@survivors[1,1],nyrs.=3,output.base,Blim=0.8e6,Bpa=1.3e6,Flim=NULL,Fpa=0.25,Bmsy=NULL,Fmsy=NULL)

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
