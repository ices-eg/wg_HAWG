### ======================================================================================================
### Setting up
### ======================================================================================================
rm(list=ls())
graphics.off()

library(ggplot2)
library(FLSAM)
library(FLEDA)
path <- "C:/git/wg_HAWG/NSAS/"
try(setwd(path),silent=TRUE)

dataDir           <-  file.path(".","data/")        # figures directory
resPath           <-  file.path(".","results/")        # figures directory
output.dir        <-  file.path(".","results/plots_singlefleet")        # figures directory
assessment_name_multifleet  <- "HAWG2020_multifleet"
assessment_name_singlefleet <- "HAWG2020_singlefleet"

source(file.path("../_Common/HAWG_Common_module.r")) # load general functions

#load(paste(resPath,"/NSH_HAWG2020_sf.RData",sep=""))

PDF <- F
PNG <- ifelse(PDF,F,T)
### ============================================================================
### single fleet
### ============================================================================

load(paste(resPath,"/NSH_HAWG2020_sf_retro.RData",sep=""))

### ============================================================================
### Model fit
### ============================================================================

# figure - residual plots at each age for each time series
#if(PDF) pdf(file.path(output.dir,paste(assessment_name_singlefleet,".pdf",sep="")))
#if(PNG) png(file.path(output.dir,paste(assessment_name_singlefleet,"_1_fit_diagnostics_%02d.png",sep="")),units = "px", height=800,width=672, bg = "white")

png(file.path(output.dir,paste(assessment_name_singlefleet,"_1_fit_diagnostics_%02d.png",sep="")),units = "px", height=800,width=672, bg = "white")

residual.diagnostics(NSH.sam)

dev.off()

windows()
# figure - assessment result, spawning stock biomass, fishing mortality, recruitment
print(plot(NSH.sam,futureYrs=F))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_2_stock_trajectory.png",sep = ""),type="png")

# figure - catchabilities at age from HERAS
catch <- catchabilities(NSH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
       scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
       type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
       subset=fleet %in% c("HERAS","IBTS-Q3"),
       main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_3_catchability_HERAS.png",sep = ""),type="png")

# figure - variance by data source
obv <- obs.var(NSH.sam)
obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
obv <- obv[order(obv$value),]
bp <- barplot(obv$value,ylab="Observation Variance",
              main="Observation variances by data source",col=factor(obv$fleet))
axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_4_observation_variance.png",sep = ""),type="png")

# figure - variance vs uncertainty for each data source
plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
     pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_5_observation_variance_CV.png",sep = ""),type="png")

# figure - fishing age selectivity per year
sel.pat <- merge(f(NSH.sam),fbar(NSH.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
xyplot(sel ~ age|sprintf("%i's",floor((year)/5)*5),sel.pat,
       groups=year,type="l",as.table=TRUE,
       scale=list(alternating=FALSE),
       main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar")

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_6_fishing_selectivity.png",sep = ""),type="png")

# figure - correlation matrix of model parameters
print(cor.plot(NSH.sam))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_7_SAM_prameter_correlation.png",sep = ""),type="png")

# figure - catch residuals per year per age
dat <- subset(residuals(NSH.sam),fleet=="catch unique")
print(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year Catch",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
       }))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_8_residuals_catches.png",sep = ""),type="png")

# figure - IBTS-Q1 index residuals per year per age
dat <- subset(residuals(NSH.sam),fleet=="IBTS-Q1")
print(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year IBTSQ1",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
       }))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_9_residuals_IBTSQ1.png",sep = ""),type="png")

# figure - acosutic index residuals per year per age
dat <- subset(residuals(NSH.sam),fleet=="HERAS")
print(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year HERAS",
             panel=function(...){
               lst <- list(...)
               panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
             }))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_10_residuals_HERAS.png",sep = ""),type="png")

# figure - acosutic index residuals per year per age
dat <- subset(residuals(NSH.sam),fleet=="IBTS-Q3")
print(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year IBTS-Q3",
             panel=function(...){
               lst <- list(...)
               panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
             }))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_11_residuals_IBTSQ3.png",sep = ""),type="png")

# process error in terms of N
print(procerr.plot(NSH+NSH.sam,weight="stock.wt",type="n",rel=T))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_12_process_error_N.png",sep = ""),type="png")

# process error in terms of additional mortality
print(procerr.plot(NSH+NSH.sam,weight="stock.wt",type="mort",rel=F))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_13_process_error_M.png",sep = ""),type="png")

# figure - times series for each age, stock
timeseries(window(NSH,1975,range(NSH)["maxyear"]),slot="stock.wt")

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_14_ts_stock_weight.png",sep = ""),type="png")

# figure - times series for each age, catches
timeseries(window(NSH,1975,range(NSH)["maxyear"]),slot="catch.wt")

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_15_ts_catch_weight.png",sep = ""),type="png")

# figure - times series for each age, harvest
print(timeseries(window(NSH,2000,range(NSH)["maxyear"]),slot="harvest"))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_16_ts_harvest.png",sep = ""),type="png")

# figure - times series for each age, maturity
print(timeseries(window(NSH,1990,range(NSH)["maxyear"]),slot="mat"))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_17_ts_mat.png",sep = ""),type="png")

# figure - times series for each age, mortality
print(timeseries(window(NSH,1947,range(NSH)["maxyear"]),slot="m"))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_18_ts_M.png",sep = ""),type="png")

## figure - acoustic index at age
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH.tun[["HERAS"]]@index)),groups="age",main="Proportion of Acoustic index at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_19_HERAS_prop.png",sep = ""),type="png")

#figure - TACs and catches
#TACs          <- read.csv(file.path(".","data","historic data","TAC-historic.csv"))
TACs          <- read.csv(file.path(".","data","TAC_var","NSAS_TAC.csv"))
TAC.plot.dat  <- data.frame(year=rep(TACs$year,each=2)+c(-0.5,0.5),TAC=rep(rowSums(TACs[,c("A","B")],na.rm=T),each=2))
catch         <- as.data.frame(NSH@catch[,ac(TACs$year[1:(length(TACs$year)-1)])])#
plot(0,0,pch=NA,xlab="Year",ylab="Catch",xlim=range(c(catch$year,TAC.plot.dat$year)),ylim=range(c(0,TAC.plot.dat$TAC,catch$data)),cex.lab=1.2,cex.axis=1.1,font=2)
rect(catch$year-0.5,0,catch$year+0.5,catch$data,col="grey")
lines(TAC.plot.dat,lwd=3)
legend("topright",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",pt.bg="grey",pt.cex=c(2),box.lty=0)
box()
title(main=paste(NSH@name,"Catch and TAC"))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_20_TAC_catches.png",sep = ""),type="png")

# weight in the stock by cohort
west.by.cohort      <- as.data.frame(FLCohort(window(NSH@stock.wt,2000,range(NSH)["maxyear"])))
west.by.cohort      <- subset(west.by.cohort,!is.na(west.by.cohort$data))
west.by.cohort$year <- west.by.cohort$age + west.by.cohort$cohort
west.cohort.plot    <- xyplot(data~year,data=west.by.cohort,
                              groups=cohort,
                              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
                              type="b",
                              xlab="Year",ylab="Weight in the stock (kg)",
                              main=paste(NSH@name,"Weight in the stock by cohort"),
                              par.settings=list(superpose.symbol=list(pch=as.character(unique(west.by.cohort$cohort)%%10),cex=1.25)),
                              panel=function(...) {
                                panel.grid(h=-1,v=-1)
                                panel.xyplot(...)
                              })
print(west.cohort.plot)

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_21_weight_stock_cohort.png",sep = ""),type="png")

# figure - overlay survey time series for the different ages.
start_year  <- 2000
end_year    <- 2020

for(iAge in range(NSH)["min"]:range(NSH)["max"]){
  for(idxSurvey in 1:length(NSH.tun)){
    # make sure we don't take any of the LAI data
    if(!grepl('LAI',name(NSH.tun[[idxSurvey]]))){
      idxFilt <- which(iAge == NSH.tun[[idxSurvey]]@range[1]:NSH.tun[[idxSurvey]]@range[2])
      
      if(idxSurvey == 1){
        dat <- cbind(as.data.frame(NSH.tun[[idxSurvey]]@index[idxFilt,]),
                     rep(NSH.tun[[idxSurvey]]@name,
                         length(drop(NSH.tun[[idxSurvey]]@index[idxFilt,]))))
        colnames(dat)[dim(dat)[2]] <- 'survey'
        dat$data <- (dat$data-mean(dat$data[dat$data != -1]))/sd(dat$data[dat$data != -1])
      }else{
        tempVar <- cbind(as.data.frame(NSH.tun[[idxSurvey]]@index[idxFilt,]),
                         rep(NSH.tun[[idxSurvey]]@name,
                             length(drop(NSH.tun[[idxSurvey]]@index[idxFilt,]))))
        colnames(tempVar)[dim(tempVar)[2]] <- 'survey'
        tempVar$data <- (tempVar$data-mean(tempVar$data[tempVar$data != -1]))/sd(tempVar$data[tempVar$data != -1])
        dat     <- rbind(dat,tempVar)
      }
    }
  }
  
  p <-  ggplot(data = dat,mapping=aes(x=year,y=data,color=survey))+
        geom_line()+
        xlim(start_year, end_year)+
        ylab('standardized index')+xlab('year')+
        ggtitle(paste('age ',iAge))
  
  print(p)
  #print(overlayTimeseries(FLQuants(IBTS0=NSH.tun[["IBTS0"]]@index,
  #                                 HERAS=NSH.tun[["HERAS"]]@index,
  #                                 IBTSQ3=NSH.tun[["IBTS-Q3"]]@index,
  #                                 IBTSQ1=NSH.tun[["IBTS-Q1"]]@index),
  #                        nyrs=20,ages=iAge))
  savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_22_survey_time_series_ages",iAge,".png",sep = ""),type="png")
}

# figure - catch number at age
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@catch.n)),groups="age",main="Proportion of Catch numbers at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_23_catch_N.png",sep = ""),type="png")

# IBTS0/IBTSQ1 relationship
IBTSQ1      <- read.table(paste(dataDir,"/IBTS index/IBTSQ1/DATRAS_IBTSQ1.csv",sep=""), sep=",", header = TRUE) # read raw indices instead of standardized ones
IBTSQ1      <- IBTSQ1[IBTSQ1$IndexArea == "NS_Her",]
IBTSQ1$Year <- IBTSQ1$Year-1

array_IBTSQ1 <- data.frame(cbind(IBTSQ1$Year, IBTSQ1$Age_1))
names(array_IBTSQ1) <- c("year","data")

array_IBTS0   <- subset(as.data.frame(NSH.tun), cname == "IBTS0" & slot == "index")
array_IBTS0$year <- array_IBTS0$year-1

minYear <- max(c(min(array_IBTSQ1$year-1), 
                 min(array_IBTS0$year)))

array_IBTSQ1 <- array_IBTSQ1[array_IBTSQ1$year >= minYear,]
array_IBTS0 <- array_IBTS0[array_IBTS0$year >= minYear,]

array_IBTSQ1$year <- array_IBTSQ1$year-1
array_IBTSQ1 <- array_IBTSQ1[2:dim(array_IBTSQ1)[1],] # this because IBTS0 is the limiting factor

# construct the array to be plotted
tabMarkers <- data.frame(cbind(array_IBTSQ1$year,
                               array_IBTSQ1$data,
                               array_IBTS0[1:dim(array_IBTS0)[1]-1,]$data)) # exclude last year from the vector

names(tabMarkers) <- c("year", "IBTSQ1", "IBTS0")

g<- ggplot(tabMarkers[1:dim(tabMarkers)[1]-1,], aes(x= IBTS0, y = IBTSQ1, label = year)) + 
      ylab("IBTSQ1 index") + 
      xlab("IBTS0 index") + 
      geom_point(x=tabMarkers[dim(tabMarkers)[1],]$IBTS0, 
                 y=tabMarkers[dim(tabMarkers)[1],]$IBTSQ1, 
                 shape = 18,
                 colour = "black",
                 size=5) + # last year
      geom_text(colour = "red") + 
      geom_vline(xintercept=array_IBTS0[dim(array_IBTS0)[1],]$data,
                 colour="blue") +
      geom_text(aes(x=array_IBTS0[dim(array_IBTS0)[1],]$data, 
                    label=array_IBTS0[dim(array_IBTS0)[1],]$year, 
                    y=(max(tabMarkers$IBTSQ1)- min(tabMarkers$IBTSQ1))/2+min(tabMarkers$IBTSQ1)), 
                colour="blue", 
                angle=90, 
                vjust = -0.3)
print(g)

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_24_IBTS0_vs_IBTSQ1.png",sep = ""),type="png")

# figure - fishing mortality vs SSB, management plan
plot(x=c(0,0.8,1.4,2.6),
     y=c(0.1,0.1,0.26,0.26),
     type="l",
     ylim=c(0,0.4),
     lwd=2,
     xlab="SSB in million tonnes",
     ylab="Fbar",
     cex.lab=1.3,
     main="Management plan North Sea Herring")
abline(v=0.8,col="red",lwd=2,lty=2)
abline(v=0.9,col="blue",lwd=2,lty=2)
abline(v=1.4,col="darkgreen",lwd=2,lty=2)
text(0.8,0,labels=expression(B[lim]),col="red",cex=1.3,pos=2)
text(0.9,0,labels=expression(B[pa]),col="blue",cex=1.3,pos=2)
text(1.4,0,labels=expression(B[trigger]),col="darkgreen",cex=1.3,pos=4)

points(y=fbar(NSH[,ac(2005:2018)]), x=(ssb(NSH[,ac(2005:2018)])/1e6),pch=19)
lines(y=fbar(NSH[,ac(2005:2018)]),  x=(ssb(NSH[,ac(2005:2018)])/1e6))
text(y=fbar(NSH[,ac(2005:2018)]),   x=(ssb(NSH[,ac(2005:2018)])/1e6),labels=ac(2005:2018),pos=3,cex=0.7)

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_25_F_vs_SSB.png",sep = ""),type="png")


# internal consistency HERAS
print(plot(NSH.tun[["HERAS"]],type="internal"))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_26_HERAS_internal.png",sep = ""),type="png")

# internal consistency IBTS-Q3
print(plot(NSH.tun[["IBTS-Q3"]],type="internal"))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_27_IBTSQ3_internal.png",sep = ""),type="png")

# retro stock trajectory
print(plot(NSH.retro))
# print(plot(NSH.retro,xlim=c(2010,2020)))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_28_retro_stock.png",sep = ""),type="png")

# model parameters retrospective
print(retroParams(NSH.retro))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_29_retro_params.png",sep = ""),type="png")

# selectivity trajectory retrospective
print(retroSelectivity(NSH.retro,2009:range(NSH)["maxyear"]))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_30_retro_F.png",sep = ""),type="png")

# component proportions
print(comp.plot(NSH.sam))

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_31_LAI_prop.png",sep = ""),type="png")

# figure - otholith. Warning, this takes very long!
otolith(NSH.sam,n=1000)

savePlot(paste(output.dir,"/",assessment_name_singlefleet,"_32_otolith.png",sep = ""),type="png")

dev.off()

################################################################################
### ============================================================================
### Multi-fleet
### ============================================================================
################################################################################

output.dir        <-  file.path(".","results/plots_multifleet")        # figures directory

#load("C:/git/wg_HAWG/NSAS/results/NSH_HAWG2018_mf.RData")
load(paste(resPath,"/NSH_HAWG2020_mf_retro.RData",sep=""))

if(PDF) pdf(file.path(output.dir,paste(assessment_name_multifleet,"_multifleet_diagnostics.pdf",sep="")))
if(PNG) png(file.path(output.dir,paste(assessment_name_multifleet,"_multifleet_diagnostics_%02d.png",sep="")),units = "px", height=800,width=672, bg = "white")

#residual.diagnostics(NSH3f.sam)

# figure - residual plots at each age for each time series
#residual.diagnostics(NSH3f.sam)


windows()
# figure - assessment result, spawning stock biomass, fishing mortality, recruitment
plot(NSH3f.sam,futureYrs=F)

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_2_stock_trajectory.png",sep = ""),type="png")

# figure - catchabilities at age from HERAS
catch <- catchabilities(NSH3f.sam)
xyplot(value+ubnd+lbnd ~ age | fleet,catch,
       scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
       type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
       subset=fleet %in% c("HERAS","IBTS-Q3"),
       main="Survey catchability parameters",ylab="Catchability",xlab="Age")

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_3_catchability_HERAS.png",sep = ""),type="png")

# figure - variance by data source
obv <- obs.var(NSH3f.sam)
obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
obv <- obv[order(obv$value),]
bp <- barplot(obv$value,ylab="Observation Variance",
              main="Observation variances by data source",col=factor(obv$fleet))
axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_4_variances.png",sep = ""),type="png")

# figure - variance vs uncertainty for each data source
plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
     pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_5_CV_vs_var.png",sep = ""),type="png")

# figure - fishing age selectivity per year for A fleet
sel.pat <- merge(f(NSH3f.sam),fbar(NSH3f.sam),
                 by="year",suffixes=c(".f",".fbar"))

sel.pat <- subset(sel.pat,fleet=='catch A')
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
xyplot(sel ~ age|sprintf("%i's",floor((year+2)/5)*5),sel.pat,
       groups=year,type="l",as.table=TRUE,
       scale=list(alternating=FALSE),
       main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar")

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_6_fishing_selectivity_A.png",sep = ""),type="png")

# figure - fishing age selectivity per year for C fleet
sel.pat <- merge(f(NSH3f.sam),fbar(NSH3f.sam),
                 by="year",suffixes=c(".f",".fbar"))

sel.pat <- subset(sel.pat,fleet=='catch C')
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
xyplot(sel ~ age|sprintf("%i's",floor((year+2)/5)*5),sel.pat,
       groups=year,type="l",as.table=TRUE,
       scale=list(alternating=FALSE),
       main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar")

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_6_fishing_selectivity_C.png",sep = ""),type="png")

# figure - fishing age selectivity per year for BD fleet
sel.pat <- merge(f(NSH3f.sam),fbar(NSH3f.sam),
                 by="year",suffixes=c(".f",".fbar"))

sel.pat <- subset(sel.pat,fleet=='catch BD')
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
xyplot(sel ~ age|sprintf("%i's",floor((year+2)/5)*5),sel.pat,
       groups=year,type="l",as.table=TRUE,
       scale=list(alternating=FALSE),
       main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar")

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_6_fishing_selectivity_BD.png",sep = ""),type="png")

# figure - correlation matrix of model parameters
cor.plot(NSH3f.sam)

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_7_corr_SAM_params.png",sep = ""),type="png")

# figure - catch residuals per year per age
dat <- subset(residuals(NSH3f.sam),fleet=="catch unique")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year Catch",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       })

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_8_residuals_catch.png",sep = ""),type="png")

# figure - acosutic index residuals per year per age
dat <- subset(residuals(NSH3f.sam),fleet=="IBTS-Q1")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year IBTSQ1",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       })

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_22_residuals_IBTSQ1.png",sep = ""),type="png")

# process error in terms of N
procerr.plot(NSH+NSH3f.sam,weight="stock.wt",type="n",rel=T)

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_10_process_error_N.png",sep = ""),type="png")

# process error in terms of additional mortality
procerr.plot(NSH+NSH3f.sam,weight="stock.wt",type="mort",rel=F)

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_11_process_error_M.png",sep = ""),type="png")





### ============================================================================
### ============================================================================
### fishing selectivity
### ============================================================================
### ============================================================================

windows()
sel.pat <- merge(f(NSH3f.sam),fbar(NSH3f.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))

# fleet A
selA <- subset(sel.pat, fleet == "catch A")
selA <- subset(selA, year > 2012)
for(idxYear in 1:length(selA$year)){selA$year[idxYear] <- toString(selA$year[idxYear])}

ggplot(selA, aes(x = age, y = sel, colour = year)) + geom_line() + xlab("age") + ylab("F/Fbar") + ggtitle("fleet A")

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_selectivity_fleetA.png",sep = ""),type="png")

# fleet BD
selBD <- subset(sel.pat, fleet == "catch BD")
selBD <- subset(selBD, year > 2012)
for(idxYear in 1:length(selBD$year)){selBD$year[idxYear] <- toString(selBD$year[idxYear])}

ggplot(selBD, aes(x = age, y = sel, colour = year)) + geom_line() + xlab("age") + ylab("F/Fbar") + ggtitle("fleet BD")

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_selectivity_fleetBD.png",sep = ""),type="png")

# fleet C
selC <- subset(sel.pat, fleet == "catch C")
selC <- subset(selC, year > 2012)
for(idxYear in 1:length(selC$year)){selC$year[idxYear] <- toString(selC$year[idxYear])}

ggplot(selC, aes(x = age, y = sel, colour = year)) + geom_line() + xlab("age") + ylab("F/Fbar") + ggtitle("fleet C")

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_selectivity_fleetC.png",sep = ""),type="png")

### ============================================================================
### single fleet vs multi-fleet
### ============================================================================

load(paste(res.dir,"/NSH_HAWG2020_mf.RData",sep=""))
load(paste(res.dir,"/NSH_HAWG2020_sf.RData",sep=""))

if(PDF) pdf(file.path(output.dir,paste(assessment_name_multifleet,"_diagnostics.pdf",sep="")))
if(PNG) png(file.path(output.dir,paste(assessment_name_multifleet,"_diagnostics_%02d.png",sep="")),units = "px", height=800,width=672, bg = "white")

residual.diagnostics(NSH.sam)

# figure - residual plots at each age for each time series
#residual.diagnostics(NSH3f.sam)


load(paste(resPath,"/NSH_HAWG2019_mf.RData",sep=""))
load(paste(resPath,"/NSH_HAWG2019_sf.RData",sep=""))


windows()

par(mfrow=c(3,1))
# SSB
plotQuant <- ssb(NSH.sam)
plotQuant2 <- ssb(NSH3f.sam)
years <- plotQuant$year

plot(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),xlab='year',pch = ".",ylab='SSB')
lines(years, plotQuant$value, type="l",lwd=2,col=rgb(0,0,1))
polygon(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),col=rgb(0,0,1,0.5),lty=0)

lines(years, plotQuant2$value, type="l",lwd=2,col=rgb(1,0,0))
polygon(c(years,rev(years)),c(plotQuant2$lbnd,rev(plotQuant2$ubnd)),col=rgb(1,0,0,0.5),lty=0)

legend("topright",legend=c("single fleet","multi-fleet"),col=c(rgb(0,0,1),rgb(1,0,0)),lty=1,lwd=2)

#savePlot(paste(output.dir,"/",assessment_name_multifleet,"_single_fleet_SSB.png",sep = ""),type="png")

# fbar
plotQuant <- fbar(NSH.sam)
plotQuant2 <- fbar(NSH3f.sam)
years <- plotQuant$year

plot(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),xlab='year',pch = ".",ylab='fbar')
lines(years, plotQuant$value, type="l",lwd=2,col=rgb(0,0,1))
polygon(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),col=rgb(0,0,1,0.5),lty=0)

lines(years, plotQuant2$value, type="l",lwd=2,col=rgb(1,0,0))
polygon(c(years,rev(years)),c(plotQuant2$lbnd,rev(plotQuant2$ubnd)),col=rgb(1,0,0,0.5),lty=0)

#savePlot(paste(output.dir,"/",assessment_name_multifleet,"_single_fleet_fbar.png",sep = ""),type="png")

# recruitment
plotQuant <- rec(NSH.sam)
plotQuant2 <- rec(NSH3f.sam)
years <- plotQuant$year

plot(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),xlab='year',pch = ".",ylab='recruitment')
lines(years, plotQuant$value, type="l",lwd=2,col=rgb(0,0,1))
polygon(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),col=rgb(0,0,1,0.5),lty=0)

lines(years, plotQuant2$value, type="l",lwd=2,col=rgb(1,0,0))
polygon(c(years,rev(years)),c(plotQuant2$lbnd,rev(plotQuant2$ubnd)),col=rgb(1,0,0,0.5),lty=0)

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_single_fleet.png",sep = ""),type="png")

par(mfrow=c(1,1))
# fbar
plotQuant <- fbar(NSH.sam)
plotQuant2 <- fbar(NSH3f.sam)
years <- plotQuant$year

plot(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),xlab='year',pch = ".",ylab='fbar')
lines(years, plotQuant$value, type="l",lwd=2,col=rgb(0,0,1))
polygon(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),col=rgb(0,0,1,0.5),lty=0)

lines(years, plotQuant2$value, type="l",lwd=2,col=rgb(1,0,0))
polygon(c(years,rev(years)),c(plotQuant2$lbnd,rev(plotQuant2$ubnd)),col=rgb(1,0,0,0.5),lty=0)

legend("topright",legend=c("single fleet","multi-fleet"),col=c(rgb(0,0,1),rgb(1,0,0)),lty=1,lwd=2)

savePlot(paste(output.dir,"/",assessment_name_multifleet,"_single_fleet_fbar.png",sep = ""),type="png")


### ============================================================================
### ============================================================================
### ============================================================================
### To tidy up
### ============================================================================
### ============================================================================
### ============================================================================

### ============================================================================
### Model fit
### ============================================================================

# figure - residual plots at each age for each time series
png(file.path(output.dir,paste(assessment_name,"_fit_diagnostics_%02d.png",sep="")),units = "px", height=800,width=672, bg = "white")
residual.diagnostics(NSH.sam)
dev.off()

windows()
# figure - assessment result, spawning stock biomass, fishing mortality, recruitment
plot(NSH.sam,futureYrs=F)
savePlot(paste(output.dir,assessment_name,"_stock_trajectory.png",sep = ""),type="png")

# figure - uncertainties as a function of time
#par(mfrow=c(1,2))
#CV.yrs <- ssb(NSH.sam)$year
#CV.dat <- cbind(SSB=ssb(NSH.sam)$CV,
#                Fbar=fbar(NSH.sam)$CV,Rec=rec(NSH.sam)$CV)
#matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
#        xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
#grid()
#legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")
#CV.yrs <- ssb(NSH.sam)$year
#CV.dat <- cbind(SSB=ssb(NSH.sam)$CV,
#                Fbar=fbar(NSH.sam)$CV,Rec=rec(NSH.sam)$CV)
#matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
#        xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
#grid()
#legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")
#CV.yrs <- ssb(NSH.sam)$year
#CV.dat <- cbind(SSB=ssb(NSH.sam)$CV,
#                Fbar=fbar(NSH.sam)$CV,Rec=rec(NSH.sam)$CV)
#matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
#        xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
#grid()
#legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")

# figure - catchabilities at age from HERAS
catch <- catchabilities(NSH.sam)
xyplot(value+ubnd+lbnd ~ age | fleet,catch,
       scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
       type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
       subset=fleet %in% c("HERAS"),
       main="Survey catchability parameters",ylab="Catchability",xlab="Age")

savePlot(paste(output.dir,assessment_name,"_catchability_HERAS.png",sep = ""),type="png")

# figure - variance by data source
obv <- obs.var(NSH.sam)
obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
obv <- obv[order(obv$value),]
bp <- barplot(obv$value,ylab="Observation Variance",
              main="Observation variances by data source",col=factor(obv$fleet))
axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

savePlot(paste(output.dir,assessment_name,"_variances.png",sep = ""),type="png")

# figure - variance vs uncertainty for each data source
plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
     pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

savePlot(paste(output.dir,assessment_name,"_CV_vs_var.png",sep = ""),type="png")

# figure - fishing age selectivity per year
sel.pat <- merge(f(NSH.sam),fbar(NSH.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
xyplot(sel ~ age|sprintf("%i's",floor((year+2)/5)*5),sel.pat,
       groups=year,type="l",as.table=TRUE,
       scale=list(alternating=FALSE),
       main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar")

savePlot(paste(output.dir,assessment_name,"_fishing_selectivity.png",sep = ""),type="png")

# figure - correlation matrix of model parameters
cor.plot(NSH.sam)

savePlot(paste(output.dir,assessment_name,"_corr_SAM_params.png",sep = ""),type="png")

# figure - catch residuals per year per age
dat <- subset(residuals(NSH.sam),fleet=="catch unique")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year Catch",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       })

savePlot(paste(output.dir,assessment_name,"_residuals_catch.png",sep = ""),type="png")

# figure - acosutic index residuals per year per age  
dat <- subset(residuals(NSH.sam),fleet=="HERAS")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year HERAS",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       })

savePlot(paste(output.dir,assessment_name,"_residuals_HERAS.png",sep = ""),type="png")

# process error in terms of N
procerr.plot(NSH+NSH.sam,weight="stock.wt",type="n",rel=T)

savePlot(paste(output.dir,assessment_name,"_process_error_N.png",sep = ""),type="png")

# process error in terms of additional mortality
procerr.plot(NSH+NSH.sam,weight="stock.wt",type="mort",rel=F)

savePlot(paste(output.dir,assessment_name,"_process_error_M.png",sep = ""),type="png")

### ============================================================================
### time series
### ============================================================================

windows()
## === Plot the time series of weight in the stock and catch in the stock ===
# figure - times series for each age, stock
timeseries(window(NSH,1975,range(NSH)["maxyear"]),slot="stock.wt")
savePlot(paste(output.dir,assessment_name,"_time_series_stock_weight.png",sep = ""),type="png")
# figure - times series for each age, catches
timeseries(window(NSH,1975,range(NSH)["maxyear"]),slot="catch.wt")
savePlot(paste(output.dir,assessment_name,"_time_series_catch_weight.png",sep = ""),type="png")
# figure - times series for each age, harvest
timeseries(window(NSH,2000,range(NSH)["maxyear"]),slot="harvest")
savePlot(paste(output.dir,assessment_name,"_time_series_harvest.png",sep = ""),type="png")
# figure - times series for each age, maturity
timeseries(window(NSH,1990,range(NSH)["maxyear"]),slot="mat")
savePlot(paste(output.dir,assessment_name,"_time_series_mat.png",sep = ""),type="png")
## figure - times series for each age, mortality
timeseries(window(NSH,1947,range(NSH)["maxyear"]),slot="m")
savePlot(paste(output.dir,assessment_name,"_time_series_M.png",sep = ""),type="png")

# figure - internal consistency HERAS
plot(NSH.tun[["HERAS"]],type="internal")

savePlot(paste(output.dir,assessment_name,"_HERAS_consistency.png",sep = ""),type="png")

# figure - internal consistency IBTSQ3
plot(NSH.tun[["IBTS-Q1"]],type="internal")

savePlot(paste(output.dir,assessment_name,"_IBTSQ3_consistency.png",sep = ""),type="png")

# figure the overlay of tuning series - !!!!!!!!!!!!!!!! needs sorting !!!!!!!!!!!!!!
## figure - plot of time series: HERAS, IBTS0, IBTSQ1 by cohort.
#print(overlayTimeseries(lapply(NSH.tun,index),nyrs=20,ages=0:1))
## figure - plot of time series: IBTS0, IBTSQ1 by cohort.
#print(overlayTimeseries(FLQuants(IBTS0=NSH.tun[["IBTS0"]]@index,IBTSQ1=NSH.tun[["IBTS-Q1"]]@index),nyrs=20,ages=0:1))
#  
## figure - time series of all data by age
#print(surveyTimeseries(NSH.tun))

#savePlot(paste(output.dir,assessment_name,"_survey_time_series.png",sep = ""),type="png")

#figure - TACs and catches - !!!!!!!!!!!!!Need to update historic table !!!!!!!!!!!!!!!!!!!!!!
TACs          <- read.csv(file.path(".","data","historic data","TAC-historic.csv"))
TAC.plot.dat  <- data.frame(year=rep(TACs$year,each=2)+c(-0.5,0.5),TAC=rep(rowSums(TACs[,c("Agreed_A","Bycatch_B")],na.rm=T),each=2))
catch         <- as.data.frame(NSH@catch[,ac(TACs$year)]/1e3)
plot(0,0,pch=NA,xlab="Year",ylab="Catch",xlim=range(c(catch$year,TAC.plot.dat$year)),ylim=range(c(0,TAC.plot.dat$TAC,catch$data)),cex.lab=1.2,cex.axis=1.1,font=2)
rect(catch$year-0.5,0,catch$year+0.5,catch$data,col="grey")
lines(TAC.plot.dat,lwd=3)
legend("topright",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",pt.bg="grey",pt.cex=c(2),box.lty=0)
box()
title(main=paste(NSH@name,"Catch and TAC"))

savePlot(paste(output.dir,assessment_name,"_TAC_catch.png",sep = ""),type="png")

## === Plot the proportion of different quantities at age ===

# figure - catch number at age
stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@catch.n)),groups="age",main="Proportion of Catch numbers at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9))

savePlot(paste(output.dir,assessment_name,"_prop_catch.png",sep = ""),type="png")

# figure - proportion of stock weight at age
stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@stock.wt)),groups="age",main="Proportion of Stock weight at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9))

savePlot(paste(output.dir,assessment_name,"_prop_stock_weight.png",sep = ""),type="png")

# figure - proportion of catch weight at age
stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@catch.wt)),groups="age",main="Proportion of Catch weight at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9))

savePlot(paste(output.dir,assessment_name,"_prop_catch_weight.png",sep = ""),type="png")

## figure - acoustic index at age
stacked.area.plot(data~year| unit, as.data.frame(pay(NSH.tun[["HERAS"]]@index)),groups="age",main="Proportion of Acoustic index at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9))

savePlot(paste(output.dir,assessment_name,"_prop_acoustic_index.png",sep = ""),type="png")

# figure - proportion of natural mortality at age
stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@m)),groups="age",main="Proportion of natural mortality at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9))

savePlot(paste(output.dir,assessment_name,"_prop_nat_mort.png",sep = ""),type="png")


#### ============================================================================
#### Management
#### ============================================================================
#
## figure - fishing mortality vs SSB, management plan
plot(x=c(0,0.8,1.4,2.6),
     y=c(0.1,0.1,0.26,0.26),
     type="l",
     ylim=c(0,0.4),
     lwd=2,
     xlab="SSB in million tonnes",
     ylab="Fbar",
     cex.lab=1.3,
     main="Management plan North Sea Herring")
abline(v=0.8,col="red",lwd=2,lty=2)
abline(v=0.9,col="blue",lwd=2,lty=2)
abline(v=1.4,col="darkgreen",lwd=2,lty=2)
text(0.8,0,labels=expression(B[lim]),col="red",cex=1.3,pos=2)
text(0.9,0,labels=expression(B[pa]),col="blue",cex=1.3,pos=2)
text(1.4,0,labels=expression(B[trigger]),col="darkgreen",cex=1.3,pos=4)

points(y=fbar(NSH[,ac(2005:2017)]), x=(ssb(NSH[,ac(2005:2017)])/1e6),pch=19)
lines(y=fbar(NSH[,ac(2005:2017)]),  x=(ssb(NSH[,ac(2005:2017)])/1e6))
text(y=fbar(NSH[,ac(2005:2017)]),   x=(ssb(NSH[,ac(2005:2017)])/1e6),labels=ac(2005:2017),pos=3,cex=0.7)

savePlot(paste(output.dir,assessment_name,"_management_plan.png",sep = ""),type="png")


### ============================================================================
### figures
### ============================================================================
windows()
#Setup plots
# stock trajectory retrospective


#subset(NSH_temp,)

st.names <- c("HAWG2017","SMS2017","HAWG2017 profiled","SMS2017 profiled")
#st.names <- c("0_basecase","2_newM","2_newM","2_newM")
names(M) <- st.names
#names(M) <- c("HAWG_2016","SMS_2017","SMS_2017_profiling","SMS_2017_profiling")#st.names#c("HAWG 2016", "SMS 2017", "SMS 2017 profiling", "SMS 2017 profiling")#st.names
ggplot(M , aes (x =year ,y =data  , colour = qname)) + geom_line() + facet_wrap(~age) + ylab("M") + xlab("year") + theme(legend.position="bottom") + scale_colour_discrete(name = "")

windows()
yearListRetro <- names(NSH.retro)

# LAI_ORSH
a <- list()
for(idxyear in 1:length(yearListRetro)){
  
  NSH_components  <- t(components(NSH.retro[[idxyear]]))
  NSH_residuals   <- residuals(NSH.retro[[idxyear]])
  LAI_BUN_residuals   <- subset(NSH_residuals, fleet == 'LAI-BUN')
  LAI_CNS_residuals   <- subset(NSH_residuals, fleet == 'LAI-CNS')
  LAI_ORSH_residuals   <- subset(NSH_residuals, fleet == 'LAI-ORSH')
  LAI_SNS_residuals   <- subset(NSH_residuals, fleet == 'LAI-SNS')
  
  yearList            <- data.frame(as.numeric(row.names(NSH_components)))
  names(yearList)     <- 'year'
  LAI_data <- c(NSH_components[,1], NSH_components[,2], NSH_components[,3], NSH_components[,4])
  LAI_names <- c(rep('LAI_ORSH',dim(NSH_components)[1]), rep('LAI_CNS',dim(NSH_components)[1]), rep('LAI_BUN',dim(NSH_components)[1]), rep('LAI_SNS',dim(NSH_components)[1]))
  LAI_years <- c(yearList$year, yearList$year, yearList$year, yearList$year)
  NSH_components <- as.data.frame(t(rbind(LAI_years,LAI_names,LAI_data)))
  
  NSH_components[,1] <- as.numeric(NSH_components[,1])
  NSH_components[,3] <- as.numeric(NSH_components[,3])
  
  NSH_components_new <- NSH_components[NSH_components$LAI_names == 'LAI_ORSH',]
  NSH_components_new[,4] <- rep(toString(max(NSH_components_new[,1])),length(NSH_components_new[,1]))
  names(NSH_components_new)[4] <- 'year_retro'
  #a <- NSH_components[NSH_components$LAI_names == 'LAI_ORSH',]
  
  a <- rbind(a,NSH_components_new)
  
  #g <- g + geom_line(data=NSH_components, aes(x= LAI_years, y = LAI_data, colour = LAI_names))# + geom_line() + ylab("index") + xlab("years")
}

ggplot(a, aes(x= LAI_years, y = LAI_data, colour = year_retro)) + geom_line() + ylab("index") + xlab("years") + ggtitle('Orkney/Shetland')

savePlot(paste(output.dir,assessment_name,"_LAI_ORSH_retro.png",sep = ""),type="png")

# LAI_CNS
a <- list()
for(idxyear in 1:length(yearListRetro)){
  
  NSH_components  <- t(components(NSH.retro[[idxyear]]))
  NSH_residuals   <- residuals(NSH.retro[[idxyear]])
  LAI_BUN_residuals   <- subset(NSH_residuals, fleet == 'LAI-BUN')
  LAI_CNS_residuals   <- subset(NSH_residuals, fleet == 'LAI-CNS')
  LAI_ORSH_residuals   <- subset(NSH_residuals, fleet == 'LAI-ORSH')
  LAI_SNS_residuals   <- subset(NSH_residuals, fleet == 'LAI-SNS')
  
  yearList            <- data.frame(as.numeric(row.names(NSH_components)))
  names(yearList)     <- 'year'
  LAI_data <- c(NSH_components[,1], NSH_components[,2], NSH_components[,3], NSH_components[,4])
  LAI_names <- c(rep('LAI_ORSH',dim(NSH_components)[1]), rep('LAI_CNS',dim(NSH_components)[1]), rep('LAI_BUN',dim(NSH_components)[1]), rep('LAI_SNS',dim(NSH_components)[1]))
  LAI_years <- c(yearList$year, yearList$year, yearList$year, yearList$year)
  NSH_components <- as.data.frame(t(rbind(LAI_years,LAI_names,LAI_data)))
  
  NSH_components[,1] <- as.numeric(NSH_components[,1])
  NSH_components[,3] <- as.numeric(NSH_components[,3])
  
  NSH_components_new <- NSH_components[NSH_components$LAI_names == 'LAI_CNS',]
  NSH_components_new[,4] <- rep(toString(max(NSH_components_new[,1])),length(NSH_components_new[,1]))
  names(NSH_components_new)[4] <- 'year_retro'
  
  a <- rbind(a,NSH_components_new)
}

ggplot(a, aes(x= LAI_years, y = LAI_data, colour = year_retro)) + geom_line() + ylab("index") + xlab("years") + ggtitle('Banks')

savePlot(paste(output.dir,assessment_name,"_LAI_CNS_retro.png",sep = ""),type="png")

# LAI_BUN
a <- list()
for(idxyear in 1:length(yearListRetro)){
  
  NSH_components  <- t(components(NSH.retro[[idxyear]]))
  NSH_residuals   <- residuals(NSH.retro[[idxyear]])
  LAI_BUN_residuals   <- subset(NSH_residuals, fleet == 'LAI-BUN')
  LAI_CNS_residuals   <- subset(NSH_residuals, fleet == 'LAI-CNS')
  LAI_ORSH_residuals   <- subset(NSH_residuals, fleet == 'LAI-ORSH')
  LAI_SNS_residuals   <- subset(NSH_residuals, fleet == 'LAI-SNS')
  
  yearList            <- data.frame(as.numeric(row.names(NSH_components)))
  names(yearList)     <- 'year'
  LAI_data <- c(NSH_components[,1], NSH_components[,2], NSH_components[,3], NSH_components[,4])
  LAI_names <- c(rep('LAI_ORSH',dim(NSH_components)[1]), rep('LAI_CNS',dim(NSH_components)[1]), rep('LAI_BUN',dim(NSH_components)[1]), rep('LAI_SNS',dim(NSH_components)[1]))
  LAI_years <- c(yearList$year, yearList$year, yearList$year, yearList$year)
  NSH_components <- as.data.frame(t(rbind(LAI_years,LAI_names,LAI_data)))
  
  NSH_components[,1] <- as.numeric(NSH_components[,1])
  NSH_components[,3] <- as.numeric(NSH_components[,3])
  
  NSH_components_new <- NSH_components[NSH_components$LAI_names == 'LAI_BUN',]
  NSH_components_new[,4] <- rep(toString(max(NSH_components_new[,1])),length(NSH_components_new[,1]))
  names(NSH_components_new)[4] <- 'year_retro'
  
  a <- rbind(a,NSH_components_new)
}

ggplot(a, aes(x= LAI_years, y = LAI_data, colour = year_retro)) + geom_line() + ylab("index") + xlab("years") + ggtitle('Buchan')

savePlot(paste(output.dir,assessment_name,"_LAI_BUN_retro.png",sep = ""),type="png")

# LAI_SNS
a <- list()
for(idxyear in 1:length(yearListRetro)){
  
  NSH_components  <- t(components(NSH.retro[[idxyear]]))
  NSH_residuals   <- residuals(NSH.retro[[idxyear]])
  LAI_BUN_residuals   <- subset(NSH_residuals, fleet == 'LAI-BUN')
  LAI_CNS_residuals   <- subset(NSH_residuals, fleet == 'LAI-CNS')
  LAI_ORSH_residuals   <- subset(NSH_residuals, fleet == 'LAI-ORSH')
  LAI_SNS_residuals   <- subset(NSH_residuals, fleet == 'LAI-SNS')
  
  yearList            <- data.frame(as.numeric(row.names(NSH_components)))
  names(yearList)     <- 'year'
  LAI_data <- c(NSH_components[,1], NSH_components[,2], NSH_components[,3], NSH_components[,4])
  LAI_names <- c(rep('LAI_ORSH',dim(NSH_components)[1]), rep('LAI_CNS',dim(NSH_components)[1]), rep('LAI_BUN',dim(NSH_components)[1]), rep('LAI_SNS',dim(NSH_components)[1]))
  LAI_years <- c(yearList$year, yearList$year, yearList$year, yearList$year)
  NSH_components <- as.data.frame(t(rbind(LAI_years,LAI_names,LAI_data)))
  
  NSH_components[,1] <- as.numeric(NSH_components[,1])
  NSH_components[,3] <- as.numeric(NSH_components[,3])
  
  NSH_components_new <- NSH_components[NSH_components$LAI_names == 'LAI_SNS',]
  NSH_components_new[,4] <- rep(toString(max(NSH_components_new[,1])),length(NSH_components_new[,1]))
  names(NSH_components_new)[4] <- 'year_retro'
  
  a <- rbind(a,NSH_components_new)
}

ggplot(a, aes(x= LAI_years, y = LAI_data, colour = year_retro)) + geom_line() + ylab("index") + xlab("years") + ggtitle('Downs')

savePlot(paste(output.dir,assessment_name,"_LAI_SNS_retro.png",sep = ""),type="png")

################################################################################
### ============================================================================
### Dump
### ============================================================================
################################################################################
# LAI components retrospective

yearListRetro <- names(NSH.retro)

idxyear <- 1

NSH_components  <- t(components(NSH.retro[[idxyear]]))
#NSH_residuals   <- residuals(NSH.retro[[idxyear]])
#LAI_BUN_residuals   <- subset(NSH_residuals, fleet == 'LAI-BUN')
#LAI_CNS_residuals   <- subset(NSH_residuals, fleet == 'LAI-CNS')
#LAI_ORSH_residuals   <- subset(NSH_residuals, fleet == 'LAI-ORSH')
#LAI_SNS_residuals   <- subset(NSH_residuals, fleet == 'LAI-SNS')

yearList            <- data.frame(as.numeric(row.names(NSH_components)))
names(yearList)     <- 'year'
LAI_data <- c(NSH_components[,1], NSH_components[,2], NSH_components[,3], NSH_components[,4])
LAI_names <- c(rep('LAI_ORSH',dim(NSH_components)[1]), rep('LAI_CNS',dim(NSH_components)[1]), rep('LAI_BUN',dim(NSH_components)[1]), rep('LAI_SNS',dim(NSH_components)[1]))
LAI_years <- c(yearList$year, yearList$year, yearList$year, yearList$year)
NSH_components <- as.data.frame(t(rbind(LAI_years,LAI_names,LAI_data)))

NSH_components[,1] <- as.numeric(NSH_components[,1])
NSH_components[,3] <- as.numeric(NSH_components[,3])

NSH_components <- NSH_components[NSH_components$LAI_names == 'LAI_ORSH',]
#a <- NSH_components[NSH_components$LAI_names == 'LAI_ORSH',]

g <- ggplot(NSH_components, aes(x= LAI_years, y = LAI_data, colour = LAI_names)) + geom_line() + ylab("index") + xlab("years")
print(g)