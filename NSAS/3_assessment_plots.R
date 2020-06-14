### ======================================================================================================
### Setting up
### ======================================================================================================
rm(list=ls())
graphics.off()

library(ggplot2)
library(FLSAM)
library(FLEDA)
path <- "C:/git/wg_HAWG/NSAS/"
path <- "J:/git/wg_HAWG/NSAS/"
try(setwd(path),silent=TRUE)

dir.create("assessment",showWarnings = FALSE)

setwd(file.path(path,'assessment'))

dir.create("plots_singlefleet",showWarnings = FALSE)
dir.create("plots_multifleet",showWarnings = FALSE)

setwd(path)


dataDir           <-  file.path(".","data/")
resPath           <-  file.path(".","assessment/")
output.dir.single <-  file.path(".","assessment/plots_singlefleet")
output.dir.multi  <-  file.path(".","assessment/plots_multifleet")
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
load(paste(resPath,"/NSH_HAWG2020_mf_retro.RData",sep=""))

load(paste(resPath,"/NSH_HAWG2020_sf.RData",sep=""))
load(paste(resPath,"/NSH_HAWG2020_mf.RData",sep=""))

### ============================================================================
### Model fit
### ============================================================================

# figure - residual plots at each age for each time series
#if(PDF) pdf(file.path(output.dir,paste(assessment_name_singlefleet,".pdf",sep="")))
#if(PNG) png(file.path(output.dir,paste(assessment_name_singlefleet,"_1_fit_diagnostics_%02d.png",sep="")),units = "px", height=800,width=672, bg = "white")

png(file.path(output.dir.single,paste(assessment_name_singlefleet,"_1_fit_diagnostics_%02d.png",sep="")),units = "px", height=800,width=672, bg = "white")

residual.diagnostics(NSH.sam)

dev.off()

windows()
# figure - assessment result, spawning stock biomass, fishing mortality, recruitment
print(plot(NSH.sam,futureYrs=F))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_2_stock_trajectory.png",sep = ""),type="png")

# figure - catchabilities at age from HERAS
catch <- catchabilities(NSH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
       scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
       type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
       subset=fleet %in% c("HERAS","IBTS-Q3"),
       main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_3_catchability_HERAS.png",sep = ""),type="png")

# figure - variance by data source
obv <- obs.var(NSH.sam)
obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
obv <- obv[order(obv$value),]
bp <- barplot(obv$value,ylab="Observation Variance",
              main="Observation variances by data source",col=factor(obv$fleet))
axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_4_observation_variance.png",sep = ""),type="png")

# figure - variance vs uncertainty for each data source
plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
     pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_5_observation_variance_CV.png",sep = ""),type="png")

# figure - fishing age selectivity per year
sel.pat <- merge(f(NSH.sam),fbar(NSH.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
xyplot(sel ~ age|sprintf("%i's",floor((year)/5)*5),sel.pat,
       groups=year,type="l",as.table=TRUE,
       scale=list(alternating=FALSE),
       main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar")

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_6_fishing_selectivity.png",sep = ""),type="png")

# figure - correlation matrix of model parameters
print(cor.plot(NSH.sam))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_7_SAM_prameter_correlation.png",sep = ""),type="png")

# figure - catch residuals per year per age
dat <- subset(residuals(NSH.sam),fleet=="catch unique")
print(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year Catch",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
       }))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_8_residuals_catches.png",sep = ""),type="png")

# figure - IBTS-Q1 index residuals per year per age
dat <- subset(residuals(NSH.sam),fleet=="IBTS-Q1")
print(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year IBTSQ1",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
       }))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_9_residuals_IBTSQ1.png",sep = ""),type="png")

# figure - acosutic index residuals per year per age
dat <- subset(residuals(NSH.sam),fleet=="HERAS")
print(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year HERAS",
             panel=function(...){
               lst <- list(...)
               panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
             }))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_10_residuals_HERAS.png",sep = ""),type="png")

# figure - acosutic index residuals per year per age
dat <- subset(residuals(NSH.sam),fleet=="IBTS-Q3")
print(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year IBTS-Q3",
             panel=function(...){
               lst <- list(...)
               panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=1*abs(lst$cex))
             }))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_11_residuals_IBTSQ3.png",sep = ""),type="png")

# process error in terms of N
print(procerr.plot(NSH+NSH.sam,weight="stock.wt",type="n",rel=T))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_12_process_error_N.png",sep = ""),type="png")

# process error in terms of additional mortality
print(procerr.plot(NSH+NSH.sam,weight="stock.wt",type="mort",rel=F))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_13_process_error_M.png",sep = ""),type="png")

# figure - times series for each age, stock
timeseries(window(NSH,1975,range(NSH)["maxyear"]),slot="stock.wt")

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_14_ts_stock_weight.png",sep = ""),type="png")

# figure - times series for each age, catches
timeseries(window(NSH,1975,range(NSH)["maxyear"]),slot="catch.wt")

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_15_ts_catch_weight.png",sep = ""),type="png")

# figure - times series for each age, harvest
print(timeseries(window(NSH,2000,range(NSH)["maxyear"]),slot="harvest"))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_16_ts_harvest.png",sep = ""),type="png")

# figure - times series for each age, maturity
print(timeseries(window(NSH,1990,range(NSH)["maxyear"]),slot="mat"))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_17_ts_mat.png",sep = ""),type="png")

# figure - times series for each age, mortality
print(timeseries(window(NSH,1947,range(NSH)["maxyear"]),slot="m"))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_18_ts_M.png",sep = ""),type="png")

## figure - acoustic index at age
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH.tun[["HERAS"]]@index)),groups="age",main="Proportion of Acoustic index at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_19_HERAS_prop.png",sep = ""),type="png")

#figure - TACs and catches
#TACs          <- read.csv(file.path(".","data","historic data","TAC-historic.csv"))
start_year    <- 1987
end_year      <- 2020
TACs          <- read.csv(file.path(".","data","TAC_var","NSAS_TAC.csv"))
TACs          <- TACs[TACs$year <= end_year,]
TAC.plot.dat  <- data.frame(year=rep(TACs$year,each=2)+c(-0.5,0.5),TAC=rep(rowSums(TACs[,c("A","B")],na.rm=T),each=2))
catch         <- as.data.frame(NSH@catch[,ac(start_year:end_year-1)])#
plot(0,0,pch=NA,xlab="Year",ylab="Catch",xlim=range(c(catch$year,TAC.plot.dat$year)),ylim=range(c(0,TAC.plot.dat$TAC,catch$data)),cex.lab=1.2,cex.axis=1.1,font=2)
rect(catch$year-0.5,0,catch$year+0.5,catch$data,col="grey")
lines(TAC.plot.dat,lwd=3)
legend("topright",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",pt.bg="grey",pt.cex=c(2),box.lty=0)
box()
title(main=paste(NSH@name,"Catch and TAC"))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_20_TAC_catches.png",sep = ""),type="png")

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

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_21_weight_stock_cohort.png",sep = ""),type="png")

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
  savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_22_survey_time_series_ages",iAge,".png",sep = ""),type="png")
}

# figure - catch number at age
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@catch.n)),groups="age",main="Proportion of Catch numbers at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_23_catch_N.png",sep = ""),type="png")

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

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_24_IBTS0_vs_IBTSQ1.png",sep = ""),type="png")

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

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_25_F_vs_SSB.png",sep = ""),type="png")


# internal consistency HERAS
print(plot(NSH.tun[["HERAS"]],type="internal"))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_26_HERAS_internal.png",sep = ""),type="png")

# internal consistency IBTS-Q3
print(plot(NSH.tun[["IBTS-Q3"]],type="internal"))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_27_IBTSQ3_internal.png",sep = ""),type="png")

# retro stock trajectory
print(plot(NSH.retro))
# print(plot(NSH.retro,xlim=c(2010,2020)))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_28_retro_stock.png",sep = ""),type="png")

# model parameters retrospective
print(retroParams(NSH.retro))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_29_retro_params.png",sep = ""),type="png")

# selectivity trajectory retrospective
print(retroSelectivity(NSH.retro,2009:range(NSH)["maxyear"]))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_30_retro_F.png",sep = ""),type="png")

# component proportions
print(comp.plot(NSH.sam))

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_31_LAI_prop.png",sep = ""),type="png")

# figure - otholith. Warning, this takes very long!
otolith(NSH.sam,n=1000)

savePlot(paste(output.dir.single,"/",assessment_name_singlefleet,"_32_otolith.png",sep = ""),type="png")

dev.off()

################################################################################
### ============================================================================
### Multi-fleet
### ============================================================================
################################################################################

if(PDF) pdf(file.path(output.dir.multi,paste(assessment_name_multifleet,"_1_fit_diagnostics_",sep="")))
if(PNG) png(file.path(output.dir.multi,paste(assessment_name_multifleet,"_1_fit_diagnostics_%02d.png",sep="")),units = "px", height=800,width=672, bg = "white")

# figure - residual plots at each age for each time series
residual.diagnostics(NSH3f.sam)


windows()
# figure - assessment result, spawning stock biomass, fishing mortality, recruitment
plot(NSH3f.sam,futureYrs=F)

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_2_stock_trajectory.png",sep = ""),type="png")

# figure - catchabilities at age from HERAS
catch <- catchabilities(NSH3f.sam)
xyplot(value+ubnd+lbnd ~ age | fleet,catch,
       scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
       type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
       subset=fleet %in% c("HERAS","IBTS-Q3"),
       main="Survey catchability parameters",ylab="Catchability",xlab="Age")

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_3_catchability_HERAS.png",sep = ""),type="png")

# figure - variance by data source
obv <- obs.var(NSH3f.sam)
obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
obv <- obv[order(obv$value),]
bp <- barplot(obv$value,ylab="Observation Variance",
              main="Observation variances by data source",col=factor(obv$fleet))
axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_4_variances.png",sep = ""),type="png")

# figure - variance vs uncertainty for each data source
plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
     pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_5_CV_vs_var.png",sep = ""),type="png")

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

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_6_fishing_selectivity_A.png",sep = ""),type="png")

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

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_6_fishing_selectivity_C.png",sep = ""),type="png")

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

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_6_fishing_selectivity_BD.png",sep = ""),type="png")

# fishing selectivity zoom
sel.pat <- merge(f(NSH3f.sam),fbar(NSH3f.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))

# Fleet A
selA <- subset(sel.pat, fleet == "catch A")
selA <- subset(selA, year > 2012)
for(idxYear in 1:length(selA$year)){selA$year[idxYear] <- toString(selA$year[idxYear])}

ggplot(selA, aes(x = age, y = sel, colour = year)) + geom_line() + xlab("age") + ylab("F/Fbar") + ggtitle("fleet A")

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_7_fishing_selectivity_zoom_A.png",sep = ""),type="png")

# Fleet BD
selBD <- subset(sel.pat, fleet == "catch BD")
selBD <- subset(selBD, year > 2012)
for(idxYear in 1:length(selBD$year)){selBD$year[idxYear] <- toString(selBD$year[idxYear])}

ggplot(selBD, aes(x = age, y = sel, colour = year)) + geom_line() + xlab("age") + ylab("F/Fbar") + ggtitle("fleet BD")

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_7_fishing_selectivity_zoom_BD.png",sep = ""),type="png")

# Fleet C
selC <- subset(sel.pat, fleet == "catch C")
selC <- subset(selC, year > 2012)
for(idxYear in 1:length(selC$year)){selC$year[idxYear] <- toString(selC$year[idxYear])}

ggplot(selC, aes(x = age, y = sel, colour = year)) + geom_line() + xlab("age") + ylab("F/Fbar") + ggtitle("fleet C")

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_7_fishing_selectivity_zoom_C.png",sep = ""),type="png")

# figure - correlation matrix of model parameters
cor.plot(NSH3f.sam)

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_8_corr_SAM_params.png",sep = ""),type="png")

# figure - catch residuals fleet A per year per age
dat <- subset(residuals(NSH3f.sam),fleet=="catch A")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year Catch fleet A",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       })

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_9_residuals_catch_fleet_A.png",sep = ""),type="png")

# figure - catch residuals fleet BD per year per age
dat <- subset(residuals(NSH3f.sam),fleet=="catch BD")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year Catch fleet BD",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       })

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_9_residuals_catch_fleet_BD.png",sep = ""),type="png")

# figure - catch residuals fleet BD per year per age
dat <- subset(residuals(NSH3f.sam),fleet=="catch C")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year Catch fleet C",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       })

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_9_residuals_catch_fleet_C.png",sep = ""),type="png")

# figure - IBTSQ1 index residuals per year per age
dat <- subset(residuals(NSH3f.sam),fleet=="IBTS-Q1")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year IBTSQ1",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       })

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_10_residuals_IBTSQ1.png",sep = ""),type="png")

# figure - IBTSQ3 index residuals per year per age
dat <- subset(residuals(NSH3f.sam),fleet=="IBTS-Q3")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year IBTSQ3",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       })

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_11_residuals_IBTSQ3.png",sep = ""),type="png")

# figure - HERAS index residuals per year per age
dat <- subset(residuals(NSH3f.sam),fleet=="HERAS")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year HERAS",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       })

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_12_residuals_HERAS.png",sep = ""),type="png")

# retro stock trajectory
plot(NSH3f.retro)

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_13_retro.png",sep = ""),type="png")

# SSB comparison
plotQuant <- ssb(NSH.sam)
plotQuant2 <- ssb(NSH3f.sam)
years <- plotQuant$year

plot(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),xlab='year',pch = ".",ylab='SSB')
lines(years, plotQuant$value, type="l",lwd=2,col=rgb(0,0,1))
polygon(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),col=rgb(0,0,1,0.5),lty=0)

lines(years, plotQuant2$value, type="l",lwd=2,col=rgb(1,0,0))
polygon(c(years,rev(years)),c(plotQuant2$lbnd,rev(plotQuant2$ubnd)),col=rgb(1,0,0,0.5),lty=0)

legend("topright",legend=c("single fleet","multi-fleet"),col=c(rgb(0,0,1),rgb(1,0,0)),lty=1,lwd=2)

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_14_comp_single_fleet_SSB.png",sep = ""),type="png")

# fbar comparison
plotQuant <- fbar(NSH.sam)
plotQuant2 <- fbar(NSH3f.sam)
years <- plotQuant$year

plot(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),xlab='year',pch = ".",ylab='fbar')
lines(years, plotQuant$value, type="l",lwd=2,col=rgb(0,0,1))
polygon(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),col=rgb(0,0,1,0.5),lty=0)

lines(years, plotQuant2$value, type="l",lwd=2,col=rgb(1,0,0))
polygon(c(years,rev(years)),c(plotQuant2$lbnd,rev(plotQuant2$ubnd)),col=rgb(1,0,0,0.5),lty=0)

legend("topright",legend=c("single fleet","multi-fleet"),col=c(rgb(0,0,1),rgb(1,0,0)),lty=1,lwd=2)

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_14_comp_single_fleet_fbar.png",sep = ""),type="png")

# recruitment
plotQuant <- rec(NSH.sam)
plotQuant2 <- rec(NSH3f.sam)
years <- plotQuant$year

plot(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),xlab='year',pch = ".",ylab='recruitment')
lines(years, plotQuant$value, type="l",lwd=2,col=rgb(0,0,1))
polygon(c(years,rev(years)),c(plotQuant$lbnd,rev(plotQuant$ubnd)),col=rgb(0,0,1,0.5),lty=0)

lines(years, plotQuant2$value, type="l",lwd=2,col=rgb(1,0,0))
polygon(c(years,rev(years)),c(plotQuant2$lbnd,rev(plotQuant2$ubnd)),col=rgb(1,0,0,0.5),lty=0)

savePlot(paste(output.dir.multi,"/",assessment_name_multifleet,"_14_comp_single_fleet_rec.png",sep = ""),type="png")

# process error in terms of N
#procerr.plot(NSHs3,weight="stock.wt",type="n",rel=T)

#savePlot(paste(output.dir,"/",assessment_name_multifleet,"_10_process_error_N.png",sep = ""),type="png")

# process error in terms of additional mortality
#procerr.plot(NSH+NSH3f.sam,weight="stock.wt",type="mort",rel=F)

#savePlot(paste(output.dir,"/",assessment_name_multifleet,"_11_process_error_M.png",sep = ""),type="png")

dev.off()


