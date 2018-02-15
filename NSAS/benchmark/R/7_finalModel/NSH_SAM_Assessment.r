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
path <- "D:/Repository/ICES_HAWG/wg_HAWG/NSAS/benchmark/"
try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
output.dir          <-  file.path(".","results/7_finalModel/")        # figures directory
output.base         <-  file.path(output.dir,"NSH Assessment")  # Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  10                                      # Number of years for which to run the retrospective


### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA)
source(file.path("R/7_finalModel/setupAssessmentObjects.r"))
source(file.path("R/7_finalModel/setupControlObject_sf.r"))
source(file.path("R/7_finalModel/plot.cordata.r"))
### ============================================================================
### ============================================================================
### ============================================================================
### Run the assessment
### ============================================================================
### ============================================================================
### ============================================================================

NSH.samCorall                             <- FLSAM(NSH,NSH.tun,NSH.ctrl)
NSH.retroCorall                           <- retro(NSH,NSH.tun,NSH.ctrl,7)
mean(mohns.rho(NSH.retroCorall,ref.year=2016,span=7,type="fbar")[1:7,1]) #-29.9
source(file.path("R/7_finalModel/setupAssessmentObjects.r"))
source(file.path("R/7_finalModel/setupControlObject_sf.r"))
NSH.ctrl@cor.obs["HERAS",]                <- -1
NSH.ctrl@cor.obs.Flag[2]                  <- as.factor("ID")
NSH.ctrl                                  <- update(NSH.ctrl)
NSH.samdCorH                              <- FLSAM(NSH,NSH.tun,NSH.ctrl)
NSH.retrodCorH                            <- retro(NSH,NSH.tun,NSH.ctrl,7)
mean(mohns.rho(NSH.retrodCorH,ref.year=2016,span=7,type="fbar")[1:7,1]) #-10.0
source(file.path("R/7_finalModel/setupAssessmentObjects.r"))
source(file.path("R/7_finalModel/setupControlObject_sf.r"))
NSH.ctrl@cor.obs["IBTS-Q1",]              <- -1
NSH.ctrl@cor.obs.Flag[3]                  <- as.factor("ID")
NSH.ctrl                                  <- update(NSH.ctrl)
NSH.samdCorQ1                             <- FLSAM(NSH,NSH.tun,NSH.ctrl)
NSH.retrodCorQ1                           <- retro(NSH,NSH.tun,NSH.ctrl,7)
mean(mohns.rho(NSH.retrodCorQ1,ref.year=2016,span=7,type="fbar")[1:7,1]) #-31.9
source(file.path("R/7_finalModel/setupAssessmentObjects.r"))
source(file.path("R/7_finalModel/setupControlObject_sf.r"))
NSH.ctrl@cor.obs["IBTS-Q3",]              <- -1
NSH.ctrl@cor.obs.Flag[5]                  <- as.factor("ID")
NSH.ctrl                                  <- update(NSH.ctrl)
NSH.samdCorQ3                             <- FLSAM(NSH,NSH.tun,NSH.ctrl)
NSH.retrodCorQ3                           <- retro(NSH,NSH.tun,NSH.ctrl,7)
mean(mohns.rho(NSH.retrodCorQ3,ref.year=2016,span=7,type="fbar")[1:7,1]) #-23.5
AIC(FLSAMs(allcor=NSH.samCorall,dropHeras=NSH.samdCorH,dropIBTSQ1=NSH.samdCorQ1,dropIBTSQ3=NSH.samdCorQ3))
save(NSH.samCorall,NSH.samdCorH,NSH.samdCorQ1,NSH.samdCorQ3,file=file.path(output.dir,"NSH cor checks.RData"))

#- Base run
source(file.path("R/7_finalModel/setupAssessmentObjects.r"))
source(file.path("R/7_finalModel/setupControlObject_sf.r"))
NSH.ctrl@residuals                  <- TRUE
NSH.sam                             <- FLSAM(NSH,NSH.tun,NSH.ctrl)
name(NSH.sam)                       <- "All data in"
NSH.ctrl@residuals                  <- FALSE
NSH.retro                           <- retro(NSH,NSH.tun,NSH.ctrl,7)
mean(mohns.rho(NSH.retro,ref.year=2016,span=7,type="fbar")[1:7,1]) #-10.0
mean(mohns.rho(NSH.retro,ref.year=2016,span=7,type="ssb")[1:7,1])  # 11.7
mean(mohns.rho(NSH.retro,ref.year=2016,span=7,type="rec")[1:7,1])  # 12.5
xyplot(log(value) ~ an(name) | fleet,data=catchabilities(NSH.retro),group=age,type="b",scales=list(y="free"))
save(NSH,NSH.tun,NSH.ctrl,NSH.sam,NSH.retro,file=file.path(output.dir,paste("NSH_",name(NSH.sam),".RData",sep="")))

#- Drop IBTS-Q3 age 5-6
source(file.path("R/7_finalModel/setupAssessmentObjects.r"))
source(file.path("R/7_finalModel/setupControlObject_sf.r"))
NSH.ctrl@residuals                  <- TRUE
NSH.tun[["IBTS-Q3"]]                <- trim(NSH.tun[["IBTS-Q3"]],age=0:4)
NSH.ctrl@catchabilities["IBTS-Q3",] <- c(3,4,5,6,6,rep(-1,4))
NSH.ctrl@obs.vars["IBTS-Q3",]       <- c(7,8,9,9,9,rep(-1,4))
NSH.ctrl@cor.obs["IBTS-Q3",]        <- c(rep(0,4),rep(-1,4))
NSH.ctrl                            <- update(NSH.ctrl)
NSH.samdq3_56                       <- FLSAM(NSH,NSH.tun,NSH.ctrl)
name(NSH.samdq3_56)                 <- "Drop IBTS Q3 age 5_6"
NSH.ctrl@residuals                  <- FALSE
NSH.retrodq3_56                     <- retro(NSH,NSH.tun,NSH.ctrl,7)
mean(mohns.rho(NSH.retrodq3_56,ref.year=2016,span=7,type="fbar")[1:7,1]) #-15.8
mean(mohns.rho(NSH.retrodq3_56,ref.year=2016,span=7,type="ssb")[1:7,1])  # 18.1
mean(mohns.rho(NSH.retrodq3_56,ref.year=2016,span=7,type="rec")[1:7,1])  # 18.0
xyplot(value ~ an(name) | fleet,data=catchabilities(NSH.retrodq3_56),group=age,type="b",scales=list(y="free"))
save(NSH,NSH.tun,NSH.ctrl,NSH.samdq3_56,NSH.retrodq3_56,file=file.path(output.dir,paste("NSH_",name(NSH.samdq3_56),".RData",sep="")))


#- Drop IBTS-Q1 age 2-5
source(file.path("R/7_finalModel/setupAssessmentObjects.r"))
source(file.path("R/7_finalModel/setupControlObject_sf.r"))
NSH.ctrl@residuals                  <- TRUE
NSH.tun[["IBTS-Q1"]]                <- trim(NSH.tun[["IBTS-Q1"]],age=1)
NSH.ctrl@catchabilities["IBTS-Q1",] <- c(-1,101,rep(-1,7))
NSH.ctrl@obs.vars["IBTS-Q1",]       <- c(-1,101,rep(-1,7))
NSH.ctrl@cor.obs["IBTS-Q1",]        <- rep(-1,8)
NSH.ctrl@cor.obs.Flag[which(rownames(NSH.ctrl@cor.obs)=="IBTS-Q1")] <- "ID"
NSH.ctrl                            <- update(NSH.ctrl)
NSH.samdq1_25                       <- FLSAM(NSH,NSH.tun,NSH.ctrl)
name(NSH.samdq1_25)                 <- "Drop IBTS Q1 age 2_5"
NSH.ctrl@residuals                  <- FALSE
NSH.retrodq1_25                     <- retro(NSH,NSH.tun,NSH.ctrl,7)
mean(mohns.rho(NSH.retrodq1_25,ref.year=2016,span=7,type="fbar")[1:7,1]) #-9.4
mean(mohns.rho(NSH.retrodq1_25,ref.year=2016,span=7,type="ssb")[1:7,1])  # 10.8
mean(mohns.rho(NSH.retrodq1_25,ref.year=2016,span=7,type="rec")[1:7,1])  # 12.2
save(NSH,NSH.tun,NSH.ctrl,NSH.samdq1_25,NSH.retrodq1_25,file=file.path(output.dir,paste("NSH_",name(NSH.samdq1_25),".RData",sep="")))
xyplot(log(value) ~ an(name) | fleet,data=catchabilities(NSH.retrodq1_25),group=age,type="b",scales=list(y="free"))
plot.cordata(NSH,NSH.tun,age=2:5)

#- Drop HERAS 1
source(file.path("R/7_finalModel/setupAssessmentObjects.r"))
source(file.path("R/7_finalModel/setupControlObject_sf.r"))
NSH.ctrl@residuals                  <- TRUE
NSH.tun[["HERAS"]]                  <- trim(NSH.tun[["HERAS"]],age=2:8)
NSH.ctrl@catchabilities["HERAS",2]  <- -1
NSH.ctrl@obs.vars["HERAS",2]        <- -1
NSH.ctrl                            <- update(NSH.ctrl)
NSH.samdh1                       <- FLSAM(NSH,NSH.tun,NSH.ctrl)
name(NSH.samdh1)                 <- "Drop IBTS Q1 age 2_5"
NSH.ctrl@residuals                  <- FALSE
NSH.retrodh1                     <- retro(NSH,NSH.tun,NSH.ctrl,7)
mean(mohns.rho(NSH.retrodh1,ref.year=2016,span=7,type="fbar")[1:7,1]) #-12.1
mean(mohns.rho(NSH.retrodh1,ref.year=2016,span=7,type="ssb")[1:7,1])  # 13.5
mean(mohns.rho(NSH.retrodh1,ref.year=2016,span=7,type="rec")[1:7,1])  # 12.7
save(NSH,NSH.tun,NSH.ctrl,NSH.samdh1,NSH.retrodh1,file=file.path(output.dir,paste("NSH_",name(NSH.samdh1),".RData",sep="")))
#plot.cordata(NSH,NSH.tun,age=2:5)

#- Drop IBTS-Q3 age 0 and IBTS-Q1 age 2-5
source(file.path("R/7_finalModel/setupAssessmentObjects.r"))
source(file.path("R/7_finalModel/setupControlObject_sf.r"))
NSH.ctrl@residuals                  <- TRUE
NSH.tun[["IBTS-Q1"]]                <- trim(NSH.tun[["IBTS-Q1"]],age=1)
NSH.ctrl@catchabilities["IBTS-Q1",] <- c(-1,101,rep(-1,7))
NSH.ctrl@obs.vars["IBTS-Q1",]       <- c(-1,101,rep(-1,7))
NSH.ctrl@cor.obs["IBTS-Q1",]        <- rep(-1,8)
NSH.ctrl@cor.obs.Flag[which(rownames(NSH.ctrl@cor.obs)=="IBTS-Q1")] <- "ID"
NSH.tun[["IBTS-Q3"]]                <- trim(NSH.tun[["IBTS-Q3"]],age=1:6)
NSH.ctrl@catchabilities["IBTS-Q3",1]<- -1
NSH.ctrl@obs.vars["IBTS-Q3",1]      <- -1
NSH.ctrl@cor.obs["IBTS-Q3",1]       <- -1
NSH.ctrl                            <- update(NSH.ctrl)
NSH.samdq1_25_q3_0                  <- FLSAM(NSH,NSH.tun,NSH.ctrl)
name(NSH.samdq1_25_q3_0)            <- "Drop IBTS Q1 age 2_5 IBTS Q3 age 0"
NSH.ctrl@residuals                  <- FALSE
NSH.retrodq1_25_q3_0                <- retro(NSH,NSH.tun,NSH.ctrl,7)
mean(mohns.rho(NSH.retrodq1_25_q3_0,ref.year=2016,span=7,type="fbar")[1:7,1]) #-9.4
mean(mohns.rho(NSH.retrodq1_25_q3_0,ref.year=2016,span=7,type="ssb")[1:7,1])  # 10.7
mean(mohns.rho(NSH.retrodq1_25_q3_0,ref.year=2016,span=7,type="rec")[1:7,1])  # 13.2
save(NSH,NSH.tun,NSH.ctrl,NSH.samdq1_25,NSH.retrodq1_25,file=file.path(output.dir,paste("NSH_",name(NSH.samdq1_25),".RData",sep="")))


#####
#- Calculate the difference in output from single vs multifleet
#####

#-difference in selection pattern
NSHs3[[1]]@harvest <- NSH3f.sam@harvest[,ac(1997:2016)]
a <- as.data.frame(NSHs3[[1]]@harvest) #real f
b <- as.data.frame(sweep(sweep(NSHs3[[1]]@catch.n,c(1:4,6),areaSums(NSHs3[[1]]@catch.n),"/"),c(1:4,6),areaSums(NSHs3[[1]]@harvest),"*")) #approx f

comb <- rbind(cbind(type="single",b),cbind(type="multi",a))
 xyplot(data ~ an(age) | as.factor(year)*as.factor(area),data=subset(comb,year%in%2011:2016),type="l",group=type,scales=list(y="free"),xlab="Age",ylab="F at age",main="Selection at age by fleet",auto.key=T,lwd=2)
 
#-difference in catch
a <- as.data.frame(quantSums(sweep(sweep(NSHs3[[1]]@harvest,c(1:4,6),areaSums(NSHs3[[1]]@harvest + NSHs3[[1]]@m),"/") * NSHs3[[1]]@stock.n * NSHs3[[1]]@catch.wt,c(1:4,6),(1-exp(-areaSums(NSHs3[[1]]@harvest + NSHs3[[1]]@m))),"*"))) #real catch
approxfb <- sweep(sweep(NSHs3[[1]]@catch.n,c(1:4,6),areaSums(NSHs3[[1]]@catch.n),"/"),c(1:4,6),areaSums(NSHs3[[1]]@harvest),"*")
b <- as.data.frame(quantSums(sweep(sweep(approxfb,c(1:4,6),areaSums(approxfb + NSHs3[[1]]@m),"/") * NSHs3[[1]]@stock.n * NSHs3[[1]]@catch.wt,c(1:4,6),(1-exp(-areaSums(approxfb + NSHs3[[1]]@m))),"*"))) #real catch
# approx catch
comb <-rbind(cbind(type="single",b),cbind(type="multi",a))
xyplot(data ~ an(year)|as.factor(area),data=comb,type="l",auto.key=T,group=type,scales=list(y="free"),main="Catch by fleet",xlab="Years",ylab="Catch in tonnes",lwd=2)

#-error in catch
comb <- cbind(subset(comb,type=="single"),subset(comb,type=="multi")[,"data"])
colnames(comb)[ncol(comb)] <- "datamulti"
comb$error <- (comb$data - comb$datamulti)/comb$datamulti*100

xyplot(error ~ an(year) | as.factor(area),data=comb,type="l",auto.key=T,scales=list(y="free"),main="Error in catch forecast",xlab="Years",ylab="Percentage error",lwd=2,
panel=function(...){
  panel.xyplot(...)
  panel.abline(h=0,lwd=2,lty=2)
  panel.grid()
  })

#Setup plots
#pdf(file.path(output.dir,paste(name(NSH.sam),".pdf",sep="")))
png(file.path(output.dir,paste(name(NSH.sam),"figures - %02d.png")),units = "px", height=800,wi
dth=672, bg = "white")

### ============================================================================
### Model fit
### ============================================================================

# figure - residual plots at each age for each time series
residual.diagnostics(NSH.sam)

# figure - assessment result, spawning stock biomass, fishing mortality, recruitment
print(plot(NSH.sam,futureYrs=F))

# figure - uncertainties as a function of time
par(mfrow=c(1,2))
CV.yrs <- ssb(NSH.sam)$year
CV.dat <- cbind(SSB=ssb(NSH.sam)$CV,
                Fbar=fbar(NSH.sam)$CV,Rec=rec(NSH.sam)$CV)
matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
        xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
grid()
legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")
CV.yrs <- ssb(NSH3f.sam)$year
CV.dat <- cbind(SSB=ssb(NSH3f.sam)$CV,
                Fbar=fbar(NSH3f.sam)$CV,Rec=rec(NSH3f.sam)$CV)
matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
        xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
grid()
legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")
CV.yrs <- ssb(NSH4f.sam)$year
CV.dat <- cbind(SSB=ssb(NSH4f.sam)$CV,
                Fbar=fbar(NSH4f.sam)$CV,Rec=rec(NSH4f.sam)$CV)
matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
        xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
grid()
legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")

# figure - catchabilities at age from HERAS
catch <- catchabilities(NSH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
             scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
             type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
             subset=fleet %in% c("HERAS","IBTS-Q1","IBTS-Q3"),
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
sel.pat <- merge(f(NSH3f.sam),fbar(NSH3f.sam),
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

print(procerr.plot(NSH+NSH.sam,weight="stock.wt",type="n",rel=T)) #in terms of N
print(procerr.plot(NSH+NSH.sam,weight="stock.wt",type="mort",rel=F)) #in terms of additional mortality
print(plot.kobe(NSH.sam,fmsy=0.31,bmsy=1.5e6))


print(plot(NSH.retro))
print(retroParams(NSH.retro))
print(retroSelectivity(NSH.retro,2009:2017))

plot(mean(mohns.rho(NSH.retro,ref.year=2016,span=7,type="fbar")[1:7,1]),ylab="Mohns rho",xlab="")
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
NSH.ctrl@residuals <- F
NSH.retro <- retro(NSH,NSH.tun,NSH.ctrl,retro=7)
save(NSH.retro,file=file.path(output.dir,"NSHretro.RData",sep=""))

### ============================================================================
### figures
### ============================================================================
#Setup plots
png(file.path(output.dir,paste(name(NSH.sam),"figures_retro - %02d.png")),units = "px", height=800,width=672, bg = "white")

# figure - retrospective for SSB, Fbar, rec
print(plot(NSH.retro,futureYrs=F))
print(retroParams(NSH.retro))
print(retroSelectivity( NSH.retro,2009:2016))

#print(lapply(NSH.retro,retroParam))
# closing pdf and png print
dev.off()