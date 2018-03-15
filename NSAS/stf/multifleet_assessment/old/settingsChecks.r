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
