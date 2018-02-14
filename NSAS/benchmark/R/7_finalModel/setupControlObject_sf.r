################################################################################
# NSH_SAM Control for Assessment
#
# $Rev: 705 $
# $Date: 2012-02-14 19:02:57 +0100 (di, 14 feb 2012) $
#
# Author: HAWG model devlopment group
#
# Sets up a control object for use by Step 04 assessments i.e. the "refined data" run
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

NSH.ctrl <- FLSAM.control(NSH,NSH.tun)

#Set the variances. Separate variance for recruitment and plus group
#Fishing mortality RWs are set from an analysis of ICA VPA results
NSH.ctrl@logN.vars[]      <- c(1,rep(2,dims(NSH)$age-1))
catchRow <- grep("catch",rownames(NSH.ctrl@f.vars))
#NSH.ctrl@f.vars[catchRow,] <- c(rep(1,2),rep(2,2),rep(3,2),rep(4,3))

#All fishing mortality states are free except
#oldest ages to ensure stablity
NSH.ctrl@states[catchRow,] <- seq(dims(NSH)$age)
NSH.ctrl@states[catchRow,ac(7:8)] <- 101

laiRow                              <- grep("LAI",rownames(NSH.ctrl@obs.vars))
#Group observation variances of catches to ensure stability
NSH.ctrl@obs.vars[catchRow,]  <- c(0,0,1,1,1,1,1,2,2)
NSH.ctrl@obs.vars["IBTS0",ac(0)]   <- 801
NSH.ctrl@obs.vars["IBTS-Q1",ac(1:5)] <- c(501,rep(502,2),rep(503,2))
NSH.ctrl@obs.vars["IBTS-Q3",ac(0:6)]<- c(200,201,202,202,202,202,202)
NSH.ctrl@obs.vars["HERAS",ac(1:8)] <- c(101,rep(102,2),rep(103,3),rep(104,2))
NSH.ctrl@obs.vars[laiRow,1]        <- 601

NSH.ctrl@catchabilities[laiRow,1] <- 401
NSH.ctrl@catchabilities["HERAS",ac(1:8)]   <- c(rep(101,2),rep(102,2),rep(103,4))
NSH.ctrl@catchabilities["IBTS-Q3",ac(0:6)] <- c(200:204,204,204)
NSH.ctrl@catchabilities["IBTS-Q1",ac(1:5)] <- c(301:304,304)
idx <- which(rownames(NSH.ctrl@cor.obs)=="IBTS-Q3")
NSH.ctrl@cor.obs[idx,1:6] <- c(rep(102,6))
NSH.ctrl@cor.obs.Flag[idx] <- as.factor("AR")

idx <- which(rownames(NSH.ctrl@cor.obs)=="IBTS-Q1")
NSH.ctrl@cor.obs[idx,2:5] <- c(2,rep(3,3))
NSH.ctrl@cor.obs.Flag[idx] <- as.factor("AR")

#idx <- which(rownames(NSH.ctrl@cor.obs)=="HERAS")
#NSH.ctrl@cor.obs[idx,2:8] <- c(4,rep(5,6))
#NSH.ctrl@cor.obs.Flag[idx] <- as.factor("AR")

NSH.ctrl@f.vars[1,]         <- c(101,101,rep(102,4),rep(103,3))
#NSH.ctrl@catchabilities["HERAS",ac(1:8)]  <- 101:108
#NSH.ctrl@power.law.exps[laiRow,1] <- 101

NSH.ctrl@cor.F              <- 0
#Finalise
NSH.ctrl@name <- "Final Assessment"
NSH.ctrl <- update(NSH.ctrl)
#NSH.ctrl@f.vars["IBTS0",1]         <- -3
NSH.ctrl@residuals <- F

