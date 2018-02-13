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
NSH.ctrl@obs.vars[catchRow,]  <- c(0,0,1,1,2,2,2,3,3)
NSH.ctrl@obs.vars["IBTS0",ac(0)]   <- 801
NSH.ctrl@obs.vars["IBTS-Q1",ac(1)] <- 501
NSH.ctrl@obs.vars["IBTS-Q3",ac(0:4)]<- 200
#NSH.ctrl@obs.vars["HERAS",ac(2:8)] <- c(rep(101,5),102,102)
NSH.ctrl@obs.vars[laiRow,1]        <- 601#:604


NSH.ctrl@catchabilities[laiRow,1] <- 401
#NSH.ctrl@catchabilities[laiRow,1]  <- 601
NSH.ctrl@catchabilities["HERAS",ac(2:8)]   <- c(rep(101,1),rep(102,2),rep(103,4))
NSH.ctrl@catchabilities["IBTS-Q3",ac(0:4)] <- c(201,202,202,203,203)
idx <- which(rownames(NSH.ctrl@cor.obs)=="IBTS-Q3")
NSH.ctrl@cor.obs[idx,1:4] <- 1
NSH.ctrl@cor.obs.Flag[idx] <- as.factor("AR")
NSH.ctrl@f.vars[1,]         <- c(101,101,rep(102,4),rep(103,3))
#NSH.ctrl@catchabilities["HERAS",ac(1:8)]  <- 101:108
#NSH.ctrl@power.law.exps[laiRow,1] <- 101
#Group observation variances of catches to ensure stability
NSH.ctrl@obs.vars[catchRow,]  <- c(rep(0,4),rep(1,5))
NSH.ctrl@obs.vars["IBTS-Q3",ac(0:4)] <-  c(201,202,202,203,203)
NSH.ctrl@obs.vars["HERAS",ac(2:8)]   <- c(rep(101,2),rep(102,3),rep(103,2))

laiRow <- grep("LAI",rownames(NSH.ctrl@catchabilities))
#NSH.ctrl@catchabilities[laiRow,1] <- 201
#NSH.ctrl@obs.vars[laiRow,1] <- 401:404
#NSH.ctrl@catchabilities["HERAS",ac(1:8)]  <- 101:108
#NSH.ctrl@power.law.exps[laiRow,1] <- 101

#Finalise
NSH.ctrl@name <- "Final Assessment"
NSH.ctrl <- update(NSH.ctrl)
#NSH.ctrl@f.vars["IBTS0",1]         <- -3
NSH.ctrl@residuals <- F

