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


#- 3-fleets
sweep(yearSums(NSHs3[[1]]@catch.n),1,areaSums(yearSums(NSHs3[[1]]@catch.n)),"/")

NSH3.ctrl <- FLSAM.control(NSHs3,NSH.tun,sumFleets=dimnames(NSHs3[["residual"]]@catch)$area)

  catchRow                          <- grep("catch",rownames(NSH3.ctrl@f.vars))
NSH3.ctrl@states["catch A",]        <- c(-1,0:6,6)
NSH3.ctrl@states["catch BD",]       <- c(7:9,10,10,10,rep(-1,3))
NSH3.ctrl@states["catch C",]        <- c(-1,11:13,14,14,14,rep(-1,2))
NSH3.ctrl@catchabilities["HERAS",ac(1:8)]   <- c(101,102,rep(103,6))
NSH3.ctrl@catchabilities["IBTS-Q3",ac(0:5)] <- c(200:204,205)
NSH3.ctrl@obs.vars["catch A",]      <- c(-1,0,1,1,1,1,2,2,2)
NSH3.ctrl@obs.vars["catch BD",]     <- c(301,302,rep(303,4),rep(-1,3))
NSH3.ctrl@obs.vars["catch C",]      <- c(-1,401,402,rep(403,4),rep(-1,2))
NSH3.ctrl@obs.vars["IBTS0",ac(0)]   <- 801
NSH3.ctrl@obs.vars["IBTS-Q1",ac(1)] <- 501
NSH3.ctrl@obs.vars["IBTS-Q3",ac(0:5)]<- c(200,201,202,202,202,202)
NSH3.ctrl@obs.vars["HERAS",ac(1:8)] <- c(101,102,rep(102,4),rep(103,2))
  laiRow                            <- grep("LAI",rownames(NSH3.ctrl@obs.vars))
NSH3.ctrl@obs.vars[laiRow,1]        <- 601#:604
NSH3.ctrl@catchabilities[laiRow,1]  <- 601
  idx                               <- which(rownames(NSH3.ctrl@cor.obs)=="IBTS-Q3")
NSH3.ctrl@cor.obs[idx,1:5]          <- c(rep(102,5))
NSH3.ctrl@cor.obs.Flag[idx]         <- as.factor("AR")
NSH3.ctrl@cor.F                     <- as.integer(c(2,2,2))
NSH3.ctrl@f.vars["catch A",]        <- c(-1,rep(102,5),rep(103,3))
NSH3.ctrl@f.vars["catch BD",]       <- c(202,203,rep(203,4),rep(-1,3))
NSH3.ctrl@f.vars["catch C",]        <- c(-1,301,302,rep(303,4),rep(-1,2))
NSH3.ctrl@name                      <- "Three fleet model"
NSH3.ctrl                           <- update(NSH3.ctrl)


#- 4-fleets
#NSH4.ctrl <- FLSAM.control(NSHs4,NSH.tun,sumFleets=dimnames(NSHs4[["residual"]]@catch)$area)
#
#catchRow                            <- grep("catch",rownames(NSH4.ctrl@f.vars))
#NSH4.ctrl@states["catch A",]        <- c(-1,0:6,6)
#NSH4.ctrl@states["catch B",]        <- c(7:8,8,rep(-1,6))
#NSH4.ctrl@states["catch C",]        <- c(9:16,16)
#NSH4.ctrl@states["catch D",]        <- c(17:18,18,rep(-1,6))
#NSH4.ctrl@catchabilities["HERAS",ac(2:8)]   <- c(rep(101,2),rep(102,3),rep(103,2))
#NSH4.ctrl@catchabilities["IBTS-Q3",ac(0:4)] <-  c(201,202,202,203,203)
#NSH4.ctrl@obs.vars["catch A",]      <- c(rep(0,4),rep(1,5))
#NSH4.ctrl@obs.vars["catch B",]      <- c(rep(2,4),rep(-1,5))
#NSH4.ctrl@obs.vars["catch C",]      <- c(rep(3,4),rep(4,5))
#NSH4.ctrl@obs.vars["catch D",]      <- c(rep(5,4),rep(-1,5))
#NSH4.ctrl@obs.vars["IBTS-Q1",ac(1)] <- 501
#NSH4.ctrl@obs.vars["IBTS-Q3",ac(0:4)]<- c(200:203,203)
#NSH4.ctrl@obs.vars["HERAS",ac(2:8)] <- c(rep(400,3),rep(401,4))
#laiRow                              <- grep("LAI",rownames(NSH4.ctrl@obs.vars))
#NSH4.ctrl@catchabilities[laiRow,1]  <- 601
##NSH4.ctrl@power.law.exps[laiRow,1]  <- 101
#NSH4.ctrl@obs.vars[laiRow,1]        <- 601:604
#NSH4.ctrl                           <- update(NSH4.ctrl)
##NSH4.ctrl@cor.F                     <- as.integer(c(2,0,2))
##NSH4.ctrl@f.vars[catchRow,]         <- matrix(c(c(rep(0,4),rep(1,5)),c(rep(2,4),rep(-1,5)),c(rep(3,4),rep(4,5)),c(rep(5,4),rep(-1,5))),nrow=length(catchRow),byrow=T)
#NSH4.ctrl@name                      <- "Four fleet model"
#