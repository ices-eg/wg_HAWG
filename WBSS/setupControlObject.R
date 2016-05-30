################################################################################
# WBSS SAM control obj for assessment
#
# 10 March 2015
# HAWG working group
#
################################################################################

# Set the SAM controlObj
stk.ctrl <- FLSAM.control(stk,wbss.tun)
slotNames(stk.ctrl)

# point to the same tpl file used in the web-interface assessment
stk.ctrl@sam.binary <- "model/sam"

# set states
stk.ctrl@states[]["catch",] <- c(1:5,rep(6,4))

# correlated random walk
stk.ctrl@cor.F[] <- TRUE

# set catchabilities
## stk.ctrl@catchabilities[]["HERAS",] <- c(NA,1,rep(2,3),rep(3,4))
## stk.ctrl@catchabilities[]["GerAS",] <- c(4,rep(5,8))
## stk.ctrl@catchabilities[]["N20",] <- c(6,rep(NA,8))
## stk.ctrl@catchabilities[]["IBTS Q1",] <- c(NA,7,7,8,8,rep(NA,4))
## stk.ctrl@catchabilities[]["IBTS Q3",] <- c(NA,9,10,10,11,rep(NA,4))
stk.ctrl@catchabilities[]["HERAS",] <- c(NA,1:7,7)
stk.ctrl@catchabilities[]["GerAS",] <- c(8:15,15)
stk.ctrl@catchabilities[]["N20",] <- c(16,rep(NA,8))
stk.ctrl@catchabilities[]["IBTS Q1",] <- c(NA,17:20,rep(NA,4))
stk.ctrl@catchabilities[]["IBTS Q3",] <- c(NA,21:24,rep(NA,4))


stk.ctrl@power.law.exps

# set fishing mortality RW variance
stk.ctrl@f.vars[]["catch",] <- c(1,rep(2,8))

# set log N RW variances
stk.ctrl@logN.vars[] <- rep(1,9)

# set observation variances
stk.ctrl@obs.vars[]["catch",] <- c(1,rep(2,4),rep(3,4))
stk.ctrl@obs.vars[]["HERAS",] <- c(NA,4,5,rep(6,4),7,7)
stk.ctrl@obs.vars[]["GerAS",] <- c(rep(8,4),9,9,rep(10,3))
stk.ctrl@obs.vars[]["N20",] <- c(11,rep(NA,8))
stk.ctrl@obs.vars[]["IBTS Q1",] <- c(NA,rep(12,4),rep(NA,4))
stk.ctrl@obs.vars[]["IBTS Q3",] <- c(NA,13,13,14,14,rep(NA,4))

#Finalise
stk.ctrl@name <- "WBSSher"
stk.ctrl <- update(stk.ctrl)
