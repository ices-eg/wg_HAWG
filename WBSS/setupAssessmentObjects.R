################################################################################
# WBSS prepare stock obj for assessment
#
# 10 March 2015
# HAWG working group
#
################################################################################

# read stock
wbss.stk <- readFLStock("data/index.dat", no.discards=TRUE)

#Load survey indicex
wbss.tun   <- readFLIndices("data/survey.dat")
names(wbss.tun)

wbss.tun   <- lapply(wbss.tun,function(x) {x@type <- "number"; return(x)})

wbss.tun[["HERAS"]]@range["plusgroup"] <- 8
wbss.tun[["GerAS"]]@range["plusgroup"] <- 8
wbss.tun[["N20"]]@range["plusgroup"] <- NA
wbss.tun[["IBTS Q1"]]@range["plusgroup"] <- NA
wbss.tun[["IBTS Q3"]]@range["plusgroup"] <- NA

########################################
units(harvest(wbss.stk)) <- "f"
wbss.stk <- setPlusGroup(wbss.stk, 8)

stk <- wbss.stk

## empty the harvest and stock. slots
harvest(stk)[] <- NA
stock.n(stk)[] <- NA

## set Fbar
range(stk)[c("minfbar","maxfbar")] <- c(3,6)
