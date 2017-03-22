#--------------------------------------------------------------------
# WBSS FORECASTS
# 15 March 2017
# Valerio Bartolino, SLU
#--------------------------------------------------------------------
library(FLash)    # give access to automatic differenciation routines
library(FLAssess)
library(FLSAM)
rm(list=ls())
load("results/WBSS.RData")

#Define years
TaY <- dims(wbss)$maxyear   #Terminal assessment year
ImY <- TaY+1                #Intermediate Year
AdY <- TaY+2                #Advice year
CtY <- TaY+3                #Continuation year
tbl.yrs     <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table

#Deal with recruitment - a geometric mean of the five years prior to the terminal assessment year
rec.years <- (TaY-5):(TaY-1)
gm.recs  <- exp(mean(log(rec(wbss)[,as.character(rec.years)])))
wbss.srr <- list(model="geomean",params=FLPar(gm.recs))

#Expand stock object
wbss.proj <- stf(wbss, nyears=3, wts.nyears=3, fbar.nyears=3, arith.mean=TRUE, na.rm=TRUE)
wbss.proj@stock.n[1,as.character(c(TaY,ImY,AdY,CtY))] <- gm.recs

# ------------------------------------------------------------------
# Params and split of the TAC for intermediate year
# based on prop of WBSS-NSAS mixing and prop of the TAC used in the assessment year

# TAC and catches in 2017, from http://ec.europa.eu/fisheries/cfp/fishing_rules/tacs/
IVaE.catch    <- 1839    # catch transfer area terminal year (WBSS)
TAC.3a.Cfleet <- 50740   # TAC C-fleet (WBSS + NSAS)
TAC.3a.Dfleet <- 6659    # TAC D-fleet (WBSS + NSAS)
TAC.Ffleet    <- 28401   # TAC 22-24

# load proportion of WBSS/(NSAS+WBSS) in C-fleet and D-fleet
spl <- read.csv("data/splitCatch_NSAS_WBSS.csv", header=T)
avg.spl <- apply(spl[spl$year %in% (TaY-2):TaY,2:3],2,mean)  # avg split in the last 3 years

# Expected WBSS catch in ImY
# 0.54 = prop.3a.catch.Cfleet taken in 3a, informed by the fishery (PelRAC, Claus) and consistent with what used for NSAS forecasts
# 1.0 (=100%) D-fleet catch
ImY.catch <- as.numeric(round(TAC.3a.Cfleet * avg.spl[1] * 0.54 + TAC.3a.Dfleet * avg.spl[2] * 1.0 + TAC.Ffleet + IVaE.catch))

# assuming that they take the full catch for the adv.year (this is only for the tac-role over option)
AdY.catch <- as.numeric(round(TAC.3a.Cfleet * avg.spl[1] * 1 + TAC.3a.Dfleet * avg.spl[2] * 1 + TAC.Ffleet + IVaE.catch))

# Fmsy framwork (WKMSYRef)
Fmsy    <- 0.320
Fmsy.lw <- 0.230
Fmsy.up <- 0.410

Fpa  <- 0.45
Flim <- 0.52

Btrig <- 110000

SSB.ImY <- 101440
Fadv <- Fmsy * SSB.ImY / Btrig

# In addition calculate F and catch of WBSS with 0% and 50% transfer IIIA --> IV
# by accounting for North Sea LTMP
catch.withoutTrans <- 51835.638 # only WBSS, given by NSAS LTMP
catch.withTrans    <- 37015.05  # only WBSS, given by NSAS LTMP

# ------------------------------------------------------------------
# Set options

# Fmult of the F of reference for the last three years assess
f.ref <- mean(fbar(wbss)[,as.character((TaY-2):TaY)]) # avg F last three years 
f.vec <- seq(0,2,0.1)
fmult.Fmsy <- Fmsy / f.ref
f.vec <- c(f.vec, fmult.Fmsy)
options.l <- vector("list", length(f.vec))
for(i in 1:length(f.vec)){
options.l[[i]] <- fwdControl(data.frame(year=c(ImY,AdY,CtY),
					quantity=c("catch","f","f"),
                 			rel=c(NA,NA,NA),
                      		val=c(ImY.catch,f.ref*f.vec[i],f.ref*f.vec[i])))
names(options.l)[i] <- paste("fmult=",f.vec[i], sep="")
}

# add options with 0.01 interval between Fmsy lower-upper bound
Fint <- seq(Fmsy.lw, Fmsy.up, 0.01)
l <- length(options.l)
for(i in 1:length(Fint)){
    options.l[[l+i]] <- fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,Fint[i],Fint[i])))
    names(options.l)[[l+i]] <- paste("F=",Fint[i],sep="")
}

# add other options
l <- length(options.l)
options.l[[l+1]] <- #Zero catch
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity="catch",
                                          val=c(ImY.catch,0,0)))
names(options.l)[[l+1]] <- "Catch(2017) = Zero"

l <- length(options.l)
options.l[[l+1]] <- #ImY catch = TAC, AdY Fpa
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,Fpa,Fpa)))
names(options.l)[[l+1]] <- "F = Fpa"

l <- length(options.l)
options.l[[l+1]] <- #ImY catch = TAC, AdY Flim
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,Flim,Flim)))
names(options.l)[[l+1]] <- "F = Flim"

l <- length(options.l)
options.l[[l+1]] <- #ImY catch = TAC, AdY F terminal year assess
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,fbar(wbss)[,ac(TaY)],fbar(wbss)[,ac(TaY)])))
names(options.l)[[l+1]] <- "F = F(status quo)"

l <- length(options.l)
options.l[[l+1]] <- #ImY catch = TAC, AdY F corresponding to SSB=Blim in CtY
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,0.42384,0.42384)))
names(options.l)[[l+1]] <- "Blim"

l <- length(options.l)
options.l[[l+1]] <- #ImY catch = TAC, AdY F corresponding to SSB=Bpa in CtY
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,0.21414,0.21414)))
names(options.l)[[l+1]] <- "Bpa"

l <- length(options.l)
options.l[[l+1]] <- #"without Transfer"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,catch.withoutTrans,Fmsy)))
names(options.l)[[l+1]] <- "NS LTMP 0% transfer"

l <- length(options.l)
options.l[[l+1]] <- #"with 50% Transfer"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,catch.withTrans,Fmsy)))
names(options.l)[[l+1]] <- "NS LTMP 50% transfer"

l <- length(options.l)
options.l[[l+1]] <- #ImY catch = TAC, AdY Fadv rule F=Fmsy*SSB/Btrig
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,Fadv,Fadv)))
names(options.l)[[l+1]] <- "F = Fadv"

wbss.option <- lapply(options.l, function(x){fwd(wbss.proj, ctrl=x, sr=wbss.srr)})

fullTSlength <- length(1991:CtY) # assess + forecast years
out.ssb <- round(matrix(unlist(lapply(wbss.option, function(x){as.numeric(ssb(x))})), ncol=fullTSlength, byrow=T)[,(fullTSlength-2):fullTSlength])
out.fbar <- round(matrix(unlist(lapply(wbss.option, function(x){as.numeric(fbar(x))})), ncol=fullTSlength  , byrow=T)[,(fullTSlength-2):fullTSlength],3)
out.catch <- round(matrix(unlist(lapply(wbss.option, function(x){as.numeric(catch(x))})), ncol=fullTSlength  , byrow=T)[,(fullTSlength-2):fullTSlength])
out.tsb <- round(matrix(unlist(lapply(wbss.option, function(x){as.numeric(tsb(x))})), ncol=fullTSlength  , byrow=T)[,(fullTSlength-2):fullTSlength])

out.ssb   <- data.frame(names(options.l), out.ssb)
out.fbar  <- data.frame(names(options.l), out.fbar)
out.catch <- data.frame(names(options.l), out.catch)
out.tsb   <- data.frame(names(options.l), out.tsb)
colnames(out.ssb) <- colnames(out.fbar) <- colnames(out.catch) <- colnames(out.tsb) <- c("Option",ImY,AdY,CtY)


write.table(out.ssb, file="results/WBSS_projections_ssb.csv", col.names=T, row.names=F, sep=",")
write.table(out.fbar, file="results/WBSS_projections_fbar.csv", col.names=T, row.names=F, sep=",")
write.table(out.catch, file="results/WBSS_projections_catch.csv", col.names=T, row.names=F, sep=",")

# ----------------------------------------
## check selection used in the forecasts
sel.pat <- merge(f(wbss.sam),fbar(wbss.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
sel.pat.proj <- sel.pat[sel.pat$year %in% c(TaY,TaY-1,TaY-2),c("year","age","sel")]
aggregate(list(sel=sel.pat.proj$sel), list(age=sel.pat.proj$age), mean)

# ------------------------------------------------------------------
# Preparatory for Table 3.6.23
out <- data.frame("Option"=out.ssb$Option, "SSB_2018"=out.ssb[,ac(AdY)], "Fmult"=round(out.fbar[,ac(AdY)]/f.ref,3), "Fbar"=out.fbar[,ac(AdY)], "Catch_2018"=out.catch[,ac(AdY)], "Biomass_2019"=out.tsb[,ac(CtY)], "SSB_2019"=out.ssb[,ac(CtY)])

write.table(out, file="results/WBSS_projections.csv", col.names=T, row.names=F, sep=",")

sink("results/WBSS_projections")
data.frame("Biomass_2017"=out.tsb[1,ac(ImY)], "SSB_2017"=out.ssb[1,ac(ImY)], "Fmult"=out.fbar[1,ac(ImY)]/f.ref, "Fbar"=out.fbar[1,ac(ImY)], "Catch_2017"=out.catch[1,ac(ImY)])
paste("Biomass_2018 =", out.tsb[1,ac(AdY)])
paste("geomMean recr =", round(gm.recs))
out
sink()
# adjust manually...

# make a table with the input data for the forecasts 3.6.9.24
sink("results/WBSS_tab_3.6.24")
print("N2016")
wbss.option[[1]]@stock.n[,as.character(ImY)]
for(i in c(ImY,AdY,CtY)){
  tmp <- cbind("N"=c(wbss.proj@stock.n[1,as.character(i)],rep(NA,8)),
               "M"=wbss.proj@m[,as.character(i)],
               "Mat"=wbss.proj@mat[,as.character(i)],
               "PF"=wbss.proj@m.spwn[,as.character(i)],
               "PM"=wbss.proj@harvest.spwn[,as.character(i)],
               "Sel"=round(wbss.proj@harvest[,as.character(i)]/mean(wbss.proj@harvest[4:7,as.character(i)]),3),
               "SWt"=round(wbss.proj@stock.wt[,as.character(i)],3),
               "CWt"=round(wbss.proj@catch.wt[,as.character(i)],3))
  rownames(tmp) <- wbss.proj@range["min"]:wbss.proj@range["max"]
  print(i)
  print(tmp)
  rm(tmp)
}
sink()
