#EqSim run for CS Herring

rm(list=ls())
gc()

library(FLCore)
library(msy)

path<-"C:/ICES/HAWG2016/Forecast/2016"
#path<-"C:/ICES/HAWG2016/Forecast/2015"

try(setwd(path))

source("C:/ICES/HAWG2016/eqsr_fit.R")

data.source  <-  file.path("stf")

CSH<- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)
CSH@catch.n                <- CSH@landings.n
CSH@catch                  <- CSH@landings
CSH@catch.wt               <- CSH@landings.wt
units(CSH)[1:17]           <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))
#Set fbar
range(CSH)[c("minfbar","maxfbar")] <- c(2,5)
#Set plus group
CSH<- setPlusGroup(CSH,CSH@range["max"])
#Set stock object name - this is propagated through into the figure titles
CSH@name    <- "Celtic Sea Herring"

CSH@stock.n<-readVPAFile(file.path(data.source, "N.txt"))
CSH@harvest<-readVPAFile(file.path(data.source, "F.txt"))
units(CSH@harvest)<-'f'

#ssb(CSH)

#this won't work
#CSH@stock.n[,ac((range(CSH)["minyear"]):(range(CSH)["maxyear"]-2))] <- CSH@stock.n[,ac((range(CSH)["minyear"]+2):(range(CSH)["maxyear"]))]
#ssb(CSH)

#FIT <- eqsr_fit(trim(CSH, year=1958:2013), nsamp=1000)
#see source for this function in eqsr_fit.r
FIT <- eqsr_fit(CSH, nsamp=1000)

eqsr_plot(FIT, n=2e4)

#WKMSYREF4 default values for Fcv=0.212 or 0.233 (both mentioned in report) and Fphi=0.423

#last 3 years for biology and selection (as per MP evaluation)
#bio.years <- c(2013,2015); sel.years <- c(2013,2015)
bio.years <- c(2012,2014); sel.years <- c(2012,2014)

#SIM <- eqsim_run(FIT, bio.years = bio.years, bio.const = FALSE,
#                 sel.years = sel.years, sel.const = FALSE, 
#                 Fcv = 0.40, Fphi = 0.30, Blim = 33000, Bpa = 54000,
#                 Fscan = seq(0,1.2,len=40), verbose = TRUE,
#                 extreme.trim = c(0.05,0.95),Btrigger = 0)

#Flim F05 Fpa Fmsy05 Fmsy95 FmsyMed FmsyMean
#0.5621684 0.2807308 0.3431959 0.3291291 0.5297297 0.4336336 0.4307692


#with Fcv,Fphi=0, Btrigger=0
SIM <- eqsim_run(FIT, bio.years = bio.years, bio.const = FALSE,
                 sel.years = sel.years, sel.const = FALSE, 
                 Fcv = 0, Fphi = 0, Blim = 33000, Bpa = 54000,
                 Fscan = seq(0,1.2,len=40), verbose = TRUE,
                 extreme.trim = c(0.05,0.95), Btrigger = 0)

#Flim F05 Fpa Fmsy05 Fmsy95 FmsyMed FmsyMean
#0.6147476 0.338736 0.3752948 0.3795796 0.5981982 0.5057057 0.4923077

#including a Btrigger
SIM <- eqsim_run(FIT, bio.years = bio.years, bio.const = FALSE,
                 sel.years = sel.years, sel.const = FALSE, 
                 Fcv = 0, Fphi = 0, Blim = 33000, Bpa = 54000,
                 Fscan = seq(0,1.2,len=40), verbose = TRUE,
                 extreme.trim = c(0.05,0.95), Btrigger = 54000)

#Flim F05 Fpa Fmsy05 Fmsy95 FmsyMed FmsyMean
#1.070839 0.4729937 0.6537323 0.4828829 1.162763 0.8228228 0.8


eqsim_plot(SIM, catch=FALSE)

SIM$Refs
Flim <- SIM$Refs[1,3]
F05 <- SIM$Refs[1,1]
Fpa <- SIM$Refs[1,3]*exp(0.3*-1.645)
Fmsy05 = SIM$Refs2[2,6]
Fmsy95 = SIM$Refs2[2,8]
FmsyMed = SIM$Refs2[2,4]
FmsyMean = SIM$Refs2[2,5]

cat("Flim","F05","Fpa","Fmsy05","Fmsy95","FmsyMed","FmsyMean\n")
cat(Flim,F05,Fpa,Fmsy05,Fmsy95,FmsyMed,FmsyMean)


