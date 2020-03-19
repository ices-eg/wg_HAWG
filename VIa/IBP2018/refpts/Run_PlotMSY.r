# Code to run plotMSY
#
# see plotymsy.r and documentation for more details of how to use the program
#
# Please report any bugs to timothy.earl@cefas.co.uk to see whether these 
# can be removed in future versions. 
#
# Use setwd in R to set the directory to the file containing the .r files
# setwd("c:\\msy\\test\\")
# setwd("\\\\Lowfile3\\stock_assess$\\ICES_Working_Groups\\2012\\WGWIDE\\WHM_assessment\\MSY")
#
# Settings
# For full description of paramters, see "plotMSY instructions" document
# location of sen and sum file (both same name) ('.\\' indicates relative to working directory

# 25/02/2019 Adapted for IBP 2019; Martin Pastoors

# Generate sen and sum files

library(msy)
source("plotmsy.r")
source("convertsumsen.r")

load("D:/TEMP/IBP_VIaHerring_finalRun_MF.Rdata")
MSH       <- MSHm
MSH.retro <- MSHm.retro
MSH.sam   <- MSHm.sam

file.root <- "MSH"

senfile = paste(file.root,".sen", sep="")
sumfile = paste(file.root,".sum", sep="")
titlename = paste("Malin Shelf Herring 20190225", " (", senfile, ")" )
to.dir <- paste(".\\plotMSY_",file.root,"\\", sep="")

fpa         = NA                
flim        = NA                
bpa         = NA
blim        = NA
index       <- NA               # Don't use Lowestoft format files to find pf and pm
pfpm        <- c(0.67,0.67)     # Proportions of f and m occuring before spawning (pf and pm) specified manually)
varybiodata <- TRUE             # Assume uncertainty on stock weights, maturity and natural mortality
srweights   <- c(NA,NA,NA)      # Weights for the Ri, BH and HS S-R models. NA indicates automatic values
trimming    <- NA               # proportion of harmonic mean to plot / NA to produce diagnostic plot for trimming

convertSumSen(senfilename=senfile,indexfilename=index,pfpm=pfpm, nits=10, sr=srweights, 
              varybiodata=varybiodata,stockname="",silent=FALSE,srconstrain=TRUE)

  
##Takes some time to run, depending on the number of iterations
stock <- plotMSY(senfile, index, pfpm, srweights, trimming, 
                nits=10, nhair=5, varybiodata, titlename, 
                fpa, flim, bpa, blim, 
                silent=TRUE, onlyYPR=FALSE,
                outputfolder= to.dir )

# stock$srr

file.copy(senfile,to=to.dir )
file.copy(sumfile,to=to.dir )

dir.create(paste(to.dir,"tmp", sep=""))
file.copy(c("admodel.cov", "admodel.dep","admodel.hes","biopar.dat","eigv.rpt","fmin.log","simpar.dat",
            "simparssb.dat","simparssbpr.dat", "simpary.dat","simparypr.dat","sims",
            "srmsymc.ecm","srmsymc.log","srmsymc.psv","srmsymc2.log","variance",
            "out.dat","senf.tmp","sim.dat",
            "srmsymc.bar","srmsymc.cor","srmsymc.dat","srmsymc.eva", "srmsymc.mc2", 
            "srmsymc.mcm", "srmsymc.par","srmsymc.std","sumf.tmp"),
          to=paste(to.dir,"tmp", sep="")
          )

file.remove(c("admodel.cov", "admodel.dep","admodel.hes","biopar.dat","eigv.rpt","fmin.log","simpar.dat",
            "simparssb.dat","simparssbpr.dat", "simpary.dat","simparypr.dat","sims",
            "srmsymc.ecm","srmsymc.log","srmsymc.psv","srmsymc2.log","variance",
            "out.dat","senf.tmp","sim.dat",
            "srmsymc.bar","srmsymc.cor","srmsymc.dat","srmsymc.eva", "srmsymc.mc2", 
            "srmsymc.mcm", "srmsymc.par","srmsymc.std","sumf.tmp") )

