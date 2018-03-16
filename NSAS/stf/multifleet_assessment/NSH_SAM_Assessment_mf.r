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
#path <- "D:/Repository/ICES_HAWG/wg_HAWG/NSAS/benchmark/"
path <- "D:/git/wg_HAWG/NSAS/"
try(setwd(path),silent=FALSE)

### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================
output.dir          <-  file.path(".","results/")        # figures directory
output.base         <-  file.path(output.dir,"NSH_HAWG2018_mf")  # Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  10                                      # Number of years for which to run the retrospective
assessment_name     <- 'NSH_HAWG2018mf'

#load(paste(output.dir,"/NSH_mf_final.RData",sep=""))


### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA)
source(file.path("./stf/multifleet_assessment/setupMultiFleetData.r"))
source(file.path("./stf/multifleet_assessment/setupControlObject_mf.r"))

### ============================================================================
### ============================================================================
### ============================================================================
### Run the assessment
### ============================================================================
### ============================================================================
### ============================================================================
#NSH3.ctrl@residuals <- F

NSH3f.sam   <- FLSAM(NSHs3,
                     NSH.tun,
                     NSH3.ctrl)

NSH3f <- NSH
#save(NSHs3,NSHs4,NSH3f,NSH.tun,NSH3.ctrl,NSH3f.sam,file=file.path(output.dir,"NSH_mf_final_no_retro.RData"))

#NSH3.ctrl@residuals <- F
NSH3f.retro <- retro(NSHs3,NSH.tun,NSH3.ctrl,n.retro.years)

save(NSHs3,NSHs4,NSHmf,NSH3f,NSH,NSH.tun,NSH3.ctrl,NSH3f.sam,NSH3f.retro,file=file.path(output.dir,"NSH_mf_final.RData"))

### ============================================================================
### ============================================================================
### ============================================================================
#Setup plots
#pdf(file.path(output.dir,paste(name(NSH.sam),".pdf",sep="")))
NSH.sam <- NSH3f.sam
png(file.path(output.dir,paste("Multifleet_NSAS","figures - %02d.png")),units = "px", height=800,width=672, bg = "white")

### ============================================================================
### fishing selectivity
### ============================================================================

sel.pat <- merge(f(NSH3f.sam),fbar(NSH3f.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))

sel.pat <- merge(f(NSH.sam),fbar(NSH.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar

NyearPlot <- 5

# A fleet
a <- data.frame(sel.pat[sel.pat$fleet == 'catch A',])
a <- a[which(a$year %in% 2015:2017),]

for(idxString in 1:dim(a)[1]){
  a$year[idxString] <- toString(a$year[idxString])
}


ggplot(a, aes(x = age, y = sel, colour = year)) + geom_line()

# retrospective

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
windows()
#Setup plots
# stock trajectory retrospective
plot(NSH3f.retro)

savePlot(paste(output.dir,"/",assessment_name,"_stock_trajectory_retro.png",sep = ""),type="png")

#Setup plots
png(file.path(output.dir,paste(name(NSH.sam),"figures_retro - %02d.png")),units = "px", height=800,width=672, bg = "white")

# figure - retrospective for SSB, Fbar, rec
print(plot(NSH3f.retro,futureYrs=F))
print(retroParams(NSH.retro))
print(retroSelectivity( NSH.retro,2009:2016))

#print(lapply(NSH.retro,retroParam))
# closing pdf and png print
dev.off()