######################################################################################################
# CS.herring FLSAM Assessment
#
# Using R 2.13.2 and FLCore 2.4
#
# Performs an assessment of Celtic Sea Herring (cs.herring) using the FLSAM package.
#
# Afra Egan October 2013
#

####################################################################################################

### ======================================================================================================
### Initialise system, including convenience functions and title display
### ======================================================================================================
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

library(FLSAM)
 library(FLEDA)

log.msg     <-  function(fmt,...) {
	cat(sprintf(fmt,...))
	flush.console()
}

log.msg("\nCeltic Sea Herring FLSAM Assessment\n===================================\n")

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
data.source         <-  file.path("data")      #Data source, not code or package source!!!
output.dir          <-  file.path("results")       #Output directory
output.base         <-  file.path(output.dir,"CSH") #Output base filename, including directory. Other output filenames are built by appending onto this one

### ============================================================================
### Source Files
### ============================================================================

source(file.path("..","_Common","HAWG_Common_module.r"))
source(file.path("..","_Common","oto.plot.r"))
source(file.path("..","_Common","Stacked Area plot.r"))
### ============================================================================
### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
log.msg("PREPARING STOCK OBJECT...\n")
CSH<- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)

#Set no discards
CSH@catch.n                <- CSH@landings.n
CSH@catch                  <- CSH@landings
CSH@catch.wt               <- CSH@landings.wt
units(CSH)[1:17]           <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))

#Set fbar
range(CSH)[c("minfbar","maxfbar")] <- c(2,5)

#Set plus group
CSH <- setPlusGroup(CSH,CSH@range["max"])

#Set stock object name - this is propagated through into the figure titles
CSH@name    <- "Celtic Sea Herring"

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
log.msg("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
CSH.tun   <- readFLIndices(file.path(data.source, "fleet.txt"))

#Set names, and parameters etc
names(CSH.tun) <-  gsub(":.*$","",names(CSH.tun))
CSH.tun   <- lapply(CSH.tun,function(idx) {
                  idx@type 	     <- 	"number"
          		    idx@index.var[]  <-	1
                  idx@range["plusgroup"] <- 9
          		return(idx)})

names(CSH.tun)[1] <- c("CS HerAS")
CSH.tun

CSH.tun[[1]]@index[,ac(2004)] <- NA
### ======================================================================================================
### Prepare FLSAM object
### ======================================================================================================
log.msg("PREPARING FLSAM CONTROL OBJECT...\n")

#Default settings and a test run

#Create object
#CSH.ctrl <- FLSAM.control(CSH,CSH.tun, default="full")


####################################################################################################


#Setup configuration - creates an empty control object with appropriate structure
CSH.ctrl <- FLSAM.control(CSH,CSH.tun)

#All fishing mortality states are free except
#oldest ages to ensure stablity
CSH.ctrl@states[1,] <- c(1:7,8,8)


#Correlated Random walks for fishing mortalities - Default = FALSE = independent)
CSH.ctrl@cor.F<-2

# Catchabilities
CSH.ctrl@catchabilities["CS HerAS",]<-c(1,2,3,3,3,3,3,3,3)

#Fishing mortality RWs are set from an analysis of ICA VPA results
CSH.ctrl@f.vars[1,] <- c(1,2,2,2,2,2,2,2,2)

#Set the variances. Separate variance for recruitment and plus group
CSH.ctrl@logN.vars[]      <- c(1,rep(2,dims(CSH)$age-1))

#Bind the observation variances
CSH.ctrl@obs.vars[1,] <- c(1,2,3,3,4,4,5,5,5)
CSH.ctrl@obs.vars["CS HerAS",1:9] <- 6

## Stock recruitment model code (0=RW, 1=Ricker, 2=BH, ... more in time) Default = 0 RW
#CSH.ctrl@srr <- as.integer(0)

##Finalise
#CSH.ctrl@name <- "Final Assessment"
#CSH.ctrl <- update(CSH.ctrl)

CSH.ctrl@cor.obs[1,] <- NA
CSH.ctrl@cor.obs[2,] <- rep(1,8)
CSH.ctrl@cor.obs.Flag[2] <- af("AR")
CSH.ctrl <- update(CSH.ctrl)
### ======================================================================================================
### Perform the assessment
### ======================================================================================================
log.msg("PERFORMING ASSESSMENT...\n")

#Now perform the asssessment
CSH.sam   <-  FLSAM(CSH,CSH.tun,CSH.ctrl)

#save AIC
write.csv(AIC(CSH.sam),file=file.path(output.dir,"AIC.csv"))

#Update stock object
CSH <- CSH + CSH.sam


### ======================================================================================================


## check which I need to adjust
## Change recruitment in the final year to be geomean from 1981 - last year - 2 years

Rec=exp(mean(log(CSH@stock.n[1,as.character(1981:(CSH@range['maxyear']-2)),,,,])))

# puts the geomwan recruitment into last fishing year
CSH@stock.n['1',(as.character(CSH@range['maxyear'])),,,,]=Rec

#survivors  NOT TESTED YET
dmns  <- dims(CSH@stock.n)
survivors <- FLQuant(c(Rec,stock.n(CSH)[,ac(CSH@range['maxyear'])] * exp(-harvest(CSH[,ac(CSH@range['maxyear'])])-m(CSH[,ac(2012)]))),
                               dimnames=list(ages=dmns$min:(dmns$max+1),year=CSH@range['maxyear']+1,unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter))



Rec=exp(mean(log(CSH.sam@stock.n[1,as.character(1981:(CSH.sam@range['maxyear']-2)),,,,])))

# puts the geomwan recruitment into last fishing year
CSH.sam@stock.n['1',(as.character(CSH.sam@range['maxyear'])),,,,]=Rec

#survivors  NOT TESTED YET
dmns  <- dims(CSH.sam@stock.n)
survivors <- FLQuant(c(Rec,stock.n(CSH.sam)[,ac(CSH.sam@range['maxyear'])] * exp(-harvest(CSH.sam[,ac(CSH.sam@range['maxyear'])])-m(CSH[,ac(2012)]))),
                               dimnames=list(ages=dmns$min:(dmns$max),year=CSH.sam@range['maxyear']+1,unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter))



### ============================================================================
### Plots
### ============================================================================
#Setup plots

pdf(paste(output.base,"_plots.pdf",sep=""))
#png(file.path(output.dir,"figures - %02d.png"),units = "px", height=800,width=672, bg = "white")

### ======================================================================================================
### Diagnostics and plots
### ======================================================================================================
#Diagnostic plots
residual.diagnostics(CSH.sam)

#Bubble plot of survey residuals
res.dat <- subset(residuals(CSH.sam),fleet=="CS HerAS")
res.dat$data <- res.dat$std.res
p <- xyplot(age  ~ year |fleet,res.dat,
       cex=4*abs(res.dat$std.res)/max(abs(res.dat$std.res))+0.1,
       col=ifelse(res.dat$std.res<0,"red","black"),
       pch=16)
print(p)

## bubble plot of catch residuals
res.dat <- subset(residuals(CSH.sam),fleet=="catch")
res.dat$data <- res.dat$std.res
p <- xyplot(age  ~ year |fleet,res.dat,
       cex=4*abs(res.dat$std.res)/max(abs(res.dat$std.res))+0.1,
       col=ifelse(res.dat$std.res<0,"red","black"),
       pch=16)
print(p)


 #Plot uncertainties as a function of time
  CV.yrs <- ssb(CSH.sam)$year
  CV.dat <- cbind(SSB=ssb(CSH.sam)$CV,
                     Fbar=fbar(CSH.sam)$CV,Rec=rec(CSH.sam)$CV)
  matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
      xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
  legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")

  #Plot catchabilities values
  catch <- catchabilities(CSH.sam)
  print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
            scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
            type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
            subset=fleet %in% c("CS HerAS"),
            main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

  #Plot obs_variance (weightings)
  obv <- obs.var(CSH.sam)
  obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
  obv <- obv[order(obv$value),]
  bp <- barplot(obv$value,ylab="Observation Variance",
         main="Observation variances by data source",col=factor(obv$fleet))
  axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
  legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)



  plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
    pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
  text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

#Plot fishery selectivity pattern over time
  sel.pat <- merge(f(CSH.sam),fbar(CSH.sam),
               by="year",suffixes=c(".f",".fbar"))
  sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
  sel.pat$age <- as.numeric(as.character(sel.pat$age))
  print(xyplot(sel ~ age|sprintf("%i's",floor((year+2)/5)*5),sel.pat,
           groups=year,type="l",as.table=TRUE,
           scale=list(alternating=FALSE),
           main="Selectivity of the Fishery by Period",xlab="Age",ylab="F/Fbar"))



#Plot correlation plot
cor.plot(CSH.sam)


 ##plot stock summary plot
plot(CSH.sam)


#plot(tmp.tun,type="pairwise")
plot(CSH.tun[["CS HerAS"]],type="internal")

## Otolith Plot
plot.otolith(CSH.sam,n=1000)



### Data plots #################################################################
# Plot the time series of the surveys
timeseries(CSH.tun[["CS HerAS"]],slot="index")

  #Time series of west by cohort
  west.by.cohort      <- as.data.frame(FLCohort(window(CSH@stock.wt,1980,range(CSH)["maxyear"])))
  west.by.cohort      <- subset(west.by.cohort,!is.na(west.by.cohort$data))
  west.by.cohort$year <- west.by.cohort$age + west.by.cohort$cohort
  west.cohort.plot    <- xyplot(data~year,data=west.by.cohort,
                                groups=cohort,
                                auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
                                type="b",
                                xlab="Year",ylab="Weight in the stock (kg)",
                                main=paste(CSH@name,"Weight in the stock by cohort"),
                                par.settings=list(superpose.symbol=list(pch=as.character(unique(west.by.cohort$cohort)%%10),cex=1.25)),
                                panel=function(...) {
                                  panel.grid(h=-1,v=-1)
                                  panel.xyplot(...)
                                })
  print(west.cohort.plot)


 
 #####################################################################################

### ======================================================================================================
### Reference points
### ======================================================================================================
library(FLBRP)
ref. <- brp(FLBRP(CSH,fbar=seq(0,1,length.out=101),nyears=3))
print(refpts(ref.))

CSH.SRR <- FLSR(
	rec = rec(CSH)[,ac((range(CSH)["minyear"]+1): range(CSH)["maxyear"])],
	ssb = ssb(CSH)[,ac((range(CSH)["minyear"])  :(range(CSH)["maxyear"]-1))],
	model='segreg')
CSH.SRR <- fmle(CSH.SRR)

plot(CSH.SRR)

#newData <- predict(CSH.SRR,ssb=FLQuant(seq(0,max(ssb(CSH)),length.out=200)))
#yrange  <- range(pretty(c(0,range(rec(CSH)))))/1e6; xrange <- range(pretty(c(0,range(ssb(CSH)))))/1e6
#plot(y=newData/1e6,x=seq(0,max(ssb(CSH)),length.out=200)/1e6,type="l",lwd=2,
#     xlab="SSB (million tonnes)",ylab="Recruitment (billions)",xlim=xrange,ylim=yrange,
#     las=1,cex.lab=1.3,cex.axis=1.1,xaxs="i",yaxs="i")
#points(y=rec(CSH)/1e6,x=ssb(CSH)/1e6)

dev.off()

### ======================================================================================================
### Document Assessment
### ======================================================================================================

#source(file.path(,"_Common","FLSAM.out.r"))
output.dir          <-  file.path(".","results")                #Output directory
log.msg("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt           <- options("width","scipen")
options("width"=80,"scipen"=1000)

sam.out.file<- FLSAM.out(CSH,CSH.tun,CSH.sam,format="TABLE 4.6.3.%i Celtic Sea Herring.")

write(sam.out.file,file.path(output.dir,"CSH_sam.out",sep=".")) #or could create output.base


options("width"=old.opt$width,"scipen"=old.opt$scipen)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
#writeFLStock(CSH,output.file=file.path(output.dir))
writeFLStock(CSH,file.path(output.dir,"hawg_her-IRLS.ypr"),type="YPR")

stockSummaryTable <- cbind(rec(CSH.sam)$year,
                           rec(CSH.sam)$value,      rec(CSH.sam)$lbnd,    rec(CSH.sam)$ubnd,
                           tsb(CSH.sam)$value,      tsb(CSH.sam)$lbnd,    tsb(CSH.sam)$ubnd,
                           ssb(CSH.sam)$value,      ssb(CSH.sam)$lbnd,    ssb(CSH.sam)$ubnd,
                           catch(CSH.sam)$value,    catch(CSH.sam)$lbnd,  catch(CSH.sam)$ubnd,
                           catch(CSH.sam)$value / ssb(CSH.sam)$value, catch(CSH.sam)$lbnd / ssb(CSH.sam)$lbnd, catch(CSH.sam)$ubnd / ssb(CSH.sam)$ubnd,
                           fbar(CSH.sam)$value,     fbar(CSH.sam)$lbnd,   fbar(CSH.sam)$ubnd,
                           c(sop(CSH)))
colnames(stockSummaryTable) <-
  c("Year",paste(rep(c("Recruits Age 0 (Thousands)","Total biomass (tonnes)","Spawing biomass (tonnes)",
                       "Landings (tonnes)","Yield / SSB (ratio)","Mean F ages 2-5"),each=3),c("Mean","Low","High")),"SoP (%)")



stockSummaryTable <- cbind(rec(CSH)$year,
                           rec(CSH)$value,      rec(CSH)$lbnd,    rec(CSH)$ubnd,
                           tsb(CSH)$value,      tsb(CSH.sam)$lbnd,    tsb(CSH)$ubnd,
                           ssb(CSH)$value,      ssb(CSH)$lbnd,    ssb(CSH)$ubnd,
                           catch(CSH)$value,    catch(CSH)$lbnd,  catch(CSH)$ubnd,
                           catch(CSH)$value / ssb(CSH)$value, catch(CSH)$lbnd / ssb(CSH)$lbnd, catch(CSH)$ubnd / ssb(CSH)$ubnd,
                           fbar(CSH)$value,     fbar(CSH)$lbnd,   fbar(CSH)$ubnd,
                           c(sop(CSH)))
colnames(stockSummaryTable) <-
  c("Year",paste(rep(c("Recruits Age 0 (Thousands)","Total biomass (tonnes)","Spawing biomass (tonnes)",
                       "Landings (tonnes)","Yield / SSB (ratio)","Mean F ages 2-5"),each=3),c("Mean","Low","High")),"SoP (%)")



## ADD 2013 VALUES FROM ??
#sst2013 <- c(2013,144514,21861) #rec = first value, SSB is the second value for intermediate year
stockSummaryTable <- rbind(stockSummaryTable,NA)
#stockSummaryTable[nrow(stockSummaryTable),c(1,2,8)] <- sst2013
write.csv(stockSummaryTable,file=file.path(output.dir,"stockSummaryTable.csv"))
options("width"=old.opt$width,"scipen"=old.opt$scipen)

### ============================================================================
### RETRO
### ============================================================================
#Import externals
library(FLSAM);
source(file.path("..","_Common","retroResidual.r"))

n.retro.years       <- 7		# (must be a numeric value, not a year range)

#Perform retrospective
CSH.retro <- retro(CSH,CSH.tun,CSH.ctrl,n.retro.years,base.assess=CSH.sam)

#Setup plots

pdf(paste(output.base,"_retro.pdf",sep=""))
#png(file.path(output.base,"figuresRetro - %02d.png"),units = "px", height=800,width=672, bg = "white")

#Plot retro
plot(CSH.retro,futureYrs=F)

print(retroResiduals(CSH.retro,"CS HerAS",2005:(range(CSH)["maxyear"])))


print(retroResiduals(CSH.retro,"catch",1990:(range(CSH)["maxyear"])))
print(retroSelectivity( CSH.retro,2005:(range(CSH)["maxyear"])))

dev.off()


### ============================================================================
### Finish
### ============================================================================

log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))























print(west.cohort.plot)########################################################################################################
## Extra Plots

#Extract fitted results tables
ssb(SCH.sam)
tsb(CSH.sam)
fbar(CSH.sam)
rec(CSH.sam)
f(CSH.sam)
n(CSH.sam)
catchabilities(CSH.sam)
obs.var(CSH.sam)

### Plot survey residuals from FLSAM to compare with FLICA


res.dat <- subset(residuals(CSH.sam),fleet=="CS HerAS")
year<-res.dat$year

age<-res.dat$age
age <-factor(age,levels=c(2,3,4,5),
labels=c(2,3,4,5))

data<-res.dat$std.res

ttl <- list(label = "Residuals by age CSHAS FLSAM", cex = 1)
yttl <- list(label = "Residuals", cex = 0.9)
xttl <- list(label="years",cex = 0.9)
stripttl <- list(cex = 0.7)
ax <- list(cex = 0.7)
print(xyplot(data ~ year | age, main = ttl, ylab = yttl, xlab = xttl,par.strip.text = stripttl,
scales = ax))


### Plot Selection pattern by year

sel.pat <- merge(f(CSH.sam),fbar(CSH.sam),
             by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
sel.pat$year<-as.numeric(as.character(sel.pat$year))

year<-factor(sel.pat$year, levels=c(1958:2012))

## selected years only
#year<-factor(sel.pat$year,levels=c(2006,2007,2008,2009,2010,2011,2012))

print(xyplot(sel ~ age|year,
         groups=year,type="l",as.table=TRUE,
         scale=list(alternating=FALSE),
         main="Selectivity of the Fishery by Year",xlab="Age",ylab="F/Fbar"))


