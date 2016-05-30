#Multi annual forecasts

library(FLCore)
library(FLAssess)

#julios mean recruitment for final assessment and 7 retrospective peels as used in the forecast
Julios <- c(537900,541287,539890,552403,532792,521410,517458,499030)

#recorded catches used in intermediate year
#2009 5745, 2010 8370, 2011 11470, 2012 21820
#2013 16247, 2014 19574, 2015 18355, 2016 18590
Imy.catches <- c(18590,18355,19574,16247,21820,11470,8370,5745)
names(Imy.catches) <- as.character(seq(2016,2009,by=-1))

#assumed intermediate year catches
Imy.catches <- c(18356,21932,17152,18236,16196,13200,9227,5745)
names(Imy.catches) <- as.character(seq(2016,2009,by=-1))


#list for results
res <- list("Peel0"<-NA,"Peel1"<-NA,"Peel2"<-NA,"Peel3"<-NA,"Peel4"<-NA,"Peel5"<-NA,"Peel6"<-NA,"Peel7"<-NA)
names(res) <- paste0("Peel",seq(0,7))
#peel0 terminal year
#last.year <- 2016

for (peel in seq(0,7)) {

  #data source
  setwd(paste0("C:/ICES/HAWG2016/Forecast/2016_peel",peel))
  data.source  <-  file.path("stf")
  
  #read in FLStock object
  CSH<- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)
  #Set no discards
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

  #Define years
  TaY <- dims(CSH)$maxyear   #Terminal assessment year
  ImY <- TaY+1                #Intermediate Year
  AdY <- TaY+2                #Advice year
  CtY1 <- TaY+3                #Continuation year 1
  CtY2 <- TaY+4                #Continuation year 2
  CtY3 <- TaY+5                #Continuation year 3
  CtY4 <- TaY+6                #Continuation year 4
  CtY5 <- TaY+7                #Continuation year 5
  CtY6 <- TaY+8                #Continuation year 6
  CtY7 <- TaY+9                #Continuation year 7
  CtY8 <- TaY+10               #Continuation year 8
  CtY9 <- TaY+11               #Continuation year 9
  CtY10 <- TaY+12               #Continuation year 10
  
  tbl.yrs     <- as.character(c(ImY,AdY,CtY1,CtY2,CtY3,CtY4,CtY5,CtY6,CtY7,CtY8,CtY9,CtY10))
  
  #recruitment
  rec <- Julios[peel+1]
  
  CSH.srr <- list(model="geomean",params=FLPar(rec))
  
  #create slots
  CSH.proj <- stf(CSH, nyears=12, wts.nyears=3, arith.mean=TRUE, na.rm=TRUE)
  
  #project terminal year into intermediate year
  CSH.proj@stock.n[ac(2:9),ac(ImY)]  <- CSH@stock.n[ac(1:8),ac(TaY)] * exp(-CSH@harvest[ac(1:8),ac(TaY)]-CSH@m[ac(1:8),ac(TaY)])
  CSH.proj@stock.n[ac(9),ac(ImY)]    <- CSH.proj@stock.n[ac(9),ac(ImY)] + CSH@stock.n[ac(9),ac(TaY)] * exp(-CSH@harvest[ac(9),ac(TaY)]-CSH@m[ac(9),ac(TaY)])
  CSH.proj@stock.n[1,as.character(c(ImY,AdY,CtY1,CtY2,CtY3,CtY4,CtY5,CtY6,CtY7,CtY8,CtY9,CtY10))] <- rec
  
  #set intermediate year catch
  ImY.catch <- Imy.catches[peel+1]   

  #Setup options
  options.l <- list(
    #Intermediate year catch followed by managment plan F
    "Fmgt"=
        fwdControl(data.frame(year=c(ImY,AdY,CtY1,CtY2,CtY3,CtY4,CtY5,CtY6,CtY7,CtY8,CtY9,CtY10),
                              quantity=c("catch",rep("f",11)),
                              rel=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                              val=c(ImY.catch,rep(0.23,11)))))
  
  CSH.options   <- lapply(options.l,function(ctrl) {fwd(CSH.proj,ctrl=ctrl,sr=CSH.srr)})

  #Options summary table
  opt.sum.tbl <- function(stcks,fname) {
    options.sum.tbl <- sapply(as.list(1:length(stcks)),function(i) {
      opt <- names(stcks)[i]
      stk <- stcks[[opt]]
      #Build up the summary
      sum.tbl     <- data.frame(Rationale=opt,
                                F.ImY=fbar(stk)[,as.character(ImY),drop=TRUE],
                                Catch.ImY=computeCatch(stk)[,as.character(ImY),drop=TRUE],
                                SSB.ImY=ssb(stk)[,as.character(ImY),drop=TRUE],
                                F.AdY=fbar(stk)[,as.character(AdY),drop=TRUE],
                                Catch.AdY=computeCatch(stk)[,as.character(AdY),drop=TRUE],
                                SSB.AdY=ssb(stk)[,as.character(AdY),drop=TRUE],
                                Catch.CtY1=computeCatch(stk)[,as.character(CtY1),drop=TRUE],
                                SSB.CtY1=ssb(stk)[,as.character(CtY1),drop=TRUE],
                                Catch.CtY2=computeCatch(stk)[,as.character(CtY2),drop=TRUE],
                                SSB.CtY2=ssb(stk)[,as.character(CtY2),drop=TRUE],
                                Catch.CtY3=computeCatch(stk)[,as.character(CtY3),drop=TRUE],
                                SSB.CtY3=ssb(stk)[,as.character(CtY3),drop=TRUE],
                                Catch.CtY4=computeCatch(stk)[,as.character(CtY4),drop=TRUE],
                                SSB.CtY4=ssb(stk)[,as.character(CtY4),drop=TRUE],
                                Catch.CtY5=computeCatch(stk)[,as.character(CtY5),drop=TRUE],
                                SSB.CtY5=ssb(stk)[,as.character(CtY5),drop=TRUE],
                                Catch.CtY6=computeCatch(stk)[,as.character(CtY6),drop=TRUE],
                                SSB.CtY6=ssb(stk)[,as.character(CtY6),drop=TRUE],
                                Catch.CtY7=computeCatch(stk)[,as.character(CtY7),drop=TRUE],
                                SSB.CtY7=ssb(stk)[,as.character(CtY7),drop=TRUE],
                                Catch.CtY8=computeCatch(stk)[,as.character(CtY8),drop=TRUE],
                                SSB.CtY8=ssb(stk)[,as.character(CtY8),drop=TRUE],
                                Catch.CtY9=computeCatch(stk)[,as.character(CtY9),drop=TRUE],
                                SSB.CtY9=ssb(stk)[,as.character(CtY9),drop=TRUE],
                                Catch.CtY10=computeCatch(stk)[,as.character(CtY10),drop=TRUE],
                                SSB.CtY10=ssb(stk)[,as.character(CtY10),drop=TRUE])
    })
    options.sum.tbl <- t(options.sum.tbl)
    colnames(options.sum.tbl) <- c("Rationale",
                                   sprintf("Fbar (%i)",ImY),sprintf("Catch (%i)",ImY),sprintf("SSB (%i)",ImY),
                                   sprintf("Fbar (%i)",AdY),sprintf("Catch (%i)",AdY),sprintf("SSB (%i)",AdY),
                                   sprintf("Catch (%i)",CtY1),sprintf("SSB (%i)",CtY1),sprintf("Catch (%i)",CtY2),sprintf("SSB (%i)",CtY2),
                                   sprintf("Catch (%i)",CtY3),sprintf("SSB (%i)",CtY3),sprintf("Catch (%i)",CtY4),sprintf("SSB (%i)",CtY4),
                                   sprintf("Catch (%i)",CtY5),sprintf("SSB (%i)",CtY5),sprintf("Catch (%i)",CtY6),sprintf("SSB (%i)",CtY6),
                                   sprintf("Catch (%i)",CtY7),sprintf("SSB (%i)",CtY7),sprintf("Catch (%i)",CtY8),sprintf("SSB (%i)",CtY8),
                                   sprintf("Catch (%i)",CtY9),sprintf("SSB (%i)",CtY9),sprintf("Catch (%i)",CtY10),sprintf("SSB (%i)",CtY10))
    write.csv(options.sum.tbl,file=fname,row.names=FALSE)
    options.sum.tbl
  }
  
  options.sum.tbl <- opt.sum.tbl(stcks=CSH.options,fname=file.path(getwd(),"options - summary.csv",sep="."))
  
  res[[peel+1]] <- list("res" = options.sum.tbl, "stk" <- CSH)
  
}

#SSB bias calculations

#terminal year of peel0 assessment
term0.year <- dims(res[['Peel0']][[2]])$maxyear
#advice year of peel0 assessment
adv0.year <- term0.year + 2

delta <- rep(NA,7)
delta.pct <- rep(NA,7)

for (peel in 1:7) {
  #comparison year
  comp.year <- adv0.year-peel+1
  
  cat(as.numeric(res[[paste0('Peel',peel-1)]][[1]][,paste0('SSB (',comp.year,')')]),"\n")
  cat(as.numeric(res[[paste0('Peel',peel)]][[1]][,paste0('SSB (',comp.year,')')]),"\n")
  
  #compare SSB in advice year from one assessment with the SSB in advice year + 1 from next peel
  delta[peel] <- as.numeric(res[[paste0('Peel',peel-1)]][[1]][,paste0('SSB (',comp.year,')')]) - as.numeric(res[[paste0('Peel',peel)]][[1]][,paste0('SSB (',comp.year,')')])
  delta.pct[peel] <- delta[peel]/as.numeric(res[[paste0('Peel',peel)]][[1]][,paste0('SSB (',comp.year,')')])
    
}

mean(delta)
mean(delta.pct)

fSelCol <- function(ip=TRUE){
  if (ip) {"green"} else {"red"}
}

layout(mat=as.matrix(c(1,2), nrow=2, ncol=1))

#plot of SSB
plot(0,0,xlim=c(1958,2015),ylim=c(0,200),xlab="Year",ylab="SSB (kt)")
for (i in 1:8) {lines(seq(dims(res[[i]][[2]])$minyear,dims(res[[i]][[2]])$maxyear),ssb(res[[i]][[2]])/1e3)}

#plot of FBar
plot(0,0,xlim=c(1958,2015),ylim=c(0,0.8),xlab="Year",ylab="FBar")
for (i in 1:8) {lines(seq(dims(res[[i]][[2]])$minyear,dims(res[[i]][[2]])$maxyear),fbar(res[[i]][[2]]))}

jpeg("CSHerring_Bias_assumedImY.jpg")
layout(mat=as.matrix(c(1), nrow=1, ncol=1))

#plot of SSB trajectories
plot(0,0,xlim=c(2005,2018),ylim=c(0,200),xlab="Year",ylab="SSB (kt)", main="CS Herring", sub="7 peels")
for (i in 1:8) {lines(seq(dims(res[[i]][[2]])$minyear,dims(res[[i]][[2]])$maxyear),ssb(res[[i]][[2]])/1e3)}

lines(seq(2015,2020),unlist(c(ssb(res[[1]][[2]])[,'2015'],res[[1]][[1]][,'SSB (2016)'],res[[1]][[1]][,'SSB (2017)'],res[[1]][[1]][,'SSB (2018)'],res[[1]][[1]][,'SSB (2019)'],res[[1]][[1]][,'SSB (2020)']))/1e3,lwd=2,col="red", lty=2)
lines(seq(2014,2020),unlist(c(ssb(res[[2]][[2]])[,'2014'],res[[2]][[1]][,'SSB (2015)'],res[[2]][[1]][,'SSB (2016)'],res[[2]][[1]][,'SSB (2017)'],res[[2]][[1]][,'SSB (2018)'],res[[2]][[1]][,'SSB (2019)'],res[[2]][[1]][,'SSB (2020)']))/1e3,lwd=2)
lines(seq(2013,2020),unlist(c(ssb(res[[3]][[2]])[,'2013'],res[[3]][[1]][,'SSB (2014)'],res[[3]][[1]][,'SSB (2015)'],res[[3]][[1]][,'SSB (2016)'],res[[3]][[1]][,'SSB (2017)'],res[[3]][[1]][,'SSB (2018)'],res[[3]][[1]][,'SSB (2019)'],res[[3]][[1]][,'SSB (2020)']))/1e3,lwd=2)
lines(seq(2012,2020),unlist(c(ssb(res[[4]][[2]])[,'2012'],res[[4]][[1]][,'SSB (2013)'],res[[4]][[1]][,'SSB (2014)'],res[[4]][[1]][,'SSB (2015)'],res[[4]][[1]][,'SSB (2016)'],res[[4]][[1]][,'SSB (2017)'],res[[4]][[1]][,'SSB (2018)'],res[[4]][[1]][,'SSB (2019)'],res[[4]][[1]][,'SSB (2020)']))/1e3,lwd=2)
lines(seq(2011,2020),unlist(c(ssb(res[[5]][[2]])[,'2011'],res[[5]][[1]][,'SSB (2012)'],res[[5]][[1]][,'SSB (2013)'],res[[5]][[1]][,'SSB (2014)'],res[[5]][[1]][,'SSB (2015)'],res[[5]][[1]][,'SSB (2016)'],res[[5]][[1]][,'SSB (2017)'],res[[5]][[1]][,'SSB (2018)'],res[[5]][[1]][,'SSB (2019)'],res[[5]][[1]][,'SSB (2020)']))/1e3,lwd=2)
lines(seq(2010,2020),unlist(c(ssb(res[[6]][[2]])[,'2010'],res[[6]][[1]][,'SSB (2011)'],res[[6]][[1]][,'SSB (2012)'],res[[6]][[1]][,'SSB (2013)'],res[[6]][[1]][,'SSB (2014)'],res[[6]][[1]][,'SSB (2015)'],res[[6]][[1]][,'SSB (2016)'],res[[6]][[1]][,'SSB (2017)'],res[[6]][[1]][,'SSB (2018)'],res[[6]][[1]][,'SSB (2019)'],res[[6]][[1]][,'SSB (2020)']))/1e3,lwd=2)
lines(seq(2009,2020),unlist(c(ssb(res[[7]][[2]])[,'2009'],res[[7]][[1]][,'SSB (2010)'],res[[7]][[1]][,'SSB (2011)'],res[[7]][[1]][,'SSB (2012)'],res[[7]][[1]][,'SSB (2013)'],res[[7]][[1]][,'SSB (2014)'],res[[7]][[1]][,'SSB (2015)'],res[[7]][[1]][,'SSB (2016)'],res[[7]][[1]][,'SSB (2017)'],res[[7]][[1]][,'SSB (2018)'],res[[7]][[1]][,'SSB (2019)'],res[[7]][[1]][,'SSB (2020)']))/1e3,lwd=2)
lines(seq(2008,2020),unlist(c(ssb(res[[8]][[2]])[,'2008'],res[[8]][[1]][,'SSB (2009)'],res[[8]][[1]][,'SSB (2010)'],res[[8]][[1]][,'SSB (2011)'],res[[8]][[1]][,'SSB (2012)'],res[[8]][[1]][,'SSB (2013)'],res[[8]][[1]][,'SSB (2014)'],res[[8]][[1]][,'SSB (2015)'],res[[8]][[1]][,'SSB (2016)'],res[[8]][[1]][,'SSB (2017)'],res[[8]][[1]][,'SSB (2018)'],res[[8]][[1]][,'SSB (2019)'],res[[8]][[1]][,'SSB (2020)']))/1e3,lwd=2)

lines(c(2017,2017),unlist(c(res[[1]][[1]][,'SSB (2017)'],res[[2]][[1]][,'SSB (2017)']))/1e3,col=fSelCol(delta[1]>0),lwd=3)
lines(c(2016,2016),unlist(c(res[[2]][[1]][,'SSB (2016)'],res[[3]][[1]][,'SSB (2016)']))/1e3,col=fSelCol(delta[2]>0),lwd=3)
lines(c(2015,2015),unlist(c(res[[3]][[1]][,'SSB (2015)'],res[[4]][[1]][,'SSB (2015)']))/1e3,col=fSelCol(delta[3]>0),lwd=3)
lines(c(2014,2014),unlist(c(res[[4]][[1]][,'SSB (2014)'],res[[5]][[1]][,'SSB (2014)']))/1e3,col=fSelCol(delta[4]>0),lwd=3)
lines(c(2013,2013),unlist(c(res[[5]][[1]][,'SSB (2013)'],res[[6]][[1]][,'SSB (2013)']))/1e3,col=fSelCol(delta[5]>0),lwd=3)
lines(c(2012,2012),unlist(c(res[[6]][[1]][,'SSB (2012)'],res[[7]][[1]][,'SSB (2012)']))/1e3,col=fSelCol(delta[6]>0),lwd=3)
lines(c(2011,2011),unlist(c(res[[7]][[1]][,'SSB (2011)'],res[[8]][[1]][,'SSB (2011)']))/1e3,col=fSelCol(delta[7]>0),lwd=3)

text(seq(2011,2017),rep(25,7),rev(paste0(round(100*delta.pct),"%")),cex=0.8)
text(2014,10,paste0(round(mean(100.0*delta.pct)),"%"))

dev.off()

mean(delta)


#plot of yield
plot(0,0,xlim=c(2005,2018),ylim=c(0,30),xlab="Year",ylab="Yield (kt)")
for (i in 1:8) {lines(seq(dims(res[[i]][[2]])$minyear,dims(res[[i]][[2]])$maxyear),catch(res[[i]][[2]])/1e3)}
lines(seq(2015,2020),unlist(c(catch(res[[1]][[2]])[,'2015'],res[[1]][[1]][,'Catch (2016)'],res[[1]][[1]][,'Catch (2017)'],res[[1]][[1]][,'Catch (2018)'],res[[1]][[1]][,'Catch (2019)'],res[[1]][[1]][,'Catch (2020)']))/1e3,lwd=2,col="red")
lines(seq(2014,2020),unlist(c(catch(res[[2]][[2]])[,'2014'],res[[2]][[1]][,'Catch (2015)'],res[[2]][[1]][,'Catch (2016)'],res[[2]][[1]][,'Catch (2017)'],res[[2]][[1]][,'Catch (2018)'],res[[2]][[1]][,'Catch (2019)'],res[[2]][[1]][,'Catch (2020)']))/1e3,lwd=2)
lines(seq(2013,2020),unlist(c(catch(res[[3]][[2]])[,'2013'],res[[3]][[1]][,'Catch (2014)'],res[[3]][[1]][,'Catch (2015)'],res[[3]][[1]][,'Catch (2016)'],res[[3]][[1]][,'Catch (2017)'],res[[3]][[1]][,'Catch (2018)'],res[[3]][[1]][,'Catch (2019)'],res[[3]][[1]][,'Catch (2020)']))/1e3,lwd=2)
lines(seq(2012,2020),unlist(c(catch(res[[4]][[2]])[,'2012'],res[[4]][[1]][,'Catch (2013)'],res[[4]][[1]][,'Catch (2014)'],res[[4]][[1]][,'Catch (2015)'],res[[4]][[1]][,'Catch (2016)'],res[[4]][[1]][,'Catch (2017)'],res[[4]][[1]][,'Catch (2018)'],res[[4]][[1]][,'Catch (2019)'],res[[4]][[1]][,'Catch (2020)']))/1e3,lwd=2)
lines(seq(2011,2020),unlist(c(catch(res[[5]][[2]])[,'2011'],res[[5]][[1]][,'Catch (2012)'],res[[5]][[1]][,'Catch (2013)'],res[[5]][[1]][,'Catch (2014)'],res[[5]][[1]][,'Catch (2015)'],res[[5]][[1]][,'Catch (2016)'],res[[5]][[1]][,'Catch (2017)'],res[[5]][[1]][,'Catch (2018)'],res[[5]][[1]][,'Catch (2019)'],res[[5]][[1]][,'Catch (2020)']))/1e3,lwd=2)
lines(seq(2010,2020),unlist(c(catch(res[[6]][[2]])[,'2010'],res[[6]][[1]][,'Catch (2011)'],res[[6]][[1]][,'Catch (2012)'],res[[6]][[1]][,'Catch (2013)'],res[[6]][[1]][,'Catch (2014)'],res[[6]][[1]][,'Catch (2015)'],res[[6]][[1]][,'Catch (2016)'],res[[6]][[1]][,'Catch (2017)'],res[[6]][[1]][,'Catch (2018)'],res[[6]][[1]][,'Catch (2019)'],res[[6]][[1]][,'Catch (2020)']))/1e3,lwd=2)
lines(seq(2009,2020),unlist(c(catch(res[[7]][[2]])[,'2009'],res[[7]][[1]][,'Catch (2010)'],res[[7]][[1]][,'Catch (2011)'],res[[7]][[1]][,'Catch (2012)'],res[[7]][[1]][,'Catch (2013)'],res[[7]][[1]][,'Catch (2014)'],res[[7]][[1]][,'Catch (2015)'],res[[7]][[1]][,'Catch (2016)'],res[[7]][[1]][,'Catch (2017)'],res[[7]][[1]][,'Catch (2018)'],res[[7]][[1]][,'Catch (2019)'],res[[7]][[1]][,'Catch (2020)']))/1e3,lwd=2)
lines(seq(2008,2020),unlist(c(catch(res[[8]][[2]])[,'2008'],res[[8]][[1]][,'Catch (2009)'],res[[8]][[1]][,'Catch (2010)'],res[[8]][[1]][,'Catch (2011)'],res[[8]][[1]][,'Catch (2012)'],res[[8]][[1]][,'Catch (2013)'],res[[8]][[1]][,'Catch (2014)'],res[[8]][[1]][,'Catch (2015)'],res[[8]][[1]][,'Catch (2016)'],res[[8]][[1]][,'Catch (2017)'],res[[8]][[1]][,'Catch (2018)'],res[[8]][[1]][,'Catch (2019)'],res[[8]][[1]][,'Catch (2020)']))/1e3,lwd=2)

