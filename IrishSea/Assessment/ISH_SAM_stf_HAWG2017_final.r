### ============================================================================
### Short term forecast
### ============================================================================
### ============================================================================
### Input data
###=============================================================================
library(FLAssess)
library(FLash)

#survivors
dmns <- dims(ISH@stock.n)
gm.recruitmentEstimate <- exp(mean(log(ISH@stock.n[1,as.character((ISH@range['maxyear']-12):(ISH@range['maxyear']-2)),,,,])))
survivors <- FLQuant(c(gm.recruitmentEstimate,stock.n(ISH)[,ac(ISH@range['maxyear'])] * exp(-harvest(ISH[,ac(ISH@range['maxyear'])])-m(ISH[,ac(2015)]))),
                               dimnames=list(ages=dmns$min:(dmns$max+1),year=ISH@range['maxyear']+1,unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter))



## plusgroup
survivors[ac(dmns$max),ac(ISH@range['maxyear']+1)] <- quantSums(survivors[ac(dmns$max:(dmns$max+1)),ac(ISH@range['maxyear']+1)])
survivors           <- survivors[ac(dmns$min:dmns$max),]

#Update
#ISH.ctrl <- update(ISH.ctrl)

# useful commmands

#getwd()
#plot(rec(stk)) for scatter plot of recruitmen or for ssb ssb(stk)


#FLR2SAM(stck,tun,ctrl,run.dir=".")  #makes the wd the rund directory

#packageDescription("FLSAM")

#save(stck,ctrl,tun,file="IrishSH.RData") # as I changed the wd then it saves the files to that directory

#write parameters to create sen file
write.csv(ISH.sam@params,file=file.path(output.dir,"ISHsam parameters.csv"))

### ======================================================================================================
### Projections
### ======================================================================================================
FnPrint("CALCULATING PROJECTIONS...\n")

#Define years
TaY <- dims(ISH)$maxyear   #Terminal assessment year
ImY <- TaY+1                      #Intermediate Year
AdY <- TaY+2                      #Advice year
CtY <- TaY+3                      #Continuation year - not of major concern but used in calculations in places
tbl.yrs <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table

#Deal with recruitment - a geometric mean of the 10 years prior to the terminal assessment year
dmns                <- dims(ISH@stock.n)
rec.years <- ((ISH@range['maxyear']-12):(ISH@range['maxyear']-2));
gm.recs  <- exp(mean(log(rec(ISH)[,as.character(rec.years)])));
ISH.srr <- list(model="geomean",params=FLPar(gm.recs));


#Expand stock object
ISH.proj <- stf(ISH,nyears=4,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE);
#ISH.proj@stock.n[-7,ac(ImY)]   <- FLQuant(c(gm.recs,stock.n(ISH)[,ac(ISH@range['maxyear'])] * exp(-harvest(ISH[,ac(ISH@range['maxyear'])])-m(ISH[,ac(2012)]))),
#                               dimnames=list(ages=dmns$min:(dmns$max+1),year=ISH@range['maxyear']+1,unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter))
#
survivors           <- FLQuant(c(gm.recs,stock.n(ISH)[,ac(ISH@range['maxyear'])] * exp(-harvest(ISH[,ac(ISH@range['maxyear'])])-m(ISH[,ac(TaY)]))),
                               dimnames=list(ages=dmns$min:(dmns$max+1),year=ISH@range['maxyear']+1,unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter))

## plusgroup
survivors[ac(dmns$max),ac(ISH@range['maxyear']+1)] <- quantSums(survivors[ac(dmns$max:(dmns$max+1)),ac(ISH@range['maxyear']+1)])
survivors           <- survivors[ac(dmns$min:dmns$max),]


#ISH.proj@stock.n[-1,ac(ImY)]  <- setPlusGroup(ISH@stock.n[,ac(TaY)] * exp(-ISH@harvest[,ac(TaY)] - ISH@m[,ac(TaY)]),7)

ISH.proj@stock.n[,ac(ImY)]<-survivors
ISH.proj@stock.n[1,as.character(c(ImY,AdY,CtY))] <- gm.recs;

#Define some constants 2011
#ImY.catch <- 16196;
#AdY.catch <- 13200;
#numFmsy <- 0.25;

#For 2017
ImY.catch <- 4127;  
#2017 real advice year
numFmsy <- 0.266;
numFlim <- 0.397;
numFpa <- 0.286;

#Setup options

options.l <- list("Catch = Fmsy"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,numFmsy,numFmsy))),
                  #Zero catch
                  "Catch = Zero"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity="catch",
                                          val=c(ImY.catch,0,0))),
                  # TAC, -15% 
                  "Catch = TAC -15%"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,ImY.catch*0.85,NA))),
                  #TAC sq
                  "Catch = TAC sq"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,ImY.catch*1,NA))),
                  #TAC +15%
                  "Catch = TAC +15%"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,ImY.catch*1.15,NA))), 
                  #Fsq
                  "F = F sq"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,fbar(ISH)[,ac(TaY)],fbar(ISH)[,ac(TaY)]))),
                  #Flim
                  "F = Flim"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,numFlim,numFlim))),
                  #Fpa
                  "F = Fpa"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,numFpa,numFpa))),
                  
                  
                  "SSB = Bpa"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","ssb","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,11831,NA))),

                  "SSB = Blim"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","ssb","f"),
                                          rel=c(NA,NA,NA),
                                          val=c(ImY.catch,8500,NA))),


              #TAC +15%
                  "Catch = TAC +15%"=
                   fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                       quantity=c("catch","catch","f"),
                                        rel=c(NA,NA,AdY),
                                        val=c(ImY.catch,ImY.catch*1.15,NA)))) #End options list
#End options list



#Multi-options table
fmult.targs  <- seq(0,2,by=0.1)
mult.opts.l <- lapply(as.list(fmult.targs),function(fmult) {
  fwdControl(data.frame(year=c(ImY,AdY,CtY),
                        quantity=c("catch","f","f"),
                        rel=c(NA,ImY,AdY),
                        val=c(ImY.catch,fmult,NA)))
})
names(mult.opts.l) <- sprintf("Fmult(2013) = %4.3f",fmult.targs)

#Calculate options
ISH.options   <- lapply(options.l,function(ctrl) {fwd(ISH.proj,ctrl=ctrl,sr=ISH.srr)})
ISH.mult.opts <- lapply(mult.opts.l,function(ctrl) {fwd(ISH.proj,ctrl=ctrl,sr=ISH.srr)})


### ======================================================================================================
### Write Options Tables
### ======================================================================================================
#FnPrint("WRITING OPTIONS TABLES...\n")
output.base <- my.path

#Document input settings
input.tbl.file <-paste(output.base,"options - input.csv",sep=".")
write.table(NULL,file=input.tbl.file,col.names=FALSE,row.names=FALSE)
input.tbl.list <- list(N="stock.n",M="m",Mat="mat",PF="harvest.spwn",
                       PM="m.spwn",SWt="stock.wt",Sel="harvest",CWt="catch.wt")
for(yr in c(ImY,AdY,CtY)){
  col.dat <- sapply(input.tbl.list,function(slt) slot(ISH.proj,slt)[,as.character(yr),drop=TRUE])
  write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
  write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

#Detailed options table
options.file <-paste(output.base,"options - details.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(ISH.options)) {
  opt <- names(ISH.options)[i]
  stk <- ISH.options[[opt]]
  #Now the F and N by age
  nums.by.age <- stk@stock.n[,tbl.yrs,drop=TRUE]
  colnames(nums.by.age) <- sprintf("N(%s)",tbl.yrs)
  f.by.age    <- stk@harvest[,tbl.yrs,drop=TRUE]
  colnames(f.by.age) <- sprintf("F(%s)",tbl.yrs)
  age.tbl     <- cbind(Age=rownames(f.by.age),N=nums.by.age,F=f.by.age)
  #And now the summary tbl
  sum.tbl     <- cbind(Year=tbl.yrs,SSB=ssb(stk)[,tbl.yrs],
                       F.bar=fbar(stk)[,tbl.yrs],Yield=computeCatch(stk)[,tbl.yrs])
  #Now, bind it all together
  sum.tbl.padding <- matrix("",nrow=nrow(age.tbl)-nrow(sum.tbl),ncol=ncol(sum.tbl))
  comb.tbl    <- cbind(age.tbl," ",rbind(sum.tbl,sum.tbl.padding))
  #And write it - hdr first, then the rest
  write.table(sprintf("%s). %s",letters[i],opt),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
  write.table(t(colnames(comb.tbl)),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
  write.table(comb.tbl,options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
  write.table(c(""),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
}

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
                              SSB.CtY=ssb(stk)[,as.character(CtY),drop=TRUE])
  })
  options.sum.tbl <- t(options.sum.tbl)
  colnames(options.sum.tbl) <- c("Rationale",
                                 sprintf("Fbar (%i)",ImY),sprintf("Catch (%i)",ImY),sprintf("SSB (%i)",ImY),
                                 sprintf("Fbar (%i)",AdY),sprintf("Catch (%i)",AdY),sprintf("SSB (%i)",AdY),
                                 sprintf("SSB (%i)",CtY))
  write.csv(options.sum.tbl,file=fname,row.names=FALSE)
}
opt.sum.tbl(stcks=ISH.options,fname=paste(output.base,"options - summary.csv",sep="."))
opt.sum.tbl(stcks=ISH.mult.opts,fname=paste(output.base,"multi-options - summary.csv",sep="."))
