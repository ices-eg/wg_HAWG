### ======================================================================================================
### Setting up
### ======================================================================================================

output.dir          <-  file.path(".","results/")        # figures directory
assessment_name_multifleet <- "IBP2018_multifleet"
assessment_name_singlefleet <- "IBP2018_singlefleet"

PDF <- T
PNG <- ifelse(PDF,F,T)
### ============================================================================
### single fleet
### ============================================================================

### ============================================================================
### Model fit
### ============================================================================

# figure - residual plots at each age for each time series
if(PDF) pdf(file.path(output.dir,paste(run_name,"_",assessment_name_multifleet,"_diagnostics_%02d.pdf",sep="")))
if(PNG) png(file.path(output.dir,paste(run_name,"_",assessment_name_multifleet,"_diagnostics_%02d.png",sep="")),units = "px", height=800,width=672, bg = "white")
residual.diagnostics(MSHm.sam)

# figure - assessment result, spawning stock biomass, fishing mortality, recruitment
plot(MSHm.sam,futureYrs=F)

# figure - catchabilities at age from HERAS
catch <- catchabilities(MSHm.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
       scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
       type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
       subset=fleet %in% c("MS HERAS","IBTS_Q1","IBTS_Q4"),
       main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

# figure - variance by data source
obv <- obs.var(MSHm.sam)
obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
obv <- obv[order(obv$value),]
bp <- barplot(obv$value,ylab="Observation Variance",
              main="Observation variances by data source",col=factor(obv$fleet))
axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

# figure - variance vs uncertainty for each data source
plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
     pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

# figure - fishing age selectivity per year
sel.pat <- merge(f(MSHm.sam),fbar(MSHm.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
xyplot(sel ~ age|sprintf("%i's",floor((year+2)/5)*5) * fleet,sel.pat,
       groups=year,type="l",as.table=TRUE,
       scale=list(alternating=FALSE),
       main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar")

# figure - correlation matrix of model parameters
print(cor.plot(MSHm.sam))

# figure - catch residuals per year per age
catchFleets <- unique(residuals(MSHm.sam)$fleet)[grep("catch",unique(residuals(MSHm.sam)$fleet))]
for(iCatch in catchFleets){
  dat <- subset(residuals(MSHm.sam),fleet==iCatch)
  print(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main=paste0("Residuals by year ",iCatch),
         panel=function(...){
           lst <- list(...)
           panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
        }))
}

# figure - acosutic index residuals per year per age
dat <- subset(residuals(MSHm.sam),fleet=="IBTS_Q1")
print(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year IBTS_Q1",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       }))
dat <- subset(residuals(MSHm.sam),fleet=="IBTS_Q4")
print(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year IBTS_Q4",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       }))
       
# figure - acosutic index residuals per year per age
dat <- subset(residuals(MSHm.sam),fleet=="MS HERAS")
print(xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year MS HERAS",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       }))


print(plot(MSHm.sam))

# process error in terms of N
MSH@harvest <- areaSums(MSHm.sam@harvest[,dimnames(MSH@catch.n)$year])#[,-which(dimnames(MSHm.sam@harvest)$year==range(MSHm.sam)["maxyear"])]
MSH@stock.n <- MSHm.sam@stock.n[,dimnames(MSH@catch.n)$year]#[,-which(dimnames(MSHm.sam@harvest)$year==range(MSHm.sam)["maxyear"])]
print(procerr.plot(MSH,weight="stock.wt",type="n",rel=T))

# process error in terms of additional mortality
print(procerr.plot(MSH,weight="stock.wt",type="mort",rel=F))

timeseries <- function(stck.,slot.,...){
                assign("stck.",stck.,envir=.GlobalEnv);assign("slot.",slot.,envir=.GlobalEnv);
                print(xyplot(data~year,data=slot(stck.,slot.),...,
                groups=age,
                auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
                type="b",
                xlab="Year",ylab=paste("Time series of",slot.,ifelse(units(slot(stck.,slot.))=="NA","",paste("(",units(slot(stck.,slot.)),")",sep=""))),
                main=paste(stck.@name,"timeseries of",slot.),
                par.settings=list(superpose.symbol=list(pch=as.character(0:8),cex=1.25))))}

# figure - times series for each age, stock
timeseries(window(MSH,1975,range(MSH)["maxyear"]),slot="stock.wt")

# figure - times series for each age, catches
timeseries(window(MSH,1975,range(MSH)["maxyear"]),slot="catch.wt")

# figure - times series for each age, harvest
timeseries(window(MSH,2000,range(MSH)["maxyear"]),slot="harvest")

# figure - times series for each age, maturity
timeseries(window(MSH,1990,range(MSH)["maxyear"]),slot="mat")

# figure - times series for each age, mortality
timeseries(window(MSH,1947,range(MSH)["maxyear"]),slot="m")


#Overlay time series by cohort and align by cohort
overlayTimeseries <- function(x,nyrs,ages){
                      require(doBy)
                      validObject(x)
                      if(class(x)!="FLQuants") stop("Object is not an FLQuants")
                      if(any(is.na(names(x))==T)) stop("Each FLQuant must have a name")
                      require(reshape)
                      lng   <- length(x)
                      dmns  <- list()
                      for(i in 1:lng)
                        dmns[[i]] <- dimnames(x[[i]])

                      ags   <- unique(unlist(lapply(dmns,function(x){return(x$age)})))
                      if("all" %in% ags){ x <- x[-which(ags == "all")]; dmns <- dmns[-which(ags == "all")]}
                      lng   <- length(x);
                      idx   <- lapply(dmns,function(x){any(x$age %in% ages)})
                      if(length(idx)>0){
                        x   <- x[unlist(idx)]; dmns <- dmns[unlist(idx)]}
                      lng   <- length(x)
                      yrs   <- range(unlist(lapply(x,function(y){dims(y)[c("minyear","maxyear")]})))

                      stk   <- data.frame()
                      for(i in 1:lng)
                        stk   <- rbind(stk,cbind(as.data.frame(rescaler(trim(window(x[[i]],start=(max(an(yrs))-(nyrs-1)),end=max(an(yrs))),age=c(max(ages[1],dims(x[[i]])$min):min(rev(ages)[1],dims(x[[i]])$max))))),qname=names(x)[i]))
                      stk$track <- stk$year - stk$age

                      stk <- orderBy(~age+qname+track,data=stk)
                      xyplot(data ~ track,data=stk,groups=qname,type="l",
                             prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},xlab="Cohort",ylab="Standardized timeseries",
                             auto.key=list(space="right",points=FALSE,lines=TRUE,type="l"),
                             panel = panel.superpose,
                             panel.groups = function(...) {
                              res <- list(...)
                              lng <- length(res$x)/nyrs
                              for(i in 1:lng){
                                panel.grid(v=-1,h=-1,lty=3)
                                panel.xyplot(res$x[(nyrs*i-nyrs+1):(nyrs*i)],res$y[(nyrs*i-nyrs+1):(nyrs*i)],lty=i,type="l",col=res$col.line)
                                panel.text(res$x[(nyrs*i-nyrs+1):(nyrs*i)],res$y[(nyrs*i-nyrs+1):(nyrs*i)],labels=stk$age[res$subscript[(nyrs*i-nyrs+1):(nyrs*i)]],col=res$col.line,cex=0.8)
                              }
                            },
                            scales=list(alternating=1,y=list(relation="free",rot=0)))
}


plot(MSH.tun[["MS HERAS"]],type="internal",main="MS HERAS")
plot(MSH.tun[["IBTS_Q1"]],type="internal",main="IBTS-Q1")
plot(MSH.tun[["IBTS_Q4"]],type="internal",main="IBTS-Q4")

print(plot(MSHm.retro,futureYrs=F))

# model parameters retrospective
retroParams(MSHm.retro)

# stock trajectory retrospective
yrs <- 2009:range(MSHm)["maxyear"]
res <- lapply(MSHm.retro, f)
res <- lapply(res, function(y) {
    y[which(y$year %in% (max(y$year) - 20):(max(y$year) -
        1)), ]
})
res <- lapply(res, function(y) {
    cbind(y, retro = max(y$year))
})
res <- do.call(rbind, res)
res <- subset(res, year %in% yrs)
    print(xyplot(value ~ an(age) | as.factor(year) , data = subset(res,fleet=="catch S"),
        type = "l", groups = retro, auto.key = list(space = "right",
            points = FALSE, lines = TRUE, type = "l"), main = paste("Retrospective pattern in F at age South"),
        ylab = "F", xlab = "Ages", panel = panel.superpose, panel.groups = function(...) {
            panel.grid(v = -1, h = -1, lty = 3)
            panel.xyplot(...)
        }, scales = list(alternating = 1, y = list(relation = "free",
            rot = 0))))

    print(xyplot(value ~ an(age) | as.factor(year) , data = subset(res,fleet=="catch N"),
        type = "l", groups = retro, auto.key = list(space = "right",
            points = FALSE, lines = TRUE, type = "l"), main = paste("Retrospective pattern in F at age North"),
        ylab = "F", xlab = "Ages", panel = panel.superpose, panel.groups = function(...) {
            panel.grid(v = -1, h = -1, lty = 3)
            panel.xyplot(...)
        }, scales = list(alternating = 1, y = list(relation = "free",
            rot = 0))))


dev.off()

