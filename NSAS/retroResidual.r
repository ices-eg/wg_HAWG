#- Plot the retrospective residual pattern in any fleet
retroResiduals <- function(x,fleet,yrs){

res <- lapply(x,residuals)
res <- lapply(res,function(y){y[which(y$fleet == fleet & y$year %in% yrs),]})
res <- lapply(res,function(y){cbind(y,retro=max(y$year))})
res <- do.call(rbind,res)

xyplot(std.res ~ age | as.factor(year),data=res,type="l",groups=retro,
auto.key=list(space="right",points=FALSE,lines=TRUE,type="l"),main=paste("Residual pattern in",fleet,"at age"),
ylab="Standardized residuals",xlab="Ages",
panel = panel.superpose,
 panel.groups = function(...) {
    panel.grid(v=-1,h=-1,lty=3)
    panel.xyplot(...)
},
scales=list(alternating=1,y=list(relation="free",rot=0)))
}
#- Plot the retrospective selectivity pattern
retroSelectivity <- function(x,yrs){

      res <- lapply(x,f)
      res <- lapply(res,function(y){y[which(y$year %in% (max(y$year)-20):(max(y$year)-1)),]})
      res <- lapply(res,function(y){cbind(y,retro=max(y$year))})
      res <- do.call(rbind,res)

      res <- subset(res,year %in% yrs)

      xyplot(value ~ age | as.factor(year),data=res,type="l",groups=retro,
      auto.key=list(space="right",points=FALSE,lines=TRUE,type="l"),main=paste("Retrospective pattern in F at age"),
      ylab="F",xlab="Ages",
      panel = panel.superpose,
       panel.groups = function(...) {
          panel.grid(v=-1,h=-1,lty=3)
          panel.xyplot(...)
      },
      scales=list(alternating=1,y=list(relation="free",rot=0)))
}

retroParams <- function(x){
  retroPars     <- lapply(x,params)
  subretroPars  <- lapply(retroPars,function(y){return(subset(y,name %in% c("logFpar","lowQpow","logSdLogFsta","logSdLogN","logSdLogObs","rec_loga",
                                                                            "rec_logb","rho","logScale","logScaleSSB","logPowSSB","logSdSSB")))})
  subretroPars  <- lapply(as.list(1:length(subretroPars)),function(y){return(cbind(year=names(subretroPars)[y],subretroPars[[y]]))})
  subretroPars  <- lapply(subretroPars,function(y){
                          lapply(as.list(names(table(ac(y$name)))),
                                 function(z){tmpy <- subset(y,name==z)
                                             tmpy$nameOrig <- tmpy$name
                                             if(nrow(tmpy)>1)
                                              tmpy$name <- paste(tmpy$name,1:nrow(tmpy))
                                             return(tmpy)})})
  subretroPars  <- do.call(rbind,lapply(subretroPars,function(y){do.call(rbind,y)}))

  print(xyplot(exp(value) ~ year | as.factor(nameOrig),data=subretroPars,groups=name,scale=list(y="free"),type="b",pch=19,xlab="Assessment year",ylab="Parameter value"))
  }
