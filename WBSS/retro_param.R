#- Plot the retrospective parameter estimates
retroParam <- function(x,...){
  nopar <- x@nopar-1 # -1 to omit 'rho'
  est.param <- x@params[1:nopar,]$value
  ci.param <- x@params[1:nopar,]$std.dev

  par(mfrow=c(2,2), omi=c(0.8,0.3,0.1,0.1))
  n <- 1:7
  plot(est.param[n], ylab="Estimate", xlab="Age",xaxt="n", ylim=range(est.param[n]+1.96*ci.param[n], est.param[n]-1.96*ci.param[n]), main="HERAS")
  for(i in n){
    lines(rep(match(i,n),2),c(est.param[i]+1.96*ci.param[i], est.param[i]-1.96*ci.param[i]))}
  axis(1, at=1:length(n), labels=1:7)
#axis(1, at=1:nopar, labels=x@params[1:nopar,]$name, las=3)

  n <- 8:15
  plot(est.param[n], ylab="Estimate", xlab="Age",xaxt="n", ylim=range(est.param[n]+1.96*ci.param[n], est.param[n]-1.96*ci.param[n]), main="GerAS")
  for(i in n){
    lines(rep(match(i,n),2),c(est.param[i]+1.96*ci.param[i], est.param[i]-1.96*ci.param[i]))}
  axis(1, at=1:length(n), labels=0:7)
#axis(1, at=1:nopar, labels=x@params[1:nopar,]$name, las=3)

  n <- 17:24
  plot(est.param[n], ylab="Estimate", xlab="Age",xaxt="n", ylim=range(est.param[n]+1.96*ci.param[n], est.param[n]-1.96*ci.param[n]), main="IBTS")
  for(i in n){
    lines(rep(match(i,n),2),c(est.param[i]+1.96*ci.param[i], est.param[i]-1.96*ci.param[i]))}
  axis(1, at=1:length(n), labels=rep(1:4,2))
#axis(1, at=1:nopar, labels=x@params[1:nopar,]$name, las=3)

  n <- 25:41
  plot(est.param[n], ylab="Estimate", xlab="",xaxt="n", ylim=range(est.param[n]+1.96*ci.param[n], est.param[n]-1.96*ci.param[n]), main="Variance parameters")
  for(i in n){
    lines(rep(match(i,n),2),c(est.param[i]+1.96*ci.param[i], est.param[i]-1.96*ci.param[i]))}
  axis(1, at=1:length(n), labels=x@params[n,]$name, las=3)
}

