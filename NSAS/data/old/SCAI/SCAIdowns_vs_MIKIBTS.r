library(FLSAM)

path <- "D:/Repository/ICES_HAWG/wg_HAWG/NSAS/"
try(setwd(path),silent=TRUE)

source("./data/SCAI/SCAI_model.r")
dwns     <- subset(base.res,year >= 1992 & comp == "Downs")$value
std.DOWNS<- (dwns - mean(dwns))/sd(dwns)

p.DOWNS <- (SCAIs[21:45,area.names]/rowSums(SCAIs[21:45,area.names]))[,"Downs"]
std.DOWNS <- (p.DOWNS - mean(p.DOWNS)) / sd(p.DOWNS)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
r01.DOWNS <- range01(p.DOWNS)

source("D:/Repository/ICES_HAWG/wg_HAWG/NSAS/setupAssessmentObjects.r")

std.IBTS <- (NSH.tun[["IBTS-Q1"]]@index - mean(NSH.tun[["IBTS-Q1"]]@index)) / sd(NSH.tun[["IBTS-Q1"]]@index)
std.MIK  <- (NSH.tun[["IBTS0"]]@index - mean(NSH.tun[["IBTS0"]]@index)) / sd(NSH.tun[["IBTS0"]]@index)

std.IBTS <- window(std.IBTS,start=1992,end=2016)
std.MIK  <- window(std.MIK,start=1992,end=2016)

cor(c(std.IBTS[,-1]/std.MIK[,-25])[,drop=T],r01.DOWNS[-25])

cor(diff(std.IBTS[,-1]),diff(std.MIK[,-25]))
cor((std.IBTS[,-1] - std.MIK[,-25])[,drop=T],std.DOWNS[-1])

plot(abs((std.IBTS[,-1] - std.MIK[,-25])[,drop=T]),type="l",ylim=c(-2,2))
lines(std.DOWNS[-1],col=2)
abline(a=0,b=1)

par(mfrow=c(3,1),oma=c(6,2,2,6),mar=c(0,4,0,2))
#-Panel 1: standardized survey
plot(y=std.IBTS[,drop=T],x=1992:2016,type="l",las=1,xaxt="n",ylim=c(-1.4,2.4),ylab="Index",font.lab=2)
grid()
lines(y=std.IBTS[,drop=T],x=1992:2016,col=1)
lines(y=std.MIK[,drop=T],x=1993:2017,col=2)

#- Panel 2: rate of change
plot(y=diff(std.IBTS[,-1,drop=T]),x=1994:2016,type="l",ylab="Rate of change",las=1,font.lab=2,xaxt="n",ylim=c(-3,3))
grid()
lines(y=diff(std.IBTS[,-1,drop=T]),x=1994:2016,col=1)
lines(y=diff(std.MIK[,-25,drop=T]),x=1994:2016,col=2)

#- Panel 3: Ratio vs p.DOWNS
dd <- diff(std.IBTS[,-1,drop=T])/diff(std.MIK[,-25,drop=T])
dd[19] <- NA
plot(y=dd,x=1994:2016,type="l",ylab="Ratio",las=1,font.lab=2,xlab="Years",font.lab=2,ylim=c(-8,5))
grid()
lines(y=dd,x=1994:2016,type="l")
abline(h=1,lty=2,col="darkgrey")
par(new=T)
plot(y=p.DOWNS,x=1992:2016,col=3,xaxt="n",yaxt="n",type="l",xlab="",ylab="")
abline(h=mean(p.DOWNS),lty=2,col="darkgreen")
axis(4,las=1)
mtext(side=4,text="Proportion Downs",outer=T,at=0.18,line=2,font=2,cex=0.7)





plot((diff(std.IBTS[,-1,drop=T])-diff(std.MIK[,-25,drop=T])),type="l")
lines(std.DOWNS,col=3)
lines(r01.DOWNS,col=4)
lines(diff(std.DOWNS[-25]),col=4)
abline(h=0,col=2,lwd=2)

cor(abs(diff(std.IBTS[,-1,drop=T])-diff(std.MIK[,-25,drop=T])),abs(diff(std.DOWNS[-c(25)])))

plot(abs(diff(std.IBTS[,-1,drop=T])-diff(std.MIK[,-25,drop=T])) ~abs(diff(std.DOWNS[-25])))


dIBTS <- diff(std.IBTS[,-1,drop=T])
dMIK  <- diff(std.MIK[,-25,drop=T])
dDOWNS<- diff(std.DOWNS[-c(25)])
dd    <- (dIBTS/dMIK)
dd[19] <- NA

std.dd <- (dd-mean(dd))/sd(dd)

plot(dIBTS,type="l"); lines(dMIK,col=2); lines(dDOWNS,col=3)
 
plot(y=dd-1,x=1994:2016,ylim=c(-10,10),type="l")
lines(y=r01.DOWNS[-25]*5-0.5,x=1993:2016,col=2)
cor(dd[-c(19)],r01.DOWNS[-c(19,24:25)])


dabs.IBTS <- diff(NSH.tun[[3]]@index[,ac(1993:2016),drop=T])
dabs.MIK  <- diff(NSH.tun[[4]]@index[,ac(1992:2015),drop=T])

std.IBTS <- (dabs.IBTS - mean(dabs.IBTS))/sd(dabs.IBTS)
std.MIK  <- (dabs.MIK - mean(dabs.MIK))/sd(dabs.MIK)

cor(std.IBTS,std.MIK)