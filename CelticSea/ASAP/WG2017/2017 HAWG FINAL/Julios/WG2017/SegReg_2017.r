#setwd("C:/ICES/HAWG2016/Julios/WG2016")
#setwd("C:/ICES/HAWG2016/Julios/WG2015")
#setwd("L:/PERSONAL/Afra/ASAP/2017 HAWG FINAL/Julios/WG2017")
setwd("C:/Users/momalley/Desktop/HAWG 2017/2017 HAWG FINAL/Julios/WG2017")

rm(list=ls())

source("funcs.r")

#9 plots
layout(matrix(c(1,2,3,4,5,6,7,8,9), 3, 3, byrow = TRUE))

#request SR data file
#file should contain columns named year, R and SSB
SRdatafile <- winDialogString("SR Data File?","sr.data.txt")
#SRdatafile <- winDialogString("SR Data File?","SegReg_000.csv")
#SRdatafile <- winDialogString("SR Data File?","SegReg_001.csv")
#SRdatafile <- winDialogString("SR Data File?","SegReg_002.csv")
#SRdatafile <- winDialogString("SR Data File?","SegReg_003.csv")
#SRdatafile <- winDialogString("SR Data File?","SegReg_004.csv")
#SRdatafile <- winDialogString("SR Data File?","SegReg_005.csv")
#SRdatafile <- winDialogString("SR Data File?","SegReg_006.csv")
#SRdatafile <- winDialogString("SR Data File?","SegReg_007.csv")

#output filename?
outfile <- winDialogString("Output data file?",paste(SRdatafile,".out",sep=""))

sink(outfile)

#age at recruitment?
RecAge <- winDialogString("Recruitment Age?","0")

#read in the data
sr.data <- read.table(SRdatafile, header=T)

#the recruitment data series needs to be offset by the recruitment age in
#order that the values correspond to the appropriate SSB values
if (as.integer(RecAge)>0) {
   for (j in 1:as.integer(RecAge)) {
      #shift the recruitment vector
      for (i in 1:(length(sr.data$R)-1)) {
	 sr.data$R[i]<-sr.data$R[i+1]
      }
   }

   Recr<-sr.data$R[1:(length(sr.data$R)-as.integer(RecAge))]
   SSB<-sr.data$SSB[1:(length(sr.data$SSB)-as.integer(RecAge))]
   Year<-sr.data$year[1:(length(sr.data$year)-as.integer(RecAge))]

} else {

   Recr<-sr.data$R
   SSB<-sr.data$SSB
   Year<-sr.data$year

}

#R vs SSB plot
plot(SSB,
     Recr, 
     col=4, 
     xlab="SSB",           
     ylab="Recruitment", 
     xlim=c(0,max(SSB)), 
     ylim=c(0,max(Recr)))

#julios algorithm
jul.mod <- slm2.sr(SSB,Recr)

#print data
print("*****jul.mod*****")
print(jul.mod)

jul.mod <- changepoint.sr(SSB, Recr, delta=jul.mod$delta, constrained=T)

#grid search
gs.mod <- slm.sr(SSB, Recr, grid=500)

#print data
print("*****gs.mod*****")
print(gs.mod)

gs.mod <- changepoint.sr(SSB, Recr, delta=gs.mod$delta, constrained=T)

#plot showing data and changepoint
plot(SSB, Recr, type="n", xlab="SSB", ylab="Recruitment",main="Stock Recruit Relationship", xlim=c(0,max(SSB)), ylim=c(0,max(Recr)),cex.axis=0.8)
text(SSB, Recr, Year, cex=0.5)
lines( c(0,jul.mod$delta), c(0, jul.mod$beta1*jul.mod$delta), lty=1)
lines( c(jul.mod$delta, max(SSB)), c(jul.mod$alpha2, jul.mod$alpha2), lty=1)
abline(v=jul.mod$delta, col=2, lty=2)

#q-q plot
qqnorm(jul.mod$res)
qqline(jul.mod$res)

#histogram of residuals
hist(jul.mod$res, nclass=10)

n <- length(Year)

#work backwards, removing 1 year each time up to 10 years back
jul.mod00a<-slm2.sr(SSB[1:c(n-0)],Recr[1:c(n-0)])
jul.mod01a<-slm2.sr(SSB[1:c(n-1)],Recr[1:c(n-1)])
jul.mod02a<-slm2.sr(SSB[1:c(n-2)],Recr[1:c(n-2)])
jul.mod03a<-slm2.sr(SSB[1:c(n-3)],Recr[1:c(n-3)])
jul.mod04a<-slm2.sr(SSB[1:c(n-4)],Recr[1:c(n-4)])
jul.mod05a<-slm2.sr(SSB[1:c(n-5)],Recr[1:c(n-5)])
jul.mod06a<-slm2.sr(SSB[1:c(n-6)],Recr[1:c(n-6)])
jul.mod07a<-slm2.sr(SSB[1:c(n-7)],Recr[1:c(n-7)])
jul.mod08a<-slm2.sr(SSB[1:c(n-8)],Recr[1:c(n-8)])
jul.mod09a<-slm2.sr(SSB[1:c(n-9)],Recr[1:c(n-9)])
jul.mod10a<-slm2.sr(SSB[1:c(n-10)],Recr[1:c(n-10)])

#vector of delta values
delta.a <- c(jul.mod00a$delta,jul.mod01a$delta,jul.mod02a$delta,jul.mod03a$delta,
	     jul.mod04a$delta,jul.mod05a$delta,jul.mod06a$delta,jul.mod07a$delta,
	     jul.mod08a$delta,jul.mod09a$delta,jul.mod10a$delta)

print("*****delta.a*****")
print(delta.a)

#year vector
yr <- seq(max(Year)-10,max(Year))

plot(yr, delta.a, type="l", ylim=c(0, c(round(max(delta.a)/1000)*1500)))

#S-R plot showing all breakpoints
plot(SSB,
     Recr,
     type="n",
     xlab="SSB",
     ylab="Recruitment",
     xlim=c(0,max(SSB)),
     ylim=c(0,max(Recr)))

text(SSB,Recr,Year,cex=0.5)

lines(c(0,jul.mod$delta),c(0,jul.mod$beta1*jul.mod$delta),col="dark red",lwd=2,lty=1)
lines(c(jul.mod$delta, max(SSB)), c(jul.mod$alpha2, jul.mod$alpha2), col="dark red", lwd=2, lty=1)
abline(v=jul.mod$delta, col=2, lty=2)
lines( c(1, jul.mod01a$delta), c(0, jul.mod01a$beta1*jul.mod01a$delta), lty=2)
lines( c(jul.mod01a$delta, max(SSB)), c(jul.mod01a$alpha2, jul.mod01a$alpha2), lty=2)
lines( c(0, jul.mod02a$delta), c(0, jul.mod02a$beta1*jul.mod02a$delta), lty=2)
lines( c(jul.mod02a$delta, max(SSB)), c(jul.mod02a$alpha2, jul.mod02a$alpha2), lty=2)
lines( c(0, jul.mod03a$delta), c(0, jul.mod03a$beta1*jul.mod03a$delta), lty=2)
lines( c(jul.mod03a$delta, max(SSB)), c(jul.mod03a$alpha2, jul.mod03a$alpha2), lty=2)
lines( c(0, jul.mod04a$delta), c(0, jul.mod04a$beta1*jul.mod04a$delta), lty=2)
lines( c(jul.mod04a$delta, max(SSB)), c(jul.mod04a$alpha2, jul.mod04a$alpha2), lty=2)
lines( c(0, jul.mod05a$delta), c(0, jul.mod05a$beta1*jul.mod05a$delta), lty=2)
lines( c(jul.mod05a$delta, max(SSB)), c(jul.mod05a$alpha2, jul.mod05a$alpha2), lty=2)
lines( c(0, jul.mod06a$delta), c(0, jul.mod06a$beta1*jul.mod06a$delta), lty=2)
lines( c(jul.mod06a$delta, max(SSB)), c(jul.mod06a$alpha2, jul.mod06a$alpha2), lty=2)
lines( c(0, jul.mod07a$delta), c(0, jul.mod07a$beta1*jul.mod07a$delta), lty=2)
lines( c(jul.mod07a$delta, max(SSB)), c(jul.mod07a$alpha2, jul.mod07a$alpha2), lty=2)
lines( c(0, jul.mod08a$delta), c(0, jul.mod08a$beta1*jul.mod08a$delta), lty=2)
lines( c(jul.mod08a$delta, max(SSB)), c(jul.mod08a$alpha2, jul.mod08a$alpha2), lty=2)
lines( c(0, jul.mod09a$delta), c(0, jul.mod09a$beta1*jul.mod09a$delta), lty=2)
lines( c(jul.mod09a$delta, max(SSB)), c(jul.mod09a$alpha2, jul.mod09a$alpha2), lty=2)
lines( c(0, jul.mod10a$delta), c(0, jul.mod10a$beta1*jul.mod10a$delta), lty=2)
lines( c(jul.mod10a$delta, max(SSB)), c(jul.mod10a$alpha2, jul.mod10a$alpha2), lty=2)

#now remove individual years
deltas<-c()
for (i in 1:length(Year)) {
   jul.mod<-slm2.sr(SSB[-i],Recr[-i])
   deltas<-c(deltas,jul.mod$delta)

}

print("*****deltas*****")
print(deltas)

plot(Year, deltas, type="l",  ylim=c(0, c(round(max(deltas)/1000)*1500)))

mean.mod <- lm(log(Recr)~1)
mean.ssq <- sum(residuals(mean.mod)^2)
print("*****Summary(mean.mod)*****")
print(summary(mean.mod))
print("*****mean.ssq*****")
print(mean.ssq)

line.mod <- lm(log(Recr)~1, offset=log(SSB))
line.ssq <- sum(residuals(line.mod)^2)
print("*****summary(line.mod)*****")
print(summary(line.mod))
print("*****line.ssq*****")
print(line.ssq)

f.obs <- (mean.ssq - jul.mod$ssq)*(n-2)/jul.mod$ssq

print("*****f.obs*****")
print(f.obs)

date()
f <- boot.sr(SSB, Recr, iter=1000)
date()

plot(ecdf(f$f.all), verticals=T, do.p=F, xlab="F statistic", main="Empirical distribution")
abline(v=f$f.obs, lty=2, col=2)

print("*****f*****")
print(f)

sink()


#SR plot + fit
#1 plot
jpeg(file="JuliosFitSR.jpg",height=500,width=800)

layout(matrix(c(1), 1, 1, byrow = TRUE))

plot(SSB/1000, Recr/1e3, col=4, xlab="SSB (kt)", ylab="Recruitment (Millions)", 
     xlim=c(0,175), ylim=c(0,max(Recr/1e3)),pch=19,bty="n",axes=F)
text(SSB/1000,Recr/1e3,Year,cex=0.75,pos=3)
lines(c(0,(537900/1e3)/9.462927,max(SSB/1000)),c(0,537900/1e3,537900/1e3))
lines(c((537900/1e3)/9.462927,(537900/1e3)/9.462927),c(0,537900/1e3),lty=2)
axis(side=1,at=c(0,25,50,75,100,125,150,175))
axis(side=2,at=c(0,250,500,750,1000,1250,1500))

dev.off()
