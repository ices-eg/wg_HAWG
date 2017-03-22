
### This code extracts the Fs and Ns from each of the years in the ASAP retrospective
### This may be used in an extended STF

library(lattice)


# user input

# number of runs
n <- 6


run<-"2017 ASSESSMENT FINAL_RETRO"

runs <- paste(run,'_',sprintf("%03d",0:(n-1)),sep='')

# first age 
firstage <- 1

#dir1<-("W:/PERSONAL/Afra/HAWG 2015/Celtic Sea/Final 2015 new nat mor/")
#dir1<-("L:/PERSONAL/Afra/ASAP/2017 HAWG/")
dir1<-("C:/Users/momalley/Desktop/HAWG 2017/2017 HAWG FINAL/")


n <- length(runs)

REPfile <- NULL
REP <- NULL
DATfile <- NULL
DAT <- NULL
nyears <- NULL
years <- NULL
for(i in 1:n) {
  REPfile[[i]] <- paste(dir1,runs[i],'.REP',sep='')
  REP[[i]] <- readLines(REPfile[[i]])
  DATfile[[i]] <- paste(dir1,runs[i],'.DAT',sep='')
  DAT[[i]] <- readLines(DATfile[[i]])
  j <- which(DAT[[i]]=='# Number of Years')
  nyears[[i]] <- read.table(DATfile[[i]],header=F,skip=j,nrows=1)[,]
  j <- which(DAT[[i]]=='# First Year')
  year1 <- read.table(DATfile[[i]],header=F,skip=j,nrows=1)[,]
  years[[i]] <- year1:(year1+nyears[[i]]-1)
}

#### Loop to extract the Fs and Ns for each retro run


TotalF <- NULL
Numbers <- NULL
for(j in 1:n) {
  i <- which(REP[[j]]=='Total F')
  TotalF[[j]] <- read.table(REPfile[[j]],header=F,skip=i,nrows=nyears[[j]])
  i <- which(REP[[j]]=='Population Numbers at the Start of the Year')
  Numbers[[j]] <- read.table(REPfile[[j]],header=F,skip=i,nrows=nyears[[j]])
}

### Wite out the Fs and Ns from each retro run in an individual csv file


retro_2016_N<-data.frame(Numbers[1])
retro_2015_N<-data.frame(Numbers[2])
retro_2014_N<-data.frame(Numbers[3])
retro_2013_N<-data.frame(Numbers[4])
retro_2012_N<-data.frame(Numbers[5])
retro_2011_N<-data.frame(Numbers[6])

ages<-1:9
colnames(retro_2016_N) <- paste('Age',ages)
colnames(retro_2015_N) <- paste('Age',ages)
colnames(retro_2014_N) <- paste('Age',ages)
colnames(retro_2013_N) <- paste('Age',ages)
colnames(retro_2012_N) <- paste('Age',ages)
colnames(retro_2011_N) <- paste('Age',ages)


write.csv(retro_2016_N,file=paste("retro_2016_N.csv",sep=""),row.names=F)
write.csv(retro_2015_N,file=paste("retro_2015_N.csv",sep=""),row.names=F)
write.csv(retro_2014_N,file=paste("retro_2014_N.csv",sep=""),row.names=F)
write.csv(retro_2013_N,file=paste("retro_2013_N.csv",sep=""),row.names=F)
write.csv(retro_2012_N,file=paste("retro_2012_N.csv",sep=""),row.names=F)
write.csv(retro_2011_N,file=paste("retro_2011_N.csv",sep=""),row.names=F)


retro_2016_F<-data.frame(TotalF[1])
retro_2015_F<-data.frame(TotalF[2])
retro_2014_F<-data.frame(TotalF[3])
retro_2013_F<-data.frame(TotalF[4])
retro_2012_F<-data.frame(TotalF[5])
retro_2011_F<-data.frame(TotalF[6])

ages<-1:9
colnames(retro_2016_F) <- paste('Age',ages)
colnames(retro_2015_F) <- paste('Age',ages)
colnames(retro_2014_F) <- paste('Age',ages)
colnames(retro_2013_F) <- paste('Age',ages)
colnames(retro_2012_F) <- paste('Age',ages)
colnames(retro_2011_F) <- paste('Age',ages)


write.csv(retro_2016_F,file=paste("retro_2016_F.csv",sep=""),row.names=F)
write.csv(retro_2015_F,file=paste("retro_2015_F.csv",sep=""),row.names=F)
write.csv(retro_2014_F,file=paste("retro_2014_F.csv",sep=""),row.names=F)
write.csv(retro_2013_F,file=paste("retro_2013_F.csv",sep=""),row.names=F)
write.csv(retro_2012_F,file=paste("retro_2012_F.csv",sep=""),row.names=F)
write.csv(retro_2011_F,file=paste("retro_2011_F.csv",sep=""),row.names=F)

