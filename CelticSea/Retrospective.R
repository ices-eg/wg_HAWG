library(lattice)

# number of runs
n <- 6
  

run<-"2017 ASSESSMENT FINAL_RETRO"

runs <- paste(run,'_',sprintf("%03d",0:(n-1)),sep='')

# first age 
firstage <- 1


#dir1<-("W:/PERSONAL/Afra/HAWG 2015/Celtic Sea/Final 2015 new nat mor/")

#dir2<-("W:/PERSONAL/Afra/HAWG 2015/Celtic Sea/Final 2015 new nat mor/more plots")

#dir1<-("L:/PERSONAL/Afra/ASAP/2017 HAWG/")
#dir2<-("L:/PERSONAL/Afra/ASAP/2017 HAWG/more plots")
dir1<-("C:/Users/momalley/Desktop/HAWG 2017/2017 HAWG FINAL/")
dir2<-("C:/Users/momalley/Desktop/HAWG 2017/2017 HAWG FINAL/more plots")

if(length(list.dirs(dir2))==0) dir.create(dir2)
setwd(dir2)

# if you dont specify discards separately, you will have to give the landings here
landhack <- TRUE

landings<-data.frame(1958:2016,c(22978,15086,18283,15372,21552,17349,10599,19126,27030,
27658,30236,44389,31727,31396,38203,26936,19940,15588,9771,7833,7559,10321,13130,17103,
13000,24981,26779,20426,25024,26200,20447,2325,18404,25562,21127,18618,19300,23305,18816,	
20496,18041,18485,17191,15269,7465,11536,12743,9494,6944,7636,5872,5745,8370,11470,21820,
16247,19574,18355,16318))



# end user input

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


lan <- NULL
dis <- NULL
catch <- NULL
ssb<- NULL
fbar <- NULL
recr <- NULL
for(j in 1:n) {
  i <- which(REP[[j]]==' fleet 1 total catches')
  lan[[j]] <- read.table(REPfile[[j]],header=F,skip=i,nrows=nyears[[j]])[,2]
  i <- which(REP[[j]]==' fleet 1 total Discards')
  dis[[j]] <- read.table(REPfile[[j]],header=F,skip=i,nrows=nyears[[j]])[,3]
  catch[[j]] <- dis[[j]]+lan[[j]]
  i <- which(REP[[j]]=='Spawning Stock, Obs Recruits(year+1), Pred Recruits(year+1), standardized residual')
  ssb[[j]] <- read.table(REPfile[[j]],header=F,skip=i+1,nrows=nyears[[j]],fill=T)[,2]
  recr[[j]] <- read.table(REPfile[[j]],header=F,skip=i,nrows=nyears[[j]],fill=T)[,3]
  i <- which(REP[[j]]=='year    unweighted   Nweighted    Bweighted')
  fbar[[j]] <- read.table(REPfile[[j]],header=F,skip=i,nrows=nyears[[j]])[,2]  
}

windows()
  par(mfrow=c(2,2))
  ylim <- c(0,max(unlist(catch)))
  for(j in 1:n) {
    if(j==1)   plot(catch[[j]]~years[[j]],ylim=ylim,type='l',xlab='Year',ylab='Tonnes',main='Catch')    
    lines(catch[[j]]~years[[j]])
    lines(lan[[j]]~years[[j]],lty=3)
  }
#  if(landhack) lines(landings,lty=3)
#  legend('topleft',c('landings'),lty=c(1,3),inset=0.02)
     ylim <- c(0,max(unlist(ssb)))
    for(j in 1:n) {
   if(j==1) plot(ssb[[j]]~years[[j]],ylim=ylim,type='n',xlab='Year',ylab='Tonnes',main='Spawning Stock Biomass')
   lines(ssb[[j]]~years[[j]])
  }
  ylim <- c(0,max(unlist(fbar)))
  for(j in 1:n) {
    if(j==1) plot(fbar[[j]]~years[[j]],ylim=ylim,type='n',xlab='Year',ylab='F',main='Fbar 2-5')
    lines(fbar[[j]]~years[[j]])
  }
  ylim <- c(0,max(unlist(recr)/1000))
  for(j in 1:n) {
    if(j==1) plot(recr[[j]]/1000~years[[j]],ylim=ylim,type='n',xlab='Year',ylab='Millions',main='Recruits age 1')
    lines(recr[[j]]/1000~years[[j]])
  }

savePlot('Retrospective.png','png')
dev.off()
