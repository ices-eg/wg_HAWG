# HG Mar 2012
# R 2.14.0
# Note that some of this code only works for fleet 1 so if you have more than one fleet...

## AE edited 2015
## Additional plotting code added

library(lattice)

######################################## USER INPUT ########################################

# name of the run
#run <- 'Final_2015'

#run <- '2015_assessment'
#run <- '2016_inc_2015_survey'
#run <- '2016_prelim'
run <- '2016_assessment'

# first age 
firstage <- 1

#dir1<-("W:/PERSONAL/Afra/HAWG 2015/Celtic Sea/Final 2015 new nat mor/")
#dir2<-("W:/PERSONAL/Afra/HAWG 2015/Celtic Sea/Final 2015 new nat mor/more plots")

dir1<-("C:/ICES/HAWG2016/Assessment/")
dir2<-(paste0("C:/ICES/HAWG2016/Assessment/plots/",run,"/more plots",sep=""))

if(length(list.dirs(dir2))==0) dir.create(dir2)
setwd(dir2)

####################################################################################

# sort out how many years, ages and indices


# the outputs are in the *.REP file
REPfile <- paste(dir1,run,'.REP',sep='')
REP <- readLines(REPfile)

#input data (need cwaa)
DATfile <- paste(dir1,run,'.DAT',sep='')
DAT <- readLines(DATfile)

#precision estimates
STDfile <- paste(dir1,run,'.STD',sep='')
STD <- readLines(STDfile)
# if the model does not run properly it will not create an STD file
# but there could be an old one there that is not overwritten
# so check time difference between report file and std file:
difftime(file.info(STDfile)$mtime,file.info(REPfile)$mtime)

# Years
i <- which(DAT=='# Number of Years')
nyears <- read.table(DATfile,header=F,skip=i,nrows=1)[,]
i <- which(DAT=='# First Year')
year1 <- read.table(DATfile,header=F,skip=i,nrows=1)[,]
years <- year1:(year1+nyears-1)

# Ages
i <- which(DAT=='# Number of Ages')
nages <- read.table(DATfile,header=F,skip=i,nrows=1)[,]
ages <- firstage:(nages-1+firstage)

# indices

##Check headings on DAT file if this doesnt work


i <- which(DAT=='# Number of Available Survey Indices')
nind <- read.table(DATfile,header=F,skip=i,nrows=1)[,]
i <- which(DAT=='# Survey Names')
namesind0 <- read.table(DATfile,header=F,skip=i,nrows=nind,comment.char='@')
namesind <- substring(as.character(namesind0$V1),3,255)
i <- which(DAT=='# Use Index (Yes=1)')
useind <- as.numeric(read.table(DATfile,header=F,skip=i,nrows=1))

indices <- NULL
for (j in 1:nind){
  if(useind[j]>0){
    i <- which(DAT==paste('# Index-',j,' Data ',sep=''))
    ind <- read.table(DATfile,header=F,skip=i,nrows=nyears)
    indy <- ind$V1[ind$V2>0]
    indices[[cumsum(useind>0)[j] ]] <- indy
  }
}

names(indices) <- namesind[useind>0]

# define bubble plot function
bubbles <- function(x,z,cex,...){
  maxz <- max(abs(z),na.rm=T)
  panel.fun <- function(x,z,subscripts,cex,...){
    pt.cex <- sqrt(abs(z)/maxz)*cex
    pt.bg <- ifelse(z<0, '#00000000','#00000050')
    lpoints(x,cex=pt.cex[subscripts],pch=21,fill=pt.bg[subscripts],col=1,...)
  }
  text <- as.character(round(seq(maxz,-maxz,length=6),2))

       key = list(space = "right",
                     text = list(text),
                     points = list(pch = c(21), 
                       cex=sqrt(abs(seq(cex,-cex,length=6)))^2, 
                       fill = rep(c('#00000050','#00000000'),each=3)),
                       rep = FALSE)
  
  
  xyplot(x,z=z,cex=cex,panel=panel.fun,key=key,...)
}

### Select the data for plots

j <- which(DAT=='# Number of Years')
nyears<- read.table(DATfile,header=F,skip=j,nrows=1)
nyears<-nyears[,1]
i <- which(DAT=='# First Year')
year1 <- read.table(DATfile,header=F,skip=i,nrows=1)
lastyear<-year1+nyears-1
years<-year1[,1]:lastyear[,1]
i <- which(REP==' fleet 1 total catches')
lan <- read.table(REPfile,header=F,skip=i,nrows=nyears)[,2]
i <- which(REP=='Spawning Stock, Obs Recruits(year+1), Pred Recruits(year+1), standardized residual')
ssb <- read.table(REPfile,header=F,skip=i+1,nrows=nyears,fill=T)[,2]
recr <- read.table(REPfile,header=F,skip=i,nrows=nyears,fill=T)[,3]
i <- which(REP=='year    unweighted   Nweighted    Bweighted')
fbar <- read.table(REPfile,header=F,skip=i,nrows=nyears)[,2]


i <- grep('SSB',STD)
ssbSTD <- read.table(STDfile,header=F,skip=min(i)-1,nrows=nyears)
i <- grep('recruits',STD)
recrSTD <- read.table(STDfile,header=F,skip=min(i)-1,nrows=nyears)
i <- grep('Freport',STD)
fbarSTD <- read.table(STDfile,header=F,skip=min(i)-1,nrows=nyears)


################################################################################################



### Basic stock summary plot

windows()
par(mfrow=c(2,2))
ylim <- c(0,max(lan/1000))
xlim <-c(min(years),max(years))
plot(lan/1000~years,ylim=ylim,xlim=xlim,type='l',xlab='Year',ylab='Kt',main='Catches')

ylim <- c(0,max(ssb/1000))
plot(NA,xlim=xlim,ylim=ylim,type='l',xlab='Year',ylab='Kt',main='Spawning Stock Biomass')
lines(ssb/1000~years,lwd=1)
                                        
ylim <- c(0,max(fbar))
plot(NA,xlim=xlim,ylim=ylim,type='l',xlab='Year',ylab='F',main='Fbar 2-5')
lines(fbar~years,lwd=1)
                                    
ylim <- c(0,max(recr/1000))
plot(NA,xlim=xlim,ylim=ylim,type='l',xlab='Year',ylab='Millions',main='Recruits age 1')
lines(recr/1000~years,lwd=1)

                    
savePlot('SummaryPlot.png','png')
 dev.off()
                    

##################################################################################################

### Stock Summary Table

i <- which(REP=='Population Numbers at the Start of the Year')
N <- read.table(REPfile,header=F,skip=i,nrows=nyears)
i <- which(DAT=='# Weight Matrix - 3')
W <- read.table(DATfile,header=F,skip=i,nrows=nyears)
tsb <- rowSums(N*W)
write.csv(data.frame(year=years,catch=lan,ssb,tsb,fbar,recr),'SummaryTableASAP.csv',row.names=F)

###################################################################################################


# objective function

obfun <- NULL
i <- which(substring(REP,1,17)=='Catch_Fleet_Total')[1]
obfun <- rbind(obfun,read.table(REPfile,header=F,skip=i-1,nrows=1))
i <- which(substring(REP,1,15)=='Index_Fit_Total')[1]
obfun <- rbind(obfun,read.table(REPfile,header=F,skip=i-1,nrows=1))
i <- which(substring(REP,1,15)=='Catch_Age_Comps')[1]
obfun <- rbind(obfun,read.table(REPfile,header=F,skip=i-1,nrows=1))
i <- which(substring(REP,1,15)=='Index_Age_Comps')[1]
obfun <- rbind(obfun,read.table(REPfile,header=F,skip=i-1,nrows=1))
i <- which(substring(REP,1,16)=='Sel_Params_Total')[1]
obfun <- rbind(obfun,read.table(REPfile,header=F,skip=i-1,nrows=1))
i <- which(substring(REP,1,22)=='Index_Sel_Params_Total')[1]
obfun <- rbind(obfun,read.table(REPfile,header=F,skip=i-1,nrows=1))
i <- which(substring(REP,1,13)=='q_year1_Total')[1]
obfun <- rbind(obfun,read.table(REPfile,header=F,skip=i-1,nrows=1))
i <- which(substring(REP,1,12)=='q_devs_Total')[1]
obfun <- rbind(obfun,read.table(REPfile,header=F,skip=i-1,nrows=1))
i <- which(substring(REP,1,23)=='Fmult_year1_fleet_Total')[1]
obfun <- rbind(obfun,read.table(REPfile,header=F,skip=i-1,nrows=1))
i <- which(substring(REP,1,22)=='Fmult_devs_fleet_Total')[1]
obfun <- rbind(obfun,read.table(REPfile,header=F,skip=i-1,nrows=1))
names(obfun) <- c('component','lamda','value')


windows()
  par(las=1,mar=c(5,12,4,2))
  b <- barplot(obfun$value,names=obfun$component,horiz=T,xlab='Value',main='Objective function')
  text(obfun$value,b,ifelse(obfun$value==0,0,''),pos=4)
  savePlot('ObjectiveFunction.png','png')
dev.off()

# catch and discards effective sample size
i <- which(REP==' Input and Estimated effective sample sizes for fleet 1')
ess1 <- read.table(REPfile,header=F,skip=i,nrows=nyears)
names(ess1) <- c('Year','Input','Estimated')
i <- which(REP==' Input and Estimated effective Discard sample sizes for fleet 1')
ess2 <- read.table(REPfile,header=F,skip=i,nrows=nyears)
names(ess2) <- c('Year','Input','Estimated')
ees <- rbind(data.frame(year=ess1$Year,type1='Landings',type2='Input',value=ess1$Input)
             ,data.frame(year=ess1$Year,type1='Landings',type2='Estimated',value=ess1$Estimated)
             )
ees$value <- ifelse(ees$value>1e+10,NA,ees$value)


windows()
  xyplot(value~year|type1,groups=type2,data=ees,type='b',auto.key=T,xlab='Year',ylab='Effective sample size (log scale)',scales=list(alternating=1,y=list(log=T)),main='Fleet 1 effective sample size')
  savePlot('Fleet_EES.png','png')
dev.off()

# observed and predicted landings
i <- which(REP=='Observed and predicted total fleet catch by year and standardized residual')
lan <- read.table(REPfile,header=F,skip=i+1,nrows=nyears)
names(lan) <- c('Year','Observed','Predicted','Residual')
catch <- rbind(data.frame(year=lan$Year,type1='Landings',type2='Observed',value=lan$Observed)
             ,data.frame(year=lan$Year,type1='Landings',type2='Predicted',value=lan$Predicted)
             )


# for the case where discards included in the landings
windows(5,5)
  xyplot(value~year,groups=type2,data=subset(catch,type1=='Landings'),type='b',auto.key=T,xlab='Tonnes',scales=list(alternating=1),main='Observed and predicted catch')
  savePlot('Fleet_Catch.png','png')
dev.off()


## Check the number of rows of data in the index number section of the REP file
## edited j +5 from original to j+6
# index fit

n <- length(indices) 
ind <- NULL
for(i in 1:n){
  j <- which(REP==paste('index number',i))
  ind0 <- read.table(REPfile,header=F,skip=j+6,nrows=length(indices[[i]]))
  names(ind0) <- c('year','obsindex','predindex','standres')
  ind <- rbind(ind,data.frame(index=i,ind0))
}


ind1 <- rbind(
  data.frame(type='observed',index=ind$index,year=ind$year,value=ind$obsindex),
  data.frame(type='predicted',index=ind$index,year=ind$year,value=ind$predindex)
  )


windows(7,5)
  xyplot(value~year|index,groups=type,data=ind1,type='b',xlab='Year',ylab='Index',scales=list(alternating=1,y=list(relation='free')),auto.key=T,main='Index fit')
  savePlot('IndexFit.png','png')
dev.off()


## Again edit the number of rows to match the data in the rep file
# index proportions-at-age
ind <- NULL

for(i in 1:n){
  j <- which(REP==paste(' Index number',i))
  ind0 <- read.table(REPfile,header=F,skip=j,nrows=10*length(indices[[i]])+4)[,-c(1,4)]
  for(k in 3:ncol(ind0)) ind0[,k] <- ifelse(ind0[,k]==-1,NA,ind0[,k])
  names(ind0) <- c('year','type',paste('age',ages))
  ind <- rbind(ind,data.frame(index=i,ind0))
}

# index proportions-at-age

res <- subset(ind,type=='Obs')[,-(1:3)]-subset(ind,type=='Pred')[,-(1:3)]
st <- stack(res)
st1 <- stack(subset(ind,type=='Obs')[,-(1:3)])
st2 <- stack(subset(ind,type=='Pred')[,-(1:3)])
index <- names(indices)[subset(ind,type=='Obs')$index]
res1 <- data.frame(index,year=subset(ind,type=='Obs')$year,obs=st1$values,pred=st2$values,residuals=st$values,age=as.numeric(substring(st$ind,5,7)))

## Edit the years you want to plot

res1<-subset(res1,year %in% seq(2002,2014))
res1 <- subset(res1,!is.na(residuals))



res1a <- aggregate(list(meanp=res1$obs),list(index=res1$index,age=res1$age),mean)

#res1b<-subset(res1, year==2002:2014)
res1b<-subset(res1,year %in% seq(2002,2014))
res1b <- subset(res1b,!is.na(residuals))

i <- match(paste(res1$index,res1$age),paste(res1a$index,res1a$age))
res1$meanp <- res1a$meanp[i]


i <- match(paste(res1b$index,res1b$age),paste(res1a$index,res1a$age))
res1b$meanp <- res1a$meanp[i]


res2 <- rbind(data.frame(type='observed',res1[,c(1,2)],value=res1b$obs,age=res1b$age),
              data.frame(type='predicted',res1[,c(1,2)],value=res1b$pred,age=res1b$age))
res2<-subset(res2,age==1:9)

windows()
  bubbles(age~year|index,data=res1b,z=res1b$residuals,cex=5,xlab='Year',ylab='Age',main='Index proportions-at-age residuals',scales=list(alternating=1))
  savePlot('IndexResidualsAge.png','png')
  bubbles(age~year|index,data=res1b,z=res1b$residuals/res1b$meanp,cex=7,xlab='Year',ylab='Age',main='Standardised proportions-at-age residuals',scales=list(alternating=1))
  savePlot('IndexStResidualsAge.png','png')
dev.off()


# index selectivity-at-age
i <- which(REP=='Index Selectivity at Age')
ind <- read.table(REPfile,header=F,skip=i,nrows=n)
sel <- data.frame(age=rep(ages,each=n),index=1:n,sel=stack(ind)$values)
sel$sel <- ifelse(sel$sel<0,NA,sel$sel)

windows()
xyplot(sel~age|names(indices)[index],data=sel,type='b',xlab='Year',ylab='Selectivity',scales=list(alternating=1),main='Selectivity at age',ylim=c(-.04,1.04))
savePlot('IndexSelectivity.png','png')
dev.off()

# Selectivity at age in catches
# only works for fleet1
i <- which(REP=='Selectivity by age and year for each fleet')
sagetab <- read.table(REPfile,header=F,skip=i+1,nrows=nyears,fill=T)
colnames(sagetab) <- ages
rownames(sagetab) <- years
mat <- as.matrix(sagetab)

sage<- stack(sagetab)
names(sage) <- c('s','age')
sage$year <- years
sage$age <- as.numeric(as.character(sage$age))


windows()
xyplot(s~age,groups=year,data=sage[order(sage$age),],xlab='Age',ylab='Selectivity',type='b',main='Fleet selectivty at age')
savePlot('Fleet_S_1.png','png')
dev.off()

# F mult and F at age
i <- which(REP=='Fmult by year for each fleet')
fmult <- read.table(REPfile,header=F,skip=i,nrows=nyears)
i <- which(REP=='Directed F by age and year for each fleet')
fdiry1 <- t(read.table(REPfile,header=F,skip=i+1,nrows=1))
i <- which(REP=='Total F')
ftoty1 <- t(read.table(REPfile,header=F,skip=i,nrows=1))
f <- rbind(data.frame(x=years,type1='F multiplier',type2='Total F',value=fmult$V2)
           ,data.frame(x=ages,type1='F at age',type2='Directed F',value=fdiry1/fmult$V2[1])
           ,data.frame(x=ages,type1='F at age',type2='Total F',value=ftoty1/fmult$V2[1])
           )


windows(7,5,10)
  xyplot(value~x|type1,groups=type2,type='b',data=f,scales='free',xlab='Year / Age',ylab='Value',main='F-multiplier and F-at-age',auto.key=T)
savePlot('F.png','png')
dev.off()

# catch-at-age proportions-at-age
# only works for first fleet
i <- which(REP=='Proportions of catch at age by fleet')
lpaa <- read.table(REPfile,header=F,skip=i+1,nrows=2*nyears)[,-c(1,4)]
names(lpaa) <- c('year','type',paste('age',ages))
i <- which(REP=='Proportions of Discards at age by fleet')
dpaa <- read.table(REPfile,header=F,skip=i+1,nrows=2*nyears)[,-c(1,4)]
names(dpaa) <- c('year','type',paste('age',ages))

i <- which(REP=='Observed and predicted total fleet catch by year and standardized residual')
lan <- read.table(REPfile,header=F,skip=i+1,nrows=nyears)
names(lan) <- c('Year','Observed','Predicted','Residual')
i <- which(REP=='Observed and predicted total fleet Discards by year and standardized residual')
dis <- read.table(REPfile,header=F,skip=i+1,nrows=nyears)
names(dis) <- c('Year','Observed','Predicted','Residual')

i <- which(DAT=='# Weight Matrix - 1')
cwaa <- read.table(DATfile,header=F,skip=i+1,nrows=nyears)

p <- subset(lpaa,type=='Pred')[,-(1:2)]
r <- lan$Predicted/rowSums(p*cwaa)
lnaa <- apply(p,2,function(x) x*r)

p <- subset(dpaa,type=='Pred')[,-(1:2)]
r <- lan$Predicted/rowSums(p*cwaa)
dnaa <- apply(p,2,function(x) x*r)
dnaa[,c(1,5:9)] <- 0

#released <- dnaa/(lnaa+dnaa)
#write.csv(released,'released.csv',row.names=F)

res <- subset(lpaa,type=='Obs')[,-(1:2)]-subset(lpaa,type=='Pred')[,-(1:2)]
st <- stack(res)
st1 <- stack(subset(lpaa,type=='Obs')[,-(1:2)])
st2 <- stack(subset(lpaa,type=='Pred')[,-(1:2)])
res1 <- data.frame(year=years,obs=st1$values,pred=st2$values,residuals=st$values,age=as.numeric(substring(st$ind,5,7)))
res1 <- subset(res1,!is.na(residuals) & obs>0)
res1a <- aggregate(list(meanp=res1$obs),list(age=res1$age),mean)
i <- match(res1$age,res1a$age)
res1$meanp <- res1a$meanp[i]

windows()
  bubbles(age~year,data=res1,z=res1$residuals,cex=8,xlab='Year',ylab='Age',main='Catch proportions-at-age residuals')
  savePlot('Fleet_CaaRes.png','png')
  bubbles(age~year,data=res1,z=res1$residuals/res1$meanp,cex=8,xlab='Year',ylab='Age',main='Standardized Catch proportions-at-age residuals')
  savePlot('Fleet_CaaStRes.png','png')
dev.off()



st <- stack(lpaa[,-c(1,2)])
st$type <-  lpaa$type
st$year <- years[lpaa$year]
st$age <- as.numeric(substring(st$ind,5,7))


windows()
  xyplot(log(values)~age|factor(year-age),groups=type,data=st,type='l',auto.key=T,xlab='Age',ylab='Log proportion',main='Catch proportions-at-age',as.table=T)
#  savePlot('Fleet_LogCaa.png','png')
  xyplot(values~year|factor(age),groups=type,data=st,type='l',auto.key=T,xlab='Year',ylab='Proportion',main='Catch proportions-at-age',as.table=T,scales=list(y='free'))
  savePlot('Fleet_Caa.png','png')
dev.off()


### popnos
i <- which(REP=="Population Numbers at the Start of the Year")
snaa <- read.table(REPfile,header=F,skip=i,nrows=nyears)
names(snaa) <- ages
rownames(snaa) <- years
snaa.df <- stack(snaa)
snaa.df$year <- years
snaa.df$cohort <- snaa.df$year - as.numeric(as.character(snaa.df$ind))
key <- simpleKey(as.character(ages),space='right',title='age',cex.title=1)
key$points$pch <- 1:length(ages)
xyplot(log(values)~year,groups=factor(ind),data=snaa.df,type='b',key=key,pch=1:length(ages),scales=list(alternating=1),xlab='Year',ylab='Log stock numbers at age',cex=0.6)

windows()
  xyplot(log(values)~cohort,groups=factor(ind),data=snaa.df,type='b',key=key,pch=1:length(ages),scales=list(alternating=1),xlab='Cohort',ylab='Log stock numbers at age',cex=0.6)
  savePlot('StockNos.png','png')
dev.off()



# index q
ind <- NULL
for(i in 1:n){
  j <- which(REP==paste(' index',i,'q over time'))
  ind0 <- read.table(REPfile,header=F,skip=j,nrows=length(indices[[i]]))
  names(ind0) <- c('year','q')
  ind <- rbind(ind,data.frame(index=names(indices)[i],ind0))
}

windows(7,5)
  xyplot(q~year|index,data=ind,type='b',scales=list(alternating=1),main='Index catchability')
  savePlot('IndexQ.png','png')
dev.off()



###rmse

i <- which(REP=='Root Mean Square Error computed from Standardized Residuals')
j <- which(substring(REP,1,10)=='Projection')[1]
rmse <- read.table(REPfile,header=F,skip=i+2,nrows=j-i-20)

windows()
  plot(V3~V2,data=rmse,xlab='Number of residuals',ylab='RMSE')
  text(rmse$V2,rmse$V3,labels=rmse$V1,pos=3)
  abline(h=1)
  lines(c(1,3,5,10,20,30,40,50,100),c(.063,.348,.473,.634,.737,.786,.815,.832,.883),lty=3)
  lines(c(1,3,5,10,20,30,40,50,100),c(1.960,1.619,1.487,1.351,1.253,1.211,1.183,1.162,1.116),lty=3)
  savePlot('RMSE.png','png')
dev.off()


# save F and N for STF as csv
i <- which(REP=='Total F')
Ftab <- read.table(REPfile,header=F,skip=i,nrows=nyears)
colnames(Ftab) <- paste('Age',ages)
write.csv(round(Ftab,3),'F.csv',row.names=F)

i <- which(REP=='Population Numbers at the Start of the Year')
Ntab <- read.table(REPfile,header=F,skip=i,nrows=nyears)
colnames(Ntab) <- paste('Age',ages)

i <- which(DAT=='# Natural Mortality')
Mtab <- read.table(DATfile,header=F,skip=i,nrows=nyears)
x <- exp(-(Ftab+Mtab)[nyears,])*Ntab[nyears,]
x1 <- c(0,unlist(x[1:(nages-1)]))
x1[nages] <- x1[nages]+x[nages]
names(x1) <- paste('Age',ages)
write.csv(round(rbind(Ntab,x1),0),'N.csv',row.names=F)




# Uncertainty Plots for key parameters
### plot stock summary with +1 and -1 standard deviations

i <- which(REP==' fleet 1 total catches')
lan <- read.table(REPfile,header=F,skip=i,nrows=nyears)[,2]

windows()
par(mfrow=c(2,2))
ylim <- c(0,max(lan/1000))
xlim <-c(min(years),max(years))
#xlim <-c(min(years),intyear$year)  if you wnat intermediate year included
plot(lan/1000~years,ylim=ylim,xlim=xlim,type='l',xlab='Year',ylab='Kt',main='landings')
legend('topright',c('landings'),lty=c(1,1,2),lwd=c(2,1,1),bty='n')


ylim <- c(0,max(ssb+ssbSTD$V4))/1000
plot(NA,xlim=xlim,ylim=ylim,type='l',xlab='Year',ylab='Kt',main='Spawning Stock Biomass',col='grey')
polygon(c(years,rev(years)),c((ssbSTD$V3-ssbSTD$V4),rev((ssbSTD$V3+ssbSTD$V4)))/1000,border=0,col='grey') 
lines(ssb/1000~years,lwd=2)
#points(ssb/1000~years)
legend('topright',c('ASAP','StDev')
       ,col=c('black','grey','black')
       ,lty=c(1,NA,NA),lwd=c(2,NA,NA)
       ,pch=c(NA,15,1),pt.cex=c(NA,2,1)
       ,bty='n',inset=c(0.02,0))


ylim <- c(0,max(fbar+fbarSTD$V4))
plot(NA,xlim=xlim,ylim=ylim,type='l',xlab='Year',ylab='F',main='Fbar 2-5',col='grey')
polygon(c(years,rev(years)),c((fbarSTD$V3-fbarSTD$V4),rev((fbarSTD$V3+fbarSTD$V4))),border=0,col='grey') 
lines(fbar~years,lwd=2)
#points(fbar~yearr)



ylim <- c(0,max(recr+recrSTD$V4)/1000)
plot(NA,xlim=xlim,ylim=ylim,type='l',xlab='Year',ylab='Millions',main='Recruits age 1',col='grey')
polygon(c(years,rev(years)),c((recrSTD$V3-recrSTD$V4)/1000,rev((recrSTD$V3+recrSTD$V4)/1000)),border=0,col='grey') 
lines(recr/1000~years,lwd=2)
#points(recr/1000~year)

savePlot('SummaryPlot_SD.png','png')
dev.off()



## PLOT cvs For ssb, rec and mean f

windows()
ylim <- c(0,0.6)
plot(NA,xlim=xlim,ylim=ylim,type='l',xlab='Year',ylab='CV',main='Spawning Stock Biomass')
lines((ssbSTD$V4/ssbSTD$V3)~years,lwd=2)
savePlot('CV_SSB.png','png')
dev.off()


windows()
ylim <- c(0,0.6)
plot(NA,xlim=xlim,ylim=ylim,type='l',xlab='Year',ylab='CV',main='Recruitment')
lines((recrSTD$V4/recrSTD$V3)~years,lwd=2)
savePlot('CV_RECR.png','png')
dev.off()

windows()
ylim <- c(0,0.5)
plot(NA,xlim=xlim,ylim=ylim,type='l',xlab='Year',ylab='CV',main='Mean F')
lines((fbarSTD$V4/fbarSTD$V3)~years,lwd=2)
savePlot('CV_MEAN_F.png','png')
dev.off()


windows()
## Cvs plotted together
ylim <- c(0,0.6)
plot(NA,xlim=xlim,ylim=ylim,type='l',xlab='Year',ylab='CV',main='Uncertainty of key parameters')
lines((ssbSTD$V4/ssbSTD$V3)~years,lwd=2, col='red')
lines((recrSTD$V4/recrSTD$V3)~years,lwd=2, col='blue')
lines((fbarSTD$V4/fbarSTD$V3)~years,lwd=2, col='green')
legend(1970,0.5,c('SSB','Rec','Fbar'),lty=c(1,1,1),lwd=2,pt.cex=1.5,col=c('red','blue','green'))

savePlot('uncertainties.png','png')
dev.off()