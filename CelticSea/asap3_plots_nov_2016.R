####################################################################
# asap3_plots                                                      #
# generate a number of plots automatically from asap3.rdat file    #
# uses functions for each plot so easy to customize and turn off/on #
# Liz Brooks and Chris Legault                                     #
# created 8 August 2012                                            #
# last update:   
# 9/25/2012: increased length of color vector to 200               #
# 9/28/2012: fixed data plot of indices when only 1 index used     #
# 10/09/2012: fixed "francis" plot saved name for indices           #
#            fixed calculation of 3 biomasses compared to use same WAA #
# 10/10/2012: fixed index selectivity--only plots ages index applies to #
# 10/14/2012: fixed q plot so that it is saved as .png            #
# 10/25/2012: fixed mcmc output.csv for Fmult                     #
# 2/20/2013: updated mcmc thin/burn/output of .BSN file           #
# 3/19/2013: added correlations for cohort tracking and catch curve results #
# 12/18/2013: cleaned up 'warning' messages from write.csv statements #
#             and added a flag for retro file naming conventions   # 
#                                                                  #
#  ----> Required Libraries: plotrix, tseries, Hmisc               #
#                                                                  #
####################################################################

rm(list=ls(all=T)) # clear session memory
graphics.off()     # close any open windows

####----BEGIN USER SPECIFICATIONS ----------------------------------


# 1. Specify Working directory (make sure you have subdirectory "plots"

# 1. Specify Working directory (make sure you have subdirectory "plots"
#wd <-"C:\\NFT\\ASAPV30\\CSH\\CSH1\\"    # working input files
#wd<-"W:\\PERSONAL\\Afra\\HAWG 2015\\Celtic Sea\\Final 2015 new nat mor\\"

#wd<-"L:\\PERSONAL\\Afra\\ASAP\\input files for exploratory runs Nov 2016\\dummy data\\"
#wd<-"L:\\PERSONAL\\Afra\\ASAP\\2017 HAWG\\"
wd<-"C:\\Users\\momalley\\Desktop\\HAWG 2017\\2017 HAWG FINAL\\"
od <- paste(wd,"plots\\", sep="")




# 2. Specify name of .RDAT file  
#     (for a model with retrospective analysis, enter the file name with or without _000)
#rdat <- "CSH2.RDAT"

rdat<-"2017 ASSESSMENT FINAL.RDAT"

# 3. specify first age to use when plotting catch curves (typically the "peak" age that is fully selected)
   # value -999 finds the peak age to plot; user can specify a positive integer to override
#cc.age <- 3  
cc.age <- -999  

# 4. for Reference Points: num of years to average for calculating YPR-SPR
nyrs.ave = 5   

# 5. for Correlation plot: set value to find parameter combos where |correlation| > value
correlation.limit <- 0.9  

# 6. for Bubble plots
scale.catch.bubble2 <- 2  # increase this to make bubbles larger, decrease to make smaller bubbes
scale.index.bubble2 <- 2  # increase this to make bubbles larger, decrease to make smaller bubbes

# 7. for MCMC plots
burn = 0  # increase this if the beginning of chain has patterns (default=0)
thin = 1  # increase this if the correlations are too high (default=1)

# 8. Save Options
save.plots <- T
make.one.pdf <- T  #if T, diagnostics/results/ref pts/MCMC/data are all saved as one pdf
plotf = 'png'



####----END USER SPECIFICATIONS ------------------------------------

setwd(wd)
npar <- -999
max.grad <- -999

#--- test for required libraries
kk1 <- library(Hmisc, logical.return=T)
kk2 <- library(tseries, logical.return=T)
kk3 <- library(plotrix, logical.return=T)

if (kk1==F) install.packages("Hmisc" )
if (kk2==F) install.packages("tseries" )
if (kk3==F) install.packages("plotrix" )

library(Hmisc)
library(tseries)
library(plotrix)
#--- end test for required libraries


asap.name <- substr(rdat,1,(nchar(rdat)-5))
ss1<-shell(paste("dir ", rdat, sep=""), intern=T )
ss2 <- which(ss1=="File Not Found")
retro.flag <- F
if (length(ss2)>0 )  {
    rdat <- paste(asap.name, ".rdat", sep="")
    asap.rts <- paste(asap.name, ".rts", sep="")
    retro.flag <- T
    asap.name <- substr(rdat,1,(nchar(rdat)-5)) 
    }

ss3<- substr(asap.name, (nchar(asap.name)-3), nchar(asap.name))
if (ss3=="_000")  {
   retro.flag <- T
   asap.rts <- paste(substr(asap.name, 1, (nchar(asap.name)-4) ),".rts", sep="")
      }
      
ss5<-shell(paste("dir plots",  sep=""), intern=T )
ss6 <- which(ss5=="File Not Found")
if (length(ss6)>0 )  shell(paste("mkdir ",  od, sep=""), intern=T )                                           

plot.colors=F
pdf.text <- NULL
pos.resid.col <- "#ffffffaa"
neg.resid.col <- "#8c8c8caa"



setwd(wd)
asap <- dget(rdat)


liz.palette = c( "black"  ,  "purple3" ,  "blue"  ,  "turquoise2"  ,
                "red2" ,   "orange" ,  "#994411",   "#770000"    ,
                "#335500"  ,  "springgreen3" , "green1" ,  "gold3",
                "#333388" ,      "orchid"   ,     "mediumpurple1"      , "gray60"  , 
                "deeppink4"  ,    "violetred2"  ,     "#9900AA"    , "#8888EE",
                "yellow1"   ,     "yellowgreen"  ,  "#778800" ,      "#FFBB11"  ,  
                "#CC5588"  ,"#11BB77"   , "#11AADD"   ,   "#335522"   ,  
                "#BB1133"   ,     "#4400AA",  "#FF7755"   ,  "#77AACC"   , 
                "#FF00BB" ,  "grey50"   ,  "#FF2233" , "#99BB77"  ,  
                "grey35"   ,    "#CCDD00" ,    "#AA77FF"   ,  "#88BB88"    )
  
liz.palette <- rep(liz.palette, 5)

# check color visibility of palette
if (plot.colors==T) {
barplot( rep(2,40),seq(1,40),  beside=T,width=1, space=0,col=liz.palette, horiz=F, ylim=c(0,3)  )
abline(v=seq(0,40,by=4) )
text(x=seq(2,40,4), y=rep(2.25,10), label=seq(1,10) )
  if (save.plots) savePlot(paste(od, "Color.palette.barplot.",plotf, sep=""), type=plotf)
             }
#------------------------------------
#get fleet and index names from dat file

grab.names <- function(asap.name,asap){
  my.names <- list()
  # in case the file was created outside the GUI
  my.names$fleet.names <- paste("FLEET-",1:asap$parms$nfleets, sep="")
  my.names$index.names <- paste("INDEX-",1:asap$parms$nindices, sep="")
gg1<-shell(paste("dir ", asap.name,".dat", sep=""), intern=T, mustWork=NA )
gg2 <- which(gg1=="File Not Found")
if (length(gg2)==0 )  {
  datfile <- readLines(con = paste(asap.name,".dat", sep=""))
  nlines <- length(datfile)
  nfinis <- nlines-asap$parms$nfleets-asap$parms$navailindices-3
  if (datfile[nfinis] == "###### FINIS ######"){
     my.names$fleet.names <- substr(datfile[(nfinis+2):(nfinis+2+asap$parms$nfleets-1)],3,100)
     avail.index.names <- substr(datfile[(nfinis+3+asap$parms$nfleets):(nlines-1)],3,100)
     my.names$index.names <- avail.index.names[asap$initial.guesses$index.use.flag==1]
         }  # end if-test for nfinis
  } # end if-test for length(gg2)
  
  return(my.names)
}




#------------------------------------
#Read-in values from the *.std, *.par, and *.cor files

grab.aux.files <- function(asap.name,asap) {
#if (retro.flag==T) asap.name="asap3"
asap.std <- read.table(paste(asap.name, ".std",sep=""), header = F, skip=1,sep = "") # Read in std file from admb
names(asap.std) <- c("index", "name", "value", "stdev" )

years <- seq(asap$parms$styr, asap$parms$endyr)

ncol.cor <- dim(asap.std) [1]
asap.cor <- read.table(paste(asap.name, ".cor",sep=""), header = F, skip=2,sep = "", 
    col.names=c("index", "name", "value", "stdev", seq(1,ncol.cor)), fill=T) # Read in std file from admb
asap.cor.mat <- as.matrix(asap.cor[,5:length(asap.cor[1,])])
asap.cor.names <- as.character(as.vector(asap.cor[,2]) )
levels.cor.names <- unique(asap.cor.names)
F.rep <- which(asap.cor.names=="Freport") 
asap.cor.names[F.rep] <- paste(asap.cor.names[F.rep],years, sep=".")  
Tot.B <- which(asap.cor.names=="TotJan1B") 
asap.cor.names[Tot.B] <- paste(asap.cor.names[Tot.B],years, sep=".")
SSB <- which(asap.cor.names=="SSB")  
asap.cor.names[SSB] <- paste(asap.cor.names[SSB],years, sep=".")
Expl.B <- which(asap.cor.names=="ExploitableB")  
asap.cor.names[Expl.B] <- paste(asap.cor.names[Expl.B],years, sep=".")
recr <- which(asap.cor.names=="recruits") 
asap.cor.names[recr] <- paste(asap.cor.names[recr],years, sep=".")

N.yr1 <- which(asap.cor.names=="log_N_year1_devs")   
asap.cor.names[N.yr1] <- paste(asap.cor.names[N.yr1],"Age",seq(2,asap$parms$nages), sep=".")

recr.devs <- which(asap.cor.names=="log_recruit_devs")   
asap.cor.names[recr.devs] <- paste(asap.cor.names[recr.devs],years, sep=".")
Fmult.yr1 <- which(asap.cor.names=="log_Fmult_year1")   
asap.cor.names[Fmult.yr1] <- paste(asap.cor.names[Fmult.yr1],fleet.names, sep=".")
Fmult.devs <- which(asap.cor.names=="log_Fmult_devs")   
asap.cor.names[Fmult.devs] <- paste(asap.cor.names[Fmult.devs],paste(fleet.names, rep(years[2:length(years)], each=asap$parms$nfleets),  sep="."), sep=".")
q.yr1 <- which(asap.cor.names=="log_q_year1")   
asap.cor.names[q.yr1] <- paste(asap.cor.names[q.yr1],index.names, sep=".")

q.devs <- which(asap.cor.names=="log_q_devs")   
#asap.cor.names[q.devs] <- paste(asap.cor.names[q.devs],index.names, sep=".")

if (length(q.devs)>0) asap.cor.names[q.devs] <- paste(asap.cor.names[q.devs],years, sep=".")

diag(asap.cor.mat) <- rep(NA, ncol.cor)

asap.grad <- readLines(paste(asap.name,".par", sep=""),n=1) 
par.split<- unlist(strsplit(asap.grad, " "))
max.grad <- par.split[length(par.split)]
npar <- par.split[ 6]
aux.list <-  list("npar"=npar, "asap.cor.names"=asap.cor.names, "asap.cor.mat"=asap.cor.mat,
    "asap.std"=asap.std, "max.grad"=max.grad, "F.rep"=F.rep[1] , "Tot.B"=Tot.B[1] , "SSB"=SSB[1] , 
     "Expl.B"=Expl.B[1] , "recr"=recr[1] )

#return(npar, asap.cor.mat)
return(aux.list)
     }
     
#------------------------------------
#Identify parameters with high correlation
plot.high.corr <- function(asap.cor.mat,correlation.limit) {
par(mfrow=c(1,1) )

  npar <- a1$npar
  ncor.pars <- dim(asap.cor.mat) [1]
  count.high.corr <- rep(0, ncor.pars)
  text.col <- ifelse(abs(asap.cor.mat)>correlation.limit, "red", "white")
  #plot(seq(1,ncor.pars), seq(1,ncor.pars), type='n', xlab="Parameter", ylab="Parameter")
  tcor<-0
  for (i in 1:ncor.pars) {
      for (j in 1:ncor.pars) {
      #points(i,j, pch='.', col=text.col[i,j], cex=2)

      if (text.col[i,j]=="red" && is.na(text.col[i,j])==F) tcor<- tcor+1
      if (text.col[i,j]=="red" && is.na(text.col[i,j])==F) count.high.corr[i]<- count.high.corr[i]+1      
              } # end j-loop
           } # end i-loop
  
plot(seq(1,npar), count.high.corr[1:npar], type='l', xlab="Parameter", lwd=1, col='black',
      ylab="Number of high correlations", ylim=c(0,1.1*max(count.high.corr) )  )
#abline(v=a1$F.rep, col="green3")
#text(x=a1$F.rep, y=1.1*max(count.high.corr), label="F.report", pos=4, cex=0.70)
#abline(v=a1$Tot.B, col="maroon")
#text(x=a1$Tot.B, y=1.1*max(count.high.corr), label="Tot.B", pos=4, cex=0.70)
#abline(v=a1$SSB, col="blue")
#text(x=a1$SSB, y=1.1*max(count.high.corr), label="SSB", pos=4, cex=0.70)
#abline(v=a1$Expl.B, col="green2")
#text(x=a1$Expl.B, y=1.1*max(count.high.corr), label="Expl.B", pos=4, cex=0.70)
#abline(v=a1$recr, col="red")
#text(x=a1$recr, y=1.1*max(count.high.corr), label="Recr", pos=4, cex=0.70)

high.rows <- matrix(NA, 1, 2)
cor.vals <- 0
 for (irow in 1:ncor.pars) {
temp.row <- as.vector(which( abs(asap.cor.mat[irow,])>correlation.limit))

if(length(temp.row)>0) {
  cor.pairs <- cbind(rep(irow, length(temp.row)), temp.row)
  high.rows<- rbind(high.rows, cor.pairs)
  cor.vals <-c(cor.vals, asap.cor.mat[irow, temp.row] )
     }
  } #end loop over irow
  high.rows<- high.rows[-1,]
  cor.vals <- cor.vals[-1]
  named.cor.pairs <- as.data.frame(matrix(NA, length(high.rows[,1]),3))
  colnames(named.cor.pairs)=c("Par1", "Par2", "Correlation")
  named.cor.pairs[,1] <-a1$asap.cor.names[high.rows[,1]]
  named.cor.pairs[,2] <-a1$asap.cor.names[high.rows[,2]]  
  named.cor.pairs[,3] <-  cor.vals

write.csv(named.cor.pairs, file=paste(od, "High.Corr.Pars.csv",sep=""),   row.names=F)
  
  
  if (save.plots) savePlot(paste(od, "Corr.Limit.Count.by.par.",plotf, sep=""), type=plotf)


} 
     
     
#------------------------------------
# barplot of main likelihoods by component
windows()
plot.main.likelihoods <- function(asap){
par(mfrow=c(1,1) )


  like2 <- unlist(asap$like[2:length(asap$like)])
  like2.names <- names(asap$like[2:length(asap$like)])
  n.like <- length(like2)
  my.range <- c(0.9*min(like2), 1.2*max(like2) )
  par(mar=c(5,10,1,1), oma=c(1,0,0,0)) 
  barplot(horiz=T, like2, beside=F, col=liz.palette[1:n.like],
         xlab="Likelihood Contribution",  axisnames=F,  axes=F,  space=0,
         xlim=my.range  )
  axis(side=1, at=pretty(seq(my.range[1],my.range[2]), n=10), labels=pretty(seq(my.range[1],my.range[2]), n=10), cex=.75 )
  axis(side=2, at=seq(0.5,(n.like-0.5)), labels= like2.names, las=2)
  text(x= like2, y=seq(0.5,(n.like-0.5)), labels=round(like2,0), cex=0.8, pos=4)
  text(x= my.range[1], y=(n.like+0.25),  cex=0.95, pos=4, col='#110033',
          labels=paste('Maximum gradient = ',round(as.numeric(max.grad),6),sep="") )  
  box()
  title(paste("Components of Obj. Function (", round(as.numeric(asap$like[1]),0), "), npar=", npar, sep=""), cex=0.9 )
  #title( sub=paste("Model: ", asap.name, "     ", date(),sep=""))
  title( sub=paste("Model: ", asap.name, "     ", asap$info$date,sep=""))
  
  if (save.plots) savePlot(paste(od, "Likelihood.Comp.",plotf, sep=""), type=plotf)

}

#------------------------------------
# barplot of additional likelihoods by user specified components
plot.additional.likelihoods <- function(asap,plotvec){
par(mfrow=c(1,1) )


  like2 <- unlist(asap$like.additional[plotvec])
  like2.names <- names(asap$like.additional[plotvec])
  n.like <- length(like2)
#  my.range <- c(0.9*min(like2), 1.2*max(like2) )
  my.range <- c(0, 1.2*max(like2) )
  par(mar=c(5,10,1,1), oma=c(1,0,0,0)) 
  barplot(horiz=T, like2, beside=F, col=rev(liz.palette[1:n.like]),
         xlab="Likelihood Contribution",  axisnames=F,  axes=F,  space=0,
         xlim=my.range  )
  axis(side=1, at=pretty(seq(my.range[1],my.range[2]), n=10), labels=pretty(seq(my.range[1],my.range[2]), n=10), cex=.75 )
  axis(side=2, at=seq(0.5,(n.like-0.5)), labels= like2.names, las=2)
  text(x= like2, y=seq(0.5,(n.like-0.5)), labels=round(like2,0), cex=0.8, pos=4)
  box()
  title("Subcomponents of Obj. Function", cex=0.9 )
  title( sub=paste("Model: ", asap.name, sep=""))
  if (save.plots) savePlot(paste(od, "Likelihood.Comp.Additional.",plotf, sep=""), type=plotf)

}

#------------------------------------
# plot of RMSE table
plot.RMSE.table <- function(asap){
par(mfrow=c(1,1) )
  max.txt<-16
  n.rmse<-length(asap$RMSE)
  par(oma=rep(2,4), mar=c(0,0,1,0) )
  plot(seq(1,15 ), seq(1,15), type='n', axes=F,
    xlab="", ylab="", xlim=c(1,(max.txt+2+ 6+2+ 8+2) ), ylim=c(n.rmse+4, 1) )
  text(rep(1, n.rmse), seq(3,n.rmse+2), labels=substr(names(asap$RMSE),6,100), pos=4 )
  text(rep(max.txt+2, n.rmse), seq(3,n.rmse+2), labels=asap$RMSE.n, pos=4 )
  text(rep(max.txt+2+ 6+2, n.rmse), seq(3,n.rmse+2), labels=signif(as.numeric(asap$RMSE),3), pos=4 )
  text( c(1, max.txt+2, max.txt+2+ 6+2), rep( 2, 3), 
      labels=c("Component","# resids","RMSE"), font=2, pos=4)
  title(main="Root Mean Square Error computed from Standardized Residuals", outer=T, cex=0.85)
  if (save.plots) savePlot(paste(od, "RMSE.Comp.Table.",plotf, sep=""), type=plotf)
}

#------------------------------------
# plot of RMSE values with 95% CI
plot.RMSE.95CI <- function(asap){
par(mfrow=c(2,1), mar=c(4,4,2,2) )#need to specify 2/3 layout

max.txt<-16
ci.vals <- matrix(NA, 9,3)
ci.vals[,1] <- c(1,3,5,10,20,30,40,50, 100)
ci.vals[,2] <- c(0.063, 0.348, 0.473, 0.634, 0.737, 0.786, 0.815, 0.832, 0.883)
ci.vals[,3] <- c(1.96, 1.619, 1.487, 1.351, 1.253, 1.211, 1.183, 1.162, 1.116)

#-- plot of just indices
rmse.val <- as.vector(unlist(asap$RMSE))
rmse.num <- as.vector(unlist(asap$RMSE.n))
rmse.names <- names(asap$RMSE)
rmse.ind1 <- which(substr(rmse.names,1,8)=="rmse.ind")
rmse.ind <- rmse.ind1[which(rmse.val[rmse.ind1]>0)]
tname1 <- rmse.names[rmse.ind]
tname <- substr(tname1,6, max(nchar(tname1))  )
tname[1:length(index.names)] <- index.names


if (length(rmse.ind)>0) {

plot(seq(0,100), rep(1, 101), lty=1, type='l', ylim=c(0,2), xlab="Number of Residuals",
        ylab="RMSE")
lines(ci.vals[,1], ci.vals[,2], lty=2, lwd=2)
lines(ci.vals[,1], ci.vals[,3], lty=2, lwd=2)

points(rmse.num[rmse.ind], rmse.val[rmse.ind], pch=seq(1,length(rmse.ind)), 
         col=liz.palette[seq(1,length(rmse.ind))], cex=1.5 )


plot(seq(1,20), seq(1,20), type='n', axes=F, xlab="", ylab="")
if (length(rmse.ind)<20) {
            text( x= rep(1,length(rmse.ind)), y=seq(1, length(rmse.ind)), 
                labels=tname,   pos=4, cex=0.9)
          points(x=rep(0.5, length(tname)), y=seq(1, length(tname)), 
              pch=seq(1,length(tname)), col=liz.palette[seq(1,length(tname))] )
                }
if (length(rmse.ind)>=20 & length(rmse.ind)<40) {
            text( x= rep(1,20), y=seq(1, 20), labels=tname[1:20],
                pos=4, cex=0.9)
            text( x= rep(7,(length(tname)-20)), y=seq(1, (length(tname)-20)), 
               labels=tname[21:(length(tname)-20)], pos=4, cex=0.9)
          points(x=rep(0.5, 20), y=seq(1, 20), 
              pch=seq(1,20), col=liz.palette[seq(1,20)] )
          points(x=rep(6.5, (length(tname)-20)), y=seq(1,(length(tname)-20)), 
             pch=seq(1,(length(tname)-20)), col=liz.palette[seq(21,(length(tname)-20))] )
                 }
if (length(rmse.ind)>=40 & length(rmse.ind)<60) {
            text( x= rep(1,20), y=seq(1, 20), labels=tname[1:20], pos=4, cex=0.9)
            text( x= rep(7,20), y=seq(1,20), labels=tname[21:40], pos=4, cex=0.9)
            text( x= rep(13,(length(tname)-40)), y=seq(1,(length(tname)-40)), 
              labels=tname[41:length(tname)], pos=4, cex=0.9)
          points(x=rep(0.5, 20), y=seq(1, 20), 
              pch=seq(1,20), col=liz.palette[seq(1,20)] )
          points(x=rep(6.5,20), y=seq(1,20), 
             pch=seq(1,20), col=liz.palette[seq(21,40)] )
          points(x=rep(12.5, (length(tname)-40)), y=seq(1,(length(tname)-40)), 
             pch=seq(1,(length(tname)-40)), col=liz.palette[seq(41,(length(tname)-40))] )
                }

  title(main="Root Mean Square Error for Indices", outer=T, cex=0.85, line=-1)
  if (save.plots) savePlot(paste(od, "RMSE.95CI.Indices.",plotf, sep=""), type=plotf)
       }    # end if-statement for rmse.ind
    

#-- plot of just catch
rmse.val <- as.vector(unlist(asap$RMSE))
rmse.num <- as.vector(unlist(asap$RMSE.n))
rmse.names <- names(asap$RMSE)
rmse.cat1 <- which(substr(rmse.names,1,8)=="rmse.cat" | substr(rmse.names,1,8)=="rmse.dis")
rmse.cat <- rmse.cat1[which(rmse.val[rmse.cat1]>0)]
tname1 <- rmse.names[rmse.cat]
tname <- substr(tname1,6, max(nchar(tname1))  )
#if (asap$parms$nfleets>1 ) tname <- c(fleet.names, "catch.tot")

if (length(rmse.cat)>0)  {

par(mfrow=c(2,1) )#need to specify 2/3 layout
plot(seq(0,100), rep(1, 101), lty=1, type='l', ylim=c(0,2), xlab="Number of Residuals",
        ylab="RMSE")
lines(ci.vals[,1], ci.vals[,2], lty=2, lwd=2)
lines(ci.vals[,1], ci.vals[,3], lty=2, lwd=2)

points(rmse.num[rmse.cat], rmse.val[rmse.cat], pch=seq(1,length(rmse.cat)), 
         col=liz.palette[seq(1,length(rmse.cat))], cex=1.5 )


plot(seq(1,20), seq(1,20), type='n', axes=F, xlab="", ylab="")
if (length(rmse.cat)<20) {
            text( x= rep(1,length(rmse.cat)), y=seq(1, length(rmse.cat)), 
                labels=tname,   pos=4, cex=0.9)
          points(x=rep(0.5, length(tname)), y=seq(1, length(tname)), 
              pch=seq(1,length(tname)), col=liz.palette[seq(1,length(tname))] )
                }
if (length(rmse.cat)>=20 & length(rmse.cat)<40) {
            text( x= rep(1,20), y=seq(1, 20), labels=tname[1:20],
                pos=4, cex=0.9)
            text( x= rep(7,(length(tname)-20)), y=seq(1, (length(tname)-20)), 
               labels=tname[21:(length(tname)-20)], pos=4, cex=0.9)
          points(x=rep(0.5, 20), y=seq(1, 20), 
              pch=seq(1,20), col=liz.palette[seq(1,20)] )
          points(x=rep(6.5, (length(tname)-20)), y=seq(1,(length(tname)-20)), 
             pch=seq(1,(length(tname)-20)), col=liz.palette[seq(21,(length(tname)-20))] )
                 }
if (length(rmse.cat)>=40 & length(rmse.cat)<60) {
            text( x= rep(1,20), y=seq(1, 20), labels=tname[1:20], pos=4, cex=0.9)
            text( x= rep(7,20), y=seq(1,20), labels=tname[21:40], pos=4, cex=0.9)
            text( x= rep(13,(length(tname)-40)), y=seq(1,(length(tname)-40)), 
              labels=tname[41:length(tname)], pos=4, cex=0.9)
          points(x=rep(0.5, 20), y=seq(1, 20), 
              pch=seq(1,20), col=liz.palette[seq(1,20)] )
          points(x=rep(6.5,20), y=seq(1,20), 
             pch=seq(1,20), col=liz.palette[seq(21,40)] )
          points(x=rep(12.5, (length(tname)-40)), y=seq(1,(length(tname)-40)), 
             pch=seq(1,(length(tname)-40)), col=liz.palette[seq(41,(length(tname)-40))] )
                }

  title(main="Root Mean Square Error for Catch", outer=T, cex=0.85, line=-1)
  if (save.plots) savePlot(paste(od, "RMSE.95CI.Catch.",plotf, sep=""), type=plotf)
           }  # end if-statement for rmse.cat



#-- plot of parameters with priors
rmse.val <- as.vector(unlist(asap$RMSE))
rmse.num <- as.vector(unlist(asap$RMSE.n))
rmse.names <- names(asap$RMSE)
#rmse.prior1 <- which(substr(rmse.names,1,8)=="rmse.cat" | substr(rmse.names,1,8)=="rmse.dis")
rmse.prior1 <- rmse.names[c(-rmse.cat1, -rmse.ind1)]
rmse.prior <- rmse.prior1[which(rmse.val[rmse.prior1]>0)]

if (length(rmse.prior)>0) {
par(mfrow=c(2,1) )#need to specify 2/3 layout
plot(seq(0,100), rep(1, 101), lty=1, type='l', ylim=c(0,2), xlab="Number of Residuals",
        ylab="RMSE")
lines(ci.vals[,1], ci.vals[,2], lty=2, lwd=2)
lines(ci.vals[,1], ci.vals[,3], lty=2, lwd=2)

points(rmse.num[rmse.prior], rmse.val[rmse.prior], pch=seq(1,length(rmse.prior)), 
         col=liz.palette[seq(1,length(rmse.prior))], cex=1.5 )

plot(seq(1,20), seq(1,20), type='n', axes=F, xlab="", ylab="")
text( x= rep(1,length(rmse.prior)), y=seq(1, length(rmse.prior)), labels=rmse.names[rmse.prior],
                pos=4)
points(x=rep(0.5, length(rmse.prior)), y=seq(1, length(rmse.prior)), 
    pch=seq(1,length(rmse.prior)), col=liz.palette[seq(1,length(rmse.prior))] )
title(main="Root Mean Square Error for Catch", outer=T, cex=0.85)
  if (save.plots) savePlot(paste(od, "RMSE.95CI.Prior.",plotf, sep=""), type=plotf)
       }
                                                                                  
} #end function

#------------------------------------
# Catch 4 panel plots by fleet
plot.catch.4.panel <- function(asap){
  par(mar=c(4,4,3,2), oma=c(1,1,1,1), mfrow=c(2,2)  )
  for (i in 1:asap$parms$nfleets) {
     catch.yrs = which(asap$catch.obs[i,]>0)
     x.yrs = as.numeric(names(catch.yrs))
     plot(x.yrs, asap$catch.obs[i,catch.yrs] ,
         type='p', col=liz.palette[i], pch=1, xlab="Year", ylab="Total Catch", 
         ylim=c(0, 1.1*max(asap$catch.obs[i,catch.yrs] )) )    
       lines(x.yrs, asap$catch.pred[i,catch.yrs], col=liz.palette[i], lwd=2)
     
     sigma <- sqrt(log(asap$control.parms$catch.tot.cv[catch.yrs,i]^2+1.0))
     log.ob.min <- log(asap$catch.obs[i,catch.yrs])-1.96*sigma
     log.ob.max <- log(asap$catch.obs[i,catch.yrs])+1.96*sigma
     plot(x.yrs, log(asap$catch.obs[i,catch.yrs]) ,
         type='p', col=liz.palette[i], pch=1, xlab="Year", ylab="Ln(Total Catch)", 
         ylim=c(min(log.ob.min,log(asap$catch.pred[i,catch.yrs])), 
         1.1*max(log.ob.max,log(asap$catch.pred[i,catch.yrs]))) )    
       lines(x.yrs, log(asap$catch.pred[i,catch.yrs]), col=liz.palette[i], lwd=2)
       arrows(x.yrs, log.ob.min, x.yrs, log.ob.max, length=0)
       title (paste("Fleet ",i, " Catch  (", fleet.names[i], ")", sep=""), outer=T, line=-1 )

     c.resid <- rep(NA, length(catch.yrs))
     c.resid <- asap$catch.std.resid[i, catch.yrs]
     plot(x.yrs, c.resid, type='h', lwd=2, col=liz.palette[i], 
       xlab="Year", ylab="Log-scale Std. Residual") 
       abline(h=0)

     hist(c.resid, plot=T, xlab="Std. Residual", ylab="Probability Density", freq=F, main=NULL) 
     if (save.plots) savePlot(paste(od, "Catch.4panel.",i,".",plotf, sep=""), type=plotf) 
  } #end loop nfleets
  par(mfrow=c(1,1))
}

#------------------------------------
# Discard 4 panel plots by fleet
plot.discard.4.panel <- function(asap){
  par(mar=c(4,4,3,2), oma=c(1,1,1,1), mfrow=c(2,2)  )
  for (i in 1:asap$parms$nfleets) {
     discard.yrs = which(asap$discard.obs[i,]>0)
     x.yrs = as.numeric(names(discard.yrs))     
    if(length(discard.yrs) > 0){
     plot(as.numeric(names(discard.yrs)), asap$discard.obs[i,discard.yrs] ,
         type='p', col=liz.palette[i], pch=1, xlab="Year", ylab="Total Discards", 
         ylim=c(0, 1.1*max(asap$discard.obs[i,discard.yrs] )) )    
       lines( as.numeric(names(discard.yrs)), asap$discard.pred[i,discard.yrs], col=liz.palette[i], lwd=2)
     
     sigma <- sqrt(log(asap$control.parms$catch.tot.cv[discard.yrs,i]^2+1.0))
     log.ob.min <- log(asap$discard.obs[i,discard.yrs])-1.96*sigma
     log.ob.max <- log(asap$discard.obs[i,discard.yrs])+1.96*sigma
     plot(as.numeric(names(discard.yrs)), log(asap$discard.obs[i,discard.yrs]) ,
         type='p', col=liz.palette[i], pch=1, xlab="Year", ylab="Ln(Total Discards)", 
         ylim=c(min(log.ob.min,log(asap$discard.pred[i,discard.yrs])), 
         1.1*max(log.ob.max,log(asap$discard.pred[i,discard.yrs]) )) )   
       lines( as.numeric(names(discard.yrs)), log(asap$discard.pred[i,discard.yrs]), col=liz.palette[i], lwd=2)
       arrows(x.yrs, log.ob.min, x.yrs, log.ob.max, length=0)
       title (paste("Fleet ",i, " Discards  (", fleet.names[i], ")", sep=""), outer=T, line=-1 )

     c.resid <- rep(NA, length(discard.yrs))
     c.resid <- asap$discard.std.resid[i, discard.yrs]
     plot( as.numeric(names(discard.yrs)), c.resid, type='h', lwd=2, col=liz.palette[i], 
       xlab="Year", ylab="Log-scale Std. Residual") 
       abline(h=0)

     hist(c.resid, plot=T, xlab="Std. Residual", ylab="Probability Density", freq=F, main=NULL) 
     if (save.plots) savePlot(paste(od, "Discard.4panel.",i,".",plotf, sep=""), type=plotf) 
    }
  } #end loop nfleets
  par(mfrow=c(1,1))
}

#------------------------------------
# Catch age composition by fleet (set is.catch.flag to False to plot discards)
plot.catch.age.comp <- function(asap,is.catch.flag=T){
  par(mar=c(4,4,2,2), oma=c(1,1,1,1), mfcol=c(5,3)  )  
  cc=0
  for (i in 1:asap$parms$nfleets) {
    acomp.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+1] )
    acomp.pred <- as.data.frame(asap$catch.comp.mats[4*(i-1)+2] )
    catch.yrs = which(asap$fleet.catch.Neff.init[i,]>0)
    my.title <- "Catch"
    my.save <- "Catch.Age.Comp.F"
    if (!is.catch.flag){
       acomp.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+3] )
       acomp.pred <- as.data.frame(asap$catch.comp.mats[4*(i-1)+4] )
       catch.yrs = which(asap$fleet.discard.Neff.init[i,]>0)
       my.title <- "Discards"
       my.save <- "Discard.Age.Comp.F"
    }
    if (length(catch.yrs)>0){
      cc=cc+1
      plot(1:10,1:10,type='n',axes='F',xlab="",ylab="")
        box()
        text(5,7,paste("Fleet ",i))
        text(5,5,fleet.names[i],cex=0.85)
        arrows(5,3,5,1,length=0.1)
        title (my.title, outer=T, line=0 )
        
      for (j in 1:length(catch.yrs)) {
        cc=cc+1
        plot(seq(asap$fleet.sel.start.age[i],asap$fleet.sel.end.age[i]), acomp.obs[catch.yrs[j],] ,
          type='p', col=liz.palette[i],
          pch=1, xlab="Age", ylab="Proportion at Age", ylim=c(0, 1.1 ) )
          lines(seq(asap$fleet.sel.start.age[i],asap$fleet.sel.end.age[i]), acomp.pred[catch.yrs[j],] , 
             col=liz.palette[i],  lwd=2)
          title( paste("Year = ", as.numeric(names(catch.yrs[j])), sep=""), outer=F )
        if (cc%%15==0)  {
           title (my.title, outer=T, line=0 )
           if (save.plots) savePlot( paste(od, my.save, i, ".p", (cc/15),"." , plotf, sep=""), type=plotf)
        } #end cc test
      }  #end loop on nyears
    } # end catch.yrs test
  }  #end loop on nfleets
  if (cc>1) {
    title (my.title, outer=T, line=0 )
    if (save.plots) savePlot( paste(od, my.save, i, ".p", ceiling(cc/15),"." , plotf, sep=""), type=plotf)
  } 
  par(mfcol=c(1,1)) 
}

#------------------------------------
# Bubble plots of catch age comp resids (set is.catch.flag to False to plot Discard age comp resids)
plot.catch.age.comp.resids <- function(asap,is.catch.flag=T){
  par(mar=c(4,4,2,2), oma=c(1,1,1,1), mfrow=c(1,1))

  ages=seq(1, asap$parms$nages)
  nages=length(ages)
  years=seq(asap$parms$styr, asap$parms$endyr)
  nyrs=asap$parms$nyears

  for (i in 1:asap$parms$nfleets) {
    acomp.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+1])
    acomp.pred <- as.data.frame(asap$catch.comp.mats[4*(i-1)+2]) 
    catch.yrs <- which(asap$fleet.catch.Neff.init[i,]>0)
    my.title <- "Age Comp Residuals for Catch by Fleet "
    my.save <- "catch.resid.bubble.plots."
    if (!is.catch.flag){
       acomp.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+3])
       acomp.pred <- as.data.frame(asap$catch.comp.mats[4*(i-1)+4]) 
       catch.yrs <- which(asap$fleet.discard.Neff.init[i,]>0)
       my.title <- "Age Comp Residuals for Discards by Fleet "
       my.save <- "discard.resid.bubble.plots."
    }
    acomp.catch.resids <- matrix(NA, nrow=nyrs, ncol=nages)
    if (length(catch.yrs)>0){
      s.age <- asap$fleet.sel.start.age[i]
      e.age <- asap$fleet.sel.end.age[i]
      for (j in 1:length(catch.yrs)){
        resids <- as.numeric(acomp.obs[catch.yrs[j],] - acomp.pred[catch.yrs[j],])  # NOTE obs-pred
        acomp.catch.resids[as.numeric(catch.yrs[j]),s.age:e.age] <- resids[s.age:e.age]
      }
      z1 <- acomp.catch.resids
        range.resids<-range(abs((as.vector(z1))), na.rm=T)
        scale.resid.bubble.catch <- 25
      
      z3 <- z1 * scale.resid.bubble.catch *scale.catch.bubble2        
      resid.col=matrix(NA, nrow=nyrs, ncol=nages)   # set color for residual bubbles
      resid.col <- ifelse(z3 > 0.0,pos.resid.col, neg.resid.col) 

      plot(ages, rev(ages),  xlim = c(1, nages), ylim = c(years[nyrs],(years[1]-2)), 
          xlab = "Age", ylab = "Residuals (Observed-Predicted)", type = "n", axes=F)
        axis(1, at= ages, lab=ages)
        axis(2, at = rev(years), lab = rev(years), cex.axis=0.75, las=1)
        box()
        abline(h=years, col="lightgray")
        segments(x0=seq(ages[1], nages), y0=rep(years[1],nages),
                 x1=seq(ages[1], nages), y1=rep(years[nyrs],nages), col = "lightgray", lty = 1)

        for (j in 1:nyrs){
           points(ages, rep((years[1]+j-1), nages), cex=abs(z3[j,]), col="black",
              bg = resid.col[j,],  pch = 21)
        }
  
#        bubble.legend1 <- c(0.05,0.2,0.4)
        bubble.legend1 <- c(0.01,0.05,0.1)
        bubble.legend2 <- bubble.legend1 * scale.resid.bubble.catch*scale.catch.bubble2
        legend("topright", xpd=T, legend=bubble.legend1, pch=rep(1, 3), 
             pt.cex=bubble.legend2, horiz=T , col='black'  )
        legend("topleft", xpd=T, legend=c("Neg.", "Pos."), pch=rep(21, 2), pt.cex=3,
             horiz=T , pt.bg=c(neg.resid.col, pos.resid.col), col="black"  )
        text(x= trunc(nages/2), y=(years[1]-1),   cex=0.8,
             label=paste("Max(resid)=",round(range.resids[2],2), sep="") )
          
        title (paste(my.title,i, " (", fleet.names[i], ")", sep=""), 
             outer=T, line=-1 ) 
        if (save.plots) savePlot(paste(od, my.save, i, ".", plotf, sep=''), type=plotf)
     } # end catch.yrs test  
  }   #end loop nfleets
}

#------------------------------------
# Neff (Effective Sample Size) for Fleets (set is.catch.flag to False to get Discards)
plot.fleet.Neff <- function(asap,is.catch.flag=T){
par(mfrow=c(1,1) )

  years <- seq(asap$parms$styr, asap$parms$endyr) 
  Neff.init <- asap$fleet.catch.Neff.init
  Neff.est <- asap$fleet.catch.Neff.est
  my.title <- "Catch Neff Fleet "
  my.save <- "Catch.Neff.F"
  if (!is.catch.flag){
     Neff.init <- asap$fleet.discard.Neff.init
     Neff.est <- asap$fleet.discard.Neff.est
     my.title <- "Discard Neff Fleet "
     my.save <- "Discard.Neff.F"
  }
  for (i in 1:asap$parms$nfleets) {
    if (sum(Neff.init[i,]) > 0){
       plot(years,Neff.init[i,], type='p', col=liz.palette[i], pch=1, xlab="Year", ylab="Effective Sample Size", 
          ylim=c(0, 1.1*max(Neff.init[i,],Neff.est[i,])) )    
        lines(years, Neff.est[i,], col=liz.palette[i], lwd=2)
        title (paste(my.title, i, " (", fleet.names[i], ")", sep="") )
        if (save.plots) savePlot(paste(od, my.save, i, ".", plotf, sep=""), type=plotf) 
    }
  } #end loop nfleets
}

#-------------------------------------------------------------------------------
# function to calculate mean and stdev of frequency data
# bins are the values in the class marks (ages in this case)
# N is the total number (ESS in this case)
# frdist is the frequency distribution (should sum to one over the bins)
calc_freq_mean_std <- function(bins,N,frdist){
  Y <- bins
  f <- N*frdist
  n <- sum(f)
  sumfY <- sum(f*Y)
  Ybar <- sumfY/n
  sumfY2 <- sum(f*Y*Y)
  CT <- sumfY^2/n
  sumfy2 <- sumfY2-CT
  s2 <- sumfy2/(n-1)
  sigma <- sqrt(s2)
  stderror <- sigma/sqrt(n)  # note this is standard deviation of the mean
  meanstderror <- c(Ybar,stderror)
  return(meanstderror)
}


#--------- Mean age and RMSE calculations (after Francis 2011)
plot.francis.fleet<-function(asap,is.catch.flag=T) { 
par(mfrow=c(2,1), mar=c(4,4,2,4) )

  years <- seq(asap$parms$styr, asap$parms$endyr) 
  nyrs <- length(years)
  ages <- seq(1,asap$parms$nages)

  Neff.init <- asap$fleet.catch.Neff.init
  Neff.est <- asap$fleet.catch.Neff.est
  my.title <- "Catch "
  my.save <- "Catch."
  if (!is.catch.flag){
     Neff.init <- asap$fleet.discard.Neff.init
     Neff.est <- asap$fleet.discard.Neff.est
     my.title <- "Discard "
     my.save <- "Discard."
  }

for (i in 1:asap$parms$nfleets) {
  t.obs<-   as.data.frame(asap$catch.comp.mats [4*(i-1)+1] )
  t.pred<-   as.data.frame(asap$catch.comp.mats [4*(i-1)+2] ) 
  catch.yrs <- which(asap$fleet.catch.Neff.init[i,]>0)
  scale.Neff= max(asap$fleet.catch.Neff.init[i,])/10
  if (!is.catch.flag){
  scale.Neff= max(asap$fleet.discard.Neff.init[i,])/10
  catch.yrs = which(asap$fleet.discard.Neff.init[i,]>0)       
       t.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+3] )
       t.pred <- as.data.frame(asap$catch.comp.mats[4*(i-1)+4] )
    }  
    
  t.res <- matrix(NA, nrow=0, ncol=6)
  ESS1 <- Neff.init[i,]
  ESS <- ESS1[ESS1>0] 
  num.acomp.yrs <- length(catch.yrs)

 if (length(catch.yrs)>0){  
  for (j in 1:nyrs ) {
    obsf <- calc_freq_mean_std(ages,ESS,t.obs[j,])
    prdf <- calc_freq_mean_std(ages,ESS,t.pred[j,])
    t.res <- rbind(t.res,c(obsf[1],obsf[1]-1.96*obsf[2],obsf[1]+1.96*obsf[2],
                       prdf[1],prdf[1]-1.96*prdf[2],prdf[1]+1.96*prdf[2]))
  } # end j-loop
  
  
  #res
  Neff.bars <-  ESS1/scale.Neff
  ess.uniques <- unique(ESS)  
  len.uniques<- length(ess.uniques)

    #-- First plot
par(mfrow=c(2,1), mar=c(4,4,2,4) )
    
    plot(years,t.res[,4],lty=1,lwd=2,col="blue", ylim=c(0,10), ylab="Mean Age", type='l')
    if (len.uniques>1) {
    segments(x0=years[catch.yrs], y0=rep(0,num.acomp.yrs), x1=years[catch.yrs], 
             y1=Neff.bars[catch.yrs], lwd=5, col='#66BB77aa')
    axis(side=4, at=seq(0,10,2), lab=scale.Neff*seq(0,10,2), las=2 )
    mtext(side = 4, "Eff. Sample Size", line = 3, col='#66BB77')    
    lines(years,t.res[,4],lty=1,lwd=2,col="blue")
      } #end test for >1 unique ESS

    errbar(years,t.res[,1],t.res[,3],t.res[,2],ylim=c(0,10),add=T)


   if(len.uniques==1)    title(main=paste(my.title,"Fleet ",i, " (", fleet.names[i], ")"," ESS = ",ESS[1], sep=""), outer=F)
   if(len.uniques>1)    title(main=paste(my.title,"Fleet ",i, " (", fleet.names[i], ")", sep=""), outer=F)

  sdres <- (t.res[,4]-t.res[,1])/((t.res[,1]-t.res[,2])/1.96)
  sdres[sdres==Inf]  <- NA
  sdres[sdres==-Inf]  <- NA
  
  sdnr <- sd(sdres, na.rm=T)
  rmse <- sqrt(mean(sdres^2, na.rm=T))
  barp(sdres,names.arg=years,col="grey50",ylab="Std Resids",xlab="Year")
  legend('topleft',legend=c(paste("SDNR=",round(sdnr,2), sep=""),paste("RMSE=",round(rmse,2), sep="")),cex=0.7,h=T)
  if (save.plots) savePlot(paste(od,my.save,"ESS_Mean_Age_Fleet_",i,".png", sep=""), type='png')


        #-- Second plot
par(mfrow=c(1,1), mar=rep(4,4) )        
   aa <- sort(sdres)
   bb <- qqnorm(aa, plot.it = F)
   qqplot(bb$x,bb$y,xlab="Theoretical Quantile",ylab="Sample Quantiles")
   qqline(aa,col="red")
   abline(a=0,b=1,col="blue",lty=2)
   legend('topleft',legend=c("y=x","1st-3rd quartiles"),lty=c(2,1),col=c("blue","red"))

   if(len.uniques==1)  title(main=paste(my.title,"Fleet ",i, " (", fleet.names[i], ")"," ESS = ",ESS[1], sep=""), outer=F)
   if(len.uniques>1)    title(main=paste(my.title,"Fleet ",i, " (", fleet.names[i], ")", sep=""), outer=F)
   
  if (save.plots) savePlot(paste(od,my.save,"ESS_QQplot_Fleet_",i,".png", sep=""), type='png')
   
    } #end test for catch.yrs
   }   # end loop on number of fleets

} #end function   

   


#------------------------------------
# Indices 4 panel plots
plot.indices.4panel <- function(asap){
  par(mar=c(4,4,3,2), oma=c(1,1,1,1), mfrow=c(2,2)  )

  for (i in 1:asap$parms$nindices) {
     index.year <- unlist(asap$index.year[i])
     index.obs <- unlist(asap$index.obs[i])
     index.pred <- unlist(asap$index.pred[i])
     index.std.resid <- unlist(asap$index.std.resid[i])
     
     plot(index.year, index.obs, type='p', col=liz.palette[i], pch=1, 
         xlab="Year", ylab="Index Value", xlim=c(asap$parms$styr,asap$parms$endyr),
         ylim=c(0, 1.1*max(index.obs)) )    
       lines(index.year, index.pred, col=liz.palette[i], lwd=2)
  
     log.ob.min <- log(index.obs)-1.96*unlist(asap$index.sigma[i])
     log.ob.max <- log(index.obs)+1.96*unlist(asap$index.sigma[i])
     plot(index.year, log(index.obs), type='p', col=liz.palette[i], pch=1, 
         xlab="Year", ylab="Ln(Index)", xlim=c(asap$parms$styr,asap$parms$endyr),
         ylim=c(min(log.ob.min,log(index.obs)), 1.1*max(log.ob.max,log(index.pred))) )    
       lines(index.year, log(index.pred), col=liz.palette[i], lwd=2)
       arrows(index.year, log.ob.min, index.year, log.ob.max, length=0)
       title (paste("Index ", i, " (", index.names[i], ")", sep=""), outer=T, line=-1 )
  
     plot(index.year, index.std.resid, type='h', lwd=2, col=liz.palette[i], 
       xlab="Year", ylab="Log-scale Std. Residual", xlim=c(asap$parms$styr,asap$parms$endyr)) 
       abline(h=0)

     hist(index.std.resid, plot=T, xlab="Std. Residual", ylab="Probability Density", freq=F, main=NULL) 
     if (save.plots) savePlot(paste(od, "Index.4panel.",i,".",plotf, sep=""), type=plotf) 
  }   # end i-loop over nindices
  
  par(mfrow=c(1,1))
}

#------------------------------------
# Bubble plots of index age comp resids 
plot.index.age.comp.resids <- function(asap){
  par(mar=c(4,4,2,2), oma=c(1,1,1,1), mfrow=c(1,1))
  ages=seq(1, asap$parms$nages)
  nages=length(ages)
  years=seq(asap$parms$styr, asap$parms$endyr)
  nyrs=asap$parms$nyears

  for (i in 1:asap$parms$nindices) {
    acomp.obs <- as.data.frame(asap$index.comp.mats[2*i-1])
    acomp.pred <- as.data.frame(asap$index.comp.mats[2*i]) 
    index.yrs <- which(asap$index.Neff.init[i,]>0)
    my.title <- "Age Comp Residuals for Index "
    my.save <- "index.resid.bubble.plots."
    acomp.index.resids <- matrix(NA, nrow=nyrs, ncol=nages)
    if (length(index.yrs)>0){
      s.age <- asap$control.parms$index.sel.start.age[i]
      e.age <- asap$control.parms$index.sel.end.age[i]
      for (j in 1:length(index.yrs)){
        resids <- as.numeric(acomp.obs[index.yrs[j],] - acomp.pred[index.yrs[j],])  # NOTE obs-pred
        acomp.index.resids[as.numeric(index.yrs[j]),s.age:e.age] <- resids[s.age:e.age]
      }
      z1 <- acomp.index.resids
        range.resids<-range(abs((as.vector(z1))), na.rm=T)
        scale.resid.bubble.index <- 25

      z3 <- z1 * scale.resid.bubble.index *scale.index.bubble2        
      resid.col=matrix(NA, nrow=nyrs, ncol=nages)   # set color for residual bubbles
      resid.col <- ifelse(z3 > 0.0,pos.resid.col, neg.resid.col) 

      plot(ages, rev(ages),  xlim = c(1, nages), ylim = c(years[nyrs],(years[1]-2)), 
          xlab = "Age", ylab = "Residuals (Observed-Predicted)", type = "n", axes=F)
        axis(1, at= ages, lab=ages)
        axis(2, at = rev(years), lab = rev(years), cex.axis=0.75, las=1)
        box()
        abline(h=years, col="lightgray")
        segments(x0=seq(ages[1], nages), y0=rep(years[1],nages),
                 x1=seq(ages[1], nages), y1=rep(years[nyrs],nages), col = "lightgray", lty = 1)

        for (j in 1:nyrs){
           points(ages, rep((years[1]+j-1), nages), cex=abs(z3[j,]), col="black",
              bg = resid.col[j,],  pch = 21)
        }
  
#        bubble.legend1 <- c(0.05,0.2,0.4)
        bubble.legend1 <- c(0.01,0.05,0.1)

        bubble.legend2 <- bubble.legend1 * scale.resid.bubble.index *scale.index.bubble2
        legend("topright", xpd=T, legend=bubble.legend1, pch=rep(1, 3), 
             pt.cex=bubble.legend2, horiz=T , col='black'  )
        legend("topleft", xpd=T, legend=c("Neg.", "Pos."), pch=rep(21, 2), pt.cex=3,
             horiz=T , pt.bg=c(neg.resid.col, pos.resid.col), col="black"  )
        text(x= trunc(nages/2), y=(years[1]-1),   cex=0.8,
             label=paste("Max(resid)=",round(range.resids[2],2), sep="") )

        title (paste(my.title,i, " (", index.names[i], ")", sep=""), 
             outer=T, line=-1 ) 
        if (save.plots) savePlot(paste(od, my.save, i, ".", plotf, sep=''), type=plotf)
     } # end index.yrs test  
  }   #end loop nindices
}

#___Neff (Effective Sample Size) for Indices ___________________________________________

plot.index.Neff  <- function(asap)  {
par(mfrow=c(1,1) )

  years <- seq(asap$parms$styr, asap$parms$endyr) 
  Neff.init <- asap$index.Neff.init
  Neff.est <- asap$index.Neff.est
  index.acomp.flags <- asap$control.parms$index.age.comp.flag
  my.title <- "Index Neff "
  my.save <- "Index.Neff."

for (i in 1:asap$parms$nindices) {
 #cum.age.comp.flag<-0
   if( index.acomp.flags[i]>0) {
       plot(years,Neff.init[i,], type='p', col=liz.palette[i], pch=1, xlab="Year", ylab="Effective Sample Size", 
          ylim=c(0, 1.1*max(Neff.init[i,],Neff.est[i,])) )    
        lines(years, Neff.est[i,], col=liz.palette[i], lwd=2)
        title (paste(my.title, i, " (", index.names[i], ")", sep="") )
   if (save.plots) savePlot(paste(od, my.save, i, ".", plotf, sep=""), type=plotf) 
    } #end test for age-comp
  } #end loop nindices
  
}   #end function



#------------------------------------
# INDEX Mean age and RMSE calculations (Francis 2011)

plot.francis.index <- function(asap) {

par(mfrow=c(2,1), mar=c(4,4,2,4) )

  years <- seq(asap$parms$styr, asap$parms$endyr)
  ages <- seq(1,asap$parms$nages)
  cum.age.comp.flag<-0
  index.acomp.flags <- asap$control.parms$index.age.comp.flag
  ind.use <- asap$initial.guesses$index.use.flag  # NOTE: this would make more sense in the control.parm list

if( sum(index.acomp.flags)>0) {


i.seq.obs<-seq(1,2*asap$parms$nindices, by=2)
i.seq.obs<-i.seq.obs[which(index.acomp.flags>0)]
i.seq.pred<-seq(2,2*asap$parms$nindices, by=2)
i.seq.pred<-i.seq.pred[which(index.acomp.flags>0)]
i.neff.seq.obs<-seq(1,sum(index.acomp.flags))

  for (i in 1:asap$parms$nindices) {

    #if (index.acomp.flags[i]==1)  {
    if (index.acomp.flags[i]==1 & sum(asap$index.Neff.init[i,])>1)  {

    cum.age.comp.flag <- sum(index.acomp.flags[1:i])
      
    t.obs<-   as.data.frame(asap$index.comp.mats [i.seq.obs[cum.age.comp.flag] ] )
    t.pred<-   as.data.frame(asap$index.comp.mats [i.seq.pred[cum.age.comp.flag] ] ) 
  
    t.res <- matrix(NA, nrow=0, ncol=6)
  
    #ESS1 <- asap$index.Neff.init [i.neff.seq.obs[cum.age.comp.flag], ] 
    ESS1 <- asap$index.Neff.init [i, ] 
    ESS <- ESS1[ESS1>0] 
    scale.Neff= max(ESS, na.rm=T)/10


    iyrs <- as.matrix(as.data.frame(asap$index.year [i]) )  
    num.ind.yrs <- length(iyrs)  # total number of years in model                                                   
    acyrs1 <-  which(years %in% iyrs)  # position of iyrs in vector of years 
    acyrs2 <- acyrs1[iyrs %in% years[ESS1>0] ] # position of iyrs in years where age comp exists

    num.acyrs2 <- length(acyrs2)  # number of age comp years 
  
  
  for (j in 1:num.acyrs2 ){
  #for (j in 1:num.ind.yrs ){  
    jj <- acyrs2[j]
    obsf <- calc_freq_mean_std(ages,ESS[j],as.matrix(t.obs[jj,]) )
    prdf <- calc_freq_mean_std(ages,ESS[j],t.pred[jj,])
    t.res <- rbind(t.res,c(obsf[1],obsf[1]-1.96*obsf[2],obsf[1]+1.96*obsf[2],
                       prdf[1],prdf[1]-1.96*prdf[2],prdf[1]+1.96*prdf[2]))
  } # end j loop

  ess.uniques <- unique(ESS)  
  len.uniques<- length(ess.uniques)
  Neff.bars <-  ESS/scale.Neff
  
  
    #-- First plot
par(mfrow=c(2,1), mar=c(4,4,2,4) )    
    plot(years[acyrs2],t.res[,4],lty=1,lwd=2,col="blue", ylim=c(0,10), ylab="Mean Age", 
       type='l', xlab="Years")
    if (len.uniques>1) {
    segments(x0=years[acyrs2], y0=rep(0,num.acyrs2), x1=years[acyrs2], y1=Neff.bars, lwd=5, col='#66BB77aa')
    axis(side=4, at=seq(0,10,2), lab=scale.Neff*seq(0,10,2), las=2 )
    mtext(side = 4, "Eff. Sample Size", line = 3, col='#66BB77')    
    lines(years[acyrs2],t.res[,4],lty=1,lwd=2,col="blue")
      } #end test for >1 unique ESS

   errbar(years[acyrs2], t.res[,1],t.res[,3],t.res[,2],ylim=c(0,10),ylab="Mean Age", 
        xlab="Years", add=T)

   if(len.uniques==1)    title(main=paste("Index ",i, " (",index.names[i],")", " ESS = ",ESS[i], sep=""), outer=F)
   if(len.uniques>1)    title(main=paste("Index ",i, " (",index.names[i],")",  sep=""), outer=F)


  sdres <- (t.res[,4]-t.res[,1])/((t.res[,1]-t.res[,2])/1.96)
  sdres[sdres==Inf]  <- NA
  sdres[sdres==-Inf]  <- NA
  sdnr <- sd(sdres, na.rm=T)
  rmse <- sqrt(mean(sdres^2, na.rm=T))
  barp(sdres,names.arg=years[acyrs2],col="grey50",ylab="Std Resids",xlab="Year")
  legend('topleft',legend=c(paste("SDNR=",round(sdnr,2), sep=""),paste("RMSE=",round(rmse,2), sep="")),cex=0.7,h=T)

  #if (save.plots) savePlot(paste(od,"Francis_Mean_Age_Orig_Ind_",ind.use[i],".png", sep=""), type='png')
  if (save.plots) savePlot(paste(od,"Francis_Mean_Age_Orig_Ind_",i,".png", sep=""), type='png')
  

      #-- Second plot
par(mfrow=c(1,1), mar=rep(4,4) )      
   aa <- sort(sdres)
   bb <- qqnorm(aa, plot.it=F)
   qqplot(bb$x,bb$y,xlab="Theoretical Quantile",ylab="Sample Quantiles")
   qqline(aa,col="red")
   abline(a=0,b=1,col="blue",lty=2)
   legend('topleft',legend=c("y=x","1st-3rd quartiles"),lty=c(2,1),col=c("blue","red"))

   if(len.uniques==1)    title(main=paste("Index ",i, " (",index.names[i],")", " ESS = ",ESS[i], sep=""), outer=F)
   if(len.uniques>1)    title(main=paste("Index ",i, " (",index.names[i],")",  sep=""), outer=F)
  if (save.plots) savePlot(paste(od,"Francis_QQ_Orig_Ind_",ind.use[i],".png", sep=""), type='png')
  if (save.plots) savePlot(paste(od,"Francis_QQ_Orig_Ind_",i,".png", sep=""), type='png')
    

          }  #end test for presence/absence of age-comp  
       } # end loop on nindices
    }  #end test for index age comp  
   

} # end function




#------------------------------------
##  Plot Results

#------------------------------------
# Fleet selectivities (by block)
plot.fleet.sel.blocks <- function(asap){
par(mfrow=c(1,1) )
  cc=0
  years <- 1:asap$parms$nyears
  for (i in 1:asap$parms$nfleets) {
     a1 <- asap$fleet.sel.start.age[i]
     a2 <- asap$fleet.sel.end.age[i]
     blocks <- unique(asap$fleet.sel.blocks[i,])
     n.blocks <- length(blocks)
     sel.mat <- as.data.frame(asap$fleet.sel.mats[i])
     sel <- matrix(0, nrow=n.blocks, ncol=asap$parms$nages)
     yr <- rep(NA, n.blocks)
     my.col <- rep(NA, n.blocks)
     for (j in 1:n.blocks){
       cc=cc+1
       my.col[j] <- liz.palette[cc]
       yr[j] <- min(years[asap$fleet.sel.blocks[i,]==blocks[j]])
       sel[j,] <- as.numeric(sel.mat[yr[j],a1:a2])
       if (j==1){
         plot(1:asap$parms$nages, sel[j,], type='l', col=my.col[j], 
            xlim=c(0,asap$parms$nages+3), ylim=c(0,1.1), 
            xlab="Age", ylab="Selectivity at Age", lwd=2) 
       }
       if (j>1){
         lines(1:asap$parms$nages, sel[j,], type='l', col=my.col[j], lwd=2)
       }
     }
     title(paste("Fleet ",i," (",fleet.names[i],")", sep=""))
     legend("topright", col=my.col, legend=asap$parms$styr+yr-1, lwd=2)
     if (save.plots) savePlot(paste(od, "Catch.Sel.Blocks.Fleet.",i,".",plotf, sep=""), type=plotf)
  }
}


#------------------------------------
# Index selectivities (only for indices with multiple ages selected)
plot.index.selectivities <- function(asap){
par(mfrow=c(1,1) )

  cc=0
  for (i in 1:asap$parms$nindices) {
    a1<-  asap$control.parms$index.sel.start.age[i]
    a2<-  asap$control.parms$index.sel.end.age[i]
    if (a2>a1){
      cc=cc+1
      ind.sel <- rep(NA,asap$parms$nages)
      ind.sel[a1:a2] <- asap$index.sel[i,a1:a2]
      if (cc==1){
          plot(1:asap$parms$nages, ind.sel, type='l', col=liz.palette[cc], ylim=c(0,1.1),
             xlab="Age", ylab="Selectivity at age" , xlim=c(1, (asap$parms$nages+3) ), lwd=2 )
          #leg.txt <- paste("Index_",i,sep="")
          leg.txt <- index.names
      }    
      if (cc>1){
          lines(1:asap$parms$nages, ind.sel, col=liz.palette[cc], lwd=2  )
          #leg.txt <- c(leg.txt,paste("Index_",i,sep=""))
      }    
    }
  } #end i loop
  if (cc>1) legend("topright", col=liz.palette[1:cc], legend=leg.txt, lwd=2)
    title("Indices")
    if (save.plots) savePlot(paste(od, "Index.Sel.",plotf, sep=""), type=plotf) 
  
}

#------------------------------------
#Catchability by year      
plot.catchability <- function(asap, a1) {
par(mfrow=c(1,1), mar=c(6,4,2,2) )

years=seq(asap$parms$styr, asap$parms$endyr)
nyrs=asap$parms$nyears
ind.use <-  which(asap$initial.guesses$index.use.flag>0)
num.q <- length(ind.use)
n.ind=asap$parms$nindices
yaxis.q<-  ifelse(num.q<7, 1.1, 1.2)
q.info <- asap$q.indices
asap.std <- a1$asap.std
log.q.pars = asap.std[asap.std$name=="log_q_year1",]  
log.q.mean <- log.q.pars[,3]
log.q.var <- log.q.pars[,4]^2
q.std <- sqrt((exp(log.q.var)-1)*(exp(2*log.q.mean+log.q.var))  )

if(asap$control.parms$phases$phase.q.devs<=0) {

se.lines=matrix(NA, nrow=num.q, ncol=2)


se.lines[,1]=q.info-2*q.std
se.lines[,2]=q.info+2*q.std

barplot( q.info,  beside=T, axisnames=F, cex.names=0.70 , 
   width=1.0, space=rep(0,num.q), xpd=F,
           xlab = '', ylab ='Catchability (q +/- 2 SE)', 
           ylim = c(0,1.05*max(se.lines[,2])), 
           xlim=c(0,num.q), col="lightblue3")
box()
axis(side=1, las=2, at=seq(0.5, (num.q-0.5) ),  
    labels=index.names, cex=0.70, las=2)

errbar( seq(0.5,(num.q -0.5)  , by=1), yplus=se.lines[,2], 
        as.numeric(q.info),
         yminus=se.lines[,1], add=T, lty=1, pch='.' )

       if (save.plots==T) savePlot( paste(od, "Q.Index.Err.Bars.",plotf, sep=""), type=plotf)      
          } # end block for no q-devs
          
          
if(asap$control.parms$phases$phase.q.devs>0) { 
  cc <- 0  
  num.x.lims <- trunc(n.ind/5) +1
  x.lims <- matrix(NA, num.x.lims, 2)
  for (ix in 1:num.x.lims) {
    if (ix < num.x.lims)  x.lims[ix,1] <- min( as.vector(unlist(asap$index.year [seq((5*(ix-1)+1),5*ix)]) ))
    if (ix==num.x.lims) x.lims[ix,1] <- min( as.vector(unlist(asap$index.year [seq((5*(ix-1)+1),n.ind)])) )

    if (ix < num.x.lims)  x.lims[ix,2] <- max( as.vector(unlist(asap$index.year [seq((5*(ix-1)+1),5*ix)]) ))
    if (ix==num.x.lims) x.lims[ix,2] <- max( as.vector(unlist(asap$index.year [seq((5*(ix-1)+1),n.ind)])) )

  }  # end ix loop
 
 i.lty <- seq(1,5)
  
   for (i in 1: num.q) {
   t.yrs <- as.vector(unlist(asap$index.year [i]))
   t.q  <- unlist(asap$q.random.walk [i])
   if (i%%5 == 1 )   {
      i.x.lims <- trunc(i/5) +1
      plot( t.yrs, t.q, type='l', lty=i.lty[i%%5],xlim=c(x.lims[i.x.lims,1], x.lims[i.x.lims,2]),
      col=liz.palette[i], lwd=2, xlab="Year", ylab="Catchability", 
      ylim=c(0, yaxis.q*max(unlist(asap$q.random.walk )) )   )
     cc = cc+1      
      }
      
    if (i %% 5 %in% c(2,3,4) )  lines(t.yrs, t.q, lty=i.lty[i%%5], col=liz.palette[i], lwd=2 )
     if (i %% 5 ==0 )  lines(t.yrs, t.q, lty=i.lty[5], col=liz.palette[i], lwd=2 )   
         

    if(i %% 5==0 )  {
        #legend("top", legend=ind.use[(i-4):i], lty=seq(1,5), 
#              col=liz.palette[(i-4):i], horiz=T , lwd=rep(2,5))   
        legend("top", legend=index.names[(i-4):i], lty=seq(1,5), 
              col=liz.palette[(i-4):i], horiz=T , lwd=rep(2,5), cex=0.8)    
               
       title ( "Index q estimates", outer=T, line=-1 )
       if (save.plots==T) savePlot( paste(od, "Q.Index.Yr.",cc,".",plotf, sep=""), type=plotf)      
            }  # end modulo test of 5 q's per plot

    if(i == n.ind & n.ind%%5>0 )  {
        #legend("top", legend=ind.use[(i-(n.ind%%5)+1):i], lty=seq(1,5), 
#              col=liz.palette[(i-(n.ind%%5)+1):i], horiz=T, lwd=rep(2,5) )    
        legend("top", legend=index.names[(i-(n.ind%%5)+1):i], lty=seq(1,5), 
              col=liz.palette[(i-(n.ind%%5)+1):i], horiz=T, lwd=rep(2,5), cex=0.8 )    

       title ( "Index q estimates", outer=T, line=-1 )
       if (save.plots==T) savePlot( paste(od, "Q.Index.Yr.",cc,".",plotf, sep=""), type=plotf)      
            }  # end modulo test of 5 q's per plot

          } #end loop over number of q parameters    
               
       }  #end if test for q.dev phase >0
       
       
 } # end function


#------------------------------------

#------------------------------------


#scatter plot of SSB, R with 2-digit year as symbol (lag by 1 year)
plot.recr.ssb.yr <- function(asap)  {

par(mfrow=c(1,1) )

ssb <- asap$SSB
recr <- asap$N.age[,1]
years <- seq(asap$parms$styr, asap$parms$endyr)
nyears <- length(years)
SR <- matrix(NA, (nyears-1), 3)
SR[,1] <- years[1:(nyears-1)]
SR[,2] <- ssb[1:(nyears-1)]
SR[,3] <- recr[2:nyears]


yr.text=substr(SR[,1],3,4)
npts<-length(years)-1

plot( SR[,2], SR[,3], type='n', col='black',
       xlab="SSB ", ylab="Recruits ", 
      ylim=c(0, 1.1*max(SR[,3])), xlim=c(0,1.2*max(SR[,2]))  )    

points (SR[npts,2], SR[npts,3], pch=19, col="#ffaa22", cex=2.5)
text(SR[,2], SR[,3], yr.text[1:(nyears-1)], cex=0.9, col="black")  

if (save.plots==T) savePlot( paste(od, "S_R.Yr.",plotf, sep=""), type=plotf)



}  #end function
#------------------------------------
plot.SR.pred.line <- function(asap) {
par(mfrow=c(1,1) )

a<- asap$SR.parms$SR.alpha 
b<- asap$SR.parms$SR.beta
S0.yr <- asap$SR.annual.parms[,2]
ssb <- asap$SSB
recr <- asap$N.age[,1]
years <- seq(asap$parms$styr, asap$parms$endyr)
nyears <- length(years)
SR <- matrix(NA, (nyears-1), 3)
SR[,1] <- years[1:(nyears-1)]
SR[,2] <- ssb[1:(nyears-1)]
SR[,3] <- recr[2:nyears]

seq.ssb= seq(0, 1.1*max(S0.yr, SR[,2]), length.out=300)
pred.r = a*seq.ssb/(b+seq.ssb)

plot( SR[,2], SR[,3], type='p', col='black', pch=19,
       xlab="SSB (mt)", ylab="Recruits (000s)", 
      ylim=c(0, 1.1*max(SR[,3])), xlim=c(0,1.2*max(SR[,2], S0.yr))  )    

lines(seq.ssb , pred.r,  col='red', lwd=2) 



if (save.plots==T) savePlot( paste(od, "S_R.Pred.Line.",plotf, sep=""), type=plotf)

  }   # end function

#------------------------------------
plot.SSB.F.trend<-function(asap)  {
par(mfrow=c(2,1), mar=c(4,4,2,2) )

ssb <- asap$SSB
fmult <- apply(asap$F.age,1, max) 
f.report <- asap$F.report
years<-  seq(asap$parms$styr, asap$parms$endyr)



plot(years, ssb, type='l', lwd=2, col='blue', xlab="Year", ylab="SSB", 
     ylim=c(0,1.05*max(ssb)) )

plot(years, f.report, type='l', lwd=2, col='black', xlab="Year", ylab="Fishing Mortality", 
     ylim=c(0,1.25*max(fmult, f.report)) )
lines(years, fmult, col='red', lty=2, lwd=2)

legend('topleft', horiz=T, legend=c('F.report', 'F.full'), col=c('black', 'red'),
        lwd=c(2,2), lty=c(1,2), cex=0.85)

if (save.plots==T) savePlot( paste(od, "SSB.F.Timeseries.Line.",plotf, sep=""), type=plotf)

}  #end function

#------------------------------------
plot.all.biomass.types<-function(asap) {
par(mfrow=c(1,1) )

sel.aa <- asap$F.age/apply(asap$F.age,1, max) 
exp.b <- apply(asap$N.age*asap$WAA.mats$WAA.jan1*sel.aa,1,sum)
#exp.b <- asap$exploitable.B
ssb.jan1 <- apply(asap$N.age*asap$maturity*asap$WAA.mats$WAA.jan1,1,sum)
tot.b <-asap$tot.jan1.B
years<-  seq(asap$parms$styr, asap$parms$endyr)


plot(years, tot.b, type='l', col='maroon', lwd=2, lty=1, xlab="Year", ylab="Biomass",
     ylim=c(0, 1.1*max(exp.b, ssb.jan1, tot.b)) )
lines(years, ssb.jan1, col='blue', lwd=2, lty=2)
lines(years, exp.b, col='green2', lwd=2, lty=4)
 legend('top', horiz=T, legend=c("Total", "SSB", "Exploitable"), lty=c(1,2,4),
         lwd=c(2,2,2), col=c("maroon", "blue", "green2") )
title ( "Comparison of January 1 Biomass", outer=T, line=-1 )

if (save.plots==T) savePlot( paste(od, "Biomass.Comparisons.",plotf, sep=""), type=plotf)
} #end function

#------------------------------------
plot.recruitment.devs <- function(asap) {
par(mfrow=c(2,1), mar=c(4,4,2,2) )
#__annual esitmates of Recruitment deviations (2 panel plot)____


years<-  asap$SR.resids[,1]
recr <- asap$SR.resids[,2]
recr.no.devs <- asap$SR.resids[,3]
resids <- asap$SR.resids[,4]

plot( years, recr.no.devs, type='l', col='#114466', lty=1, lwd=2,
       xlab="Year", ylab="Recruits ", 
      ylim=c(0, 1.1*max(recr, recr.no.devs))  )    
lines(years, recr, col="grey35", lwd=2)
points (years, recr, pch=19)


plot( years, resids, type='h', col='black',
       xlab="Year", ylab="Ln(Recruitment deviations)", lwd=2,
      ylim=c(1.1*min(resids) , 1.1*max(resids))  )    
abline(h=0, lwd=1)

if (save.plots==T) savePlot( paste(od, "Recruit.Yr.Devs.",plotf, sep=""), type=plotf)


} # end function
  
#------------------------------------
plot.NAA <- function(asap) {
par(mfrow=c(1,1) )
## stacked barplot of NAA      

N.max=max(apply(asap$N.age,1,sum))

barplot( t(asap$N.age), beside=F  ,  cex.names=0.75 ,  
           width=1, space=rep(0,(asap$parms$nyears)),
           xlab = 'Year', ylab ='ASAP Estimated NAA', ylim = c(0,1.15*N.max), 
           xlim=c(0.5,asap$parms$nyears+1-0.5), col=liz.palette[1:asap$parms$nages] )

legend('top', horiz=T, legend=seq(1, asap$parms$nages), 
        pch=15, col=liz.palette[1:asap$parms$nages], cex=0.8 )

if (save.plots==T) savePlot(paste(od, 'NAA.barplot.stacked.', plotf, sep=''), type=plotf)

#----same information, but proportion at age each year
barplot( t(asap$N.age/apply(asap$N.age,1,sum)), beside=F  ,  cex.names=0.75 ,  
           width=1, space=rep(0,(asap$parms$nyears)),
           xlab = 'Year', ylab ='ASAP Estimated Proportion (NAA)', ylim = c(0,1.1), 
           xlim=c(0.5,asap$parms$nyears+1-0.5), col=liz.palette[1:asap$parms$nages] )

legend('top', horiz=T, legend=seq(1, asap$parms$nages), 
        pch=15, col=liz.palette[1:asap$parms$nages], cex=0.8 )

if (save.plots==T) savePlot(paste(od, 'NAA.proportion.barplot.stacked.', plotf, sep=''), type=plotf)


} # end function

#------------------------------------
plot.SSB.AA <- function(asap)  {

par(mfrow=c(1,1) )

years<-  seq(asap$parms$styr, asap$parms$endyr)
ssb.aa.1 <- asap$N.age*exp(0.25*(-asap$F.age-asap$M.age))
ssb.aa <- ssb.aa.1*asap$maturity*asap$WAA.mats$WAA.ssb #ssb at age

ssb.max=max(apply(ssb.aa,1,sum))

barplot( t(ssb.aa), beside=F  ,  cex.names=0.75 ,  
           width=1, space=rep(0,(asap$parms$nyears)),
           xlab = 'Year', ylab ='ASAP Estimated SSB.AA', ylim = c(0,1.15*ssb.max), 
           xlim=c(0.5,asap$parms$nyears+1-0.5), col=liz.palette[1:asap$parms$nages] )

legend('top', horiz=T, legend=seq(1, asap$parms$nages), 
        pch=15, col=liz.palette[1:asap$parms$nages], cex=0.8 )

if (save.plots==T) savePlot(paste(od, 'SSB.AA.barplot.stacked.', plotf, sep=''), type=plotf)

#----same information, but proportion at age each year
barplot( t(ssb.aa/apply(ssb.aa,1,sum)), beside=F  ,  cex.names=0.75 ,  
           width=1, space=rep(0,(asap$parms$nyears)),
           xlab = 'Year', ylab ='ASAP Estimated Proportion (SSB.AA)', ylim = c(0,1.1), 
           xlim=c(0.5,asap$parms$nyears+1-0.5), col=liz.palette[1:asap$parms$nages] )

legend('top', horiz=T, legend=seq(1, asap$parms$nages), 
        pch=15, col=liz.palette[1:asap$parms$nages], cex=0.8 )

if (save.plots==T) savePlot(paste(od, 'SSB.AA.proportion.barplot.stacked.', plotf, sep=''), type=plotf)




}  #end funciton
#------------------------------------
plot.fleet.Fmult <- function(asap)  {

par(mfrow=c(1,1))

years<-  seq(asap$parms$styr, asap$parms$endyr)
nyears <- length(years)
nfleets = asap$parms$nfleets

Fmax<-max(asap$fleet.Fmult)

for (i in 1:nfleets) {

if (i==1) plot(years, asap$fleet.Fmult[i,], xlab="Year", ylab="Fmult by fleet", ylim=c(0,1.2*Fmax),
     type='l', lty=1, lwd=2)
if (i>1) lines(years, asap$fleet.Fmult[i,],lty=i, lwd=2, col=liz.palette[i] )
  }

leg.names <- fleet.names
legend('topleft', legend=leg.names, col=liz.palette[1:nfleets],lwd=rep(2, nfleets),
     lty=seq(1, nfleets), horiz=T, bty='n' ) 

if (save.plots==T) savePlot(paste(od, 'Fmult.by.fleet.', plotf, sep=''), type=plotf)

}   # end function


#------------------------------------
plot.cv <- function(asap, a1) {
par(mfrow=c(1,1), mar=c(4,4,2,2)  )

years = seq(asap$parms$styr, asap$parms$endyr)
asap.std <- a1$asap.std
F.rep = asap.std[asap.std$name=="Freport",3]  
cv.F.rep = asap.std[asap.std$name=="Freport",4]/F.rep
ssb.vec = asap.std[ asap.std$name=="SSB",3]
cv.ssb.vec = asap.std[ asap.std$name=="SSB",4]/ssb.vec
rec.vec = asap.std[ asap.std$name=="recruits", 3]
cv.rec.vec = asap.std[asap.std$name=="recruits", 4]/rec.vec


plot(years,   cv.rec.vec, type='l', lwd=2, col='black', xlab="Year", ylab="CV",
   ylim=c(0, 1.1*max(cv.rec.vec, cv.ssb.vec, cv.F.rep))  )
lines(years, cv.ssb.vec, lwd=2, col="blue")
lines(years, cv.F.rep, lwd=2, col="green3")
legend('top', legend=c("Recr.", "SSB", "Freport"), col=c("black", "blue", "green3"),
      lty=rep(1,3), lwd=rep(2,3), horiz=T)
      
if (save.plots==T) savePlot(paste(od, 'CV.estimates.', plotf, sep=''), type=plotf)

  }  # end function
#------------------------------------
plot.SARC.R.SSB <- function(asap)  {

par( mar = c(5,5,1,5), oma = c(0,0,0,1), family='serif')   

years = seq(asap$parms$styr, asap$parms$endyr)                       
nyears<-asap$parms$nyears
ssb <- asap$SSB
recr <- asap$N.age[,1]
ssb.plot<- ssb[1:(nyears-1)]
recr.plot<- recr[2:nyears]
yr.text=substr(years,3,4)

scale.ssb=1
scale.recruits=1

scale.r= max(ssb.plot/scale.ssb)/max(recr.plot/scale.recruits)
barplot( recr.plot/scale.recruits*scale.r , beside=T,axisnames=F , width=1, 
           space=rep(0,nyears-1), offset=rep(-0.5,nyears-1),  axes=F, xpd=F,
    xlab = '', ylab ='', ylim = c(0,1.1*max(recr.plot/scale.recruits*scale.r)), xlim=c(0.5,nyears-1.5), col="lightcyan2")
xr <-pretty(recr.plot/scale.recruits) 
axis(2, at = xr*scale.r, lab = xr )
axis(side=1, las=2, at=seq(0.5,asap$parms$nyears-1.5, by=2), 
    labels=as.character(seq(years[1],years[nyears-1], by=2)), cex=0.75, las=2)

lines(seq(0.5,nyears-1.5, by=1), ssb.plot/scale.ssb, lwd=2, col = 'navyblue')
x <- pretty(ssb.plot/scale.ssb)
axis(4, at = c(0,x), lab = c(0,x), col='navyblue', col.axis="navyblue")
box()


mtext(side = 1, 'Year', line = 3)
mtext(side = 4, paste('SSB (year)',sep=""), line = 3, col='navyblue')
mtext(side = 2, paste('Age-1 Recruits (year-class)', sep=""), line = 3)

if (save.plots==T) savePlot(paste(od, 'SARC.SSB.vs.R.barplots.', plotf, sep=''), type=plotf)
par(family="")
   }  # end function

#------------------------------------
#--------Data Plots------------------
plot.catch.by.fleet <- function(asap){
  nfleets <- asap$parms$nfleets
  barplot(asap$catch.obs,xlab="Year",ylab="Catch",ylim=c(0,1.1*max(apply(asap$catch.obs,2,sum))),col=liz.palette[1:nfleets],space=0)
   if (nfleets > 1) legend('top',legend=fleet.names,horiz=T,pch=15,col=liz.palette[1:nfleets])
 if (save.plots==T) savePlot(paste(od, 'catch.by.fleet.', plotf, sep=''), type=plotf)  
 
 # do proportions only if nfleets > 1
 if (nfleets > 1){
   catch.prop <- asap$catch.obs
   for (i in 1:length(catch.prop[1,])){
      catch.prop[,i] <- catch.prop[,i]/sum(catch.prop[,i])
   }
   barplot(catch.prop,xlab="Year",ylab="Proportion of Catch",ylim=c(0,1.1),col=liz.palette[1:nfleets],space=0)
     legend('top',legend=fleet.names,horiz=T,pch=15,col=liz.palette[1:nfleets])
 }
 if (save.plots==T) savePlot(paste(od, 'catch.proportions.by.fleet.', plotf, sep=''), type=plotf)  
}

#------------------------------------
# Bubble plots of catch age comps (set is.catch.flag to False to plot Discard age comps)
plot.catch.age.comp.bubbles <- function(asap,is.catch.flag=T){
  par(mar=c(4,4,2,2), oma=c(1,1,1,1), mfrow=c(1,1))

  ages=seq(1, asap$parms$nages)
  nages=length(ages)
  years=seq(asap$parms$styr, asap$parms$endyr)
  nyrs=asap$parms$nyears

  for (i in 1:asap$parms$nfleets) {
    acomp.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+1])
    catch.yrs <- which(asap$fleet.catch.Neff.init[i,]>0)
    my.title <- "Age Comps for Catch by Fleet "
    my.save <- "obs.catch.bubble.plots."
    if (!is.catch.flag){
       acomp.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+3])
       catch.yrs <- which(asap$fleet.discard.Neff.init[i,]>0)
       my.title <- "Age Comps for Discards by Fleet "
       my.save <- "obs.discard.bubble.plots."
    }
    if (length(catch.yrs)>0){
      scale.catch.obs <- 5
      z3 <- as.matrix(acomp.obs) * scale.catch.obs

      plot(ages, rev(ages),  xlim = c(1, nages), ylim = c(years[nyrs],(years[1]-2)), 
          xlab = "Age", ylab = "", type = "n", axes=F)
        axis(1, at= ages, lab=ages)
        axis(2, at = rev(years), lab = rev(years), cex.axis=0.75, las=1)
        box()
        abline(h=years, col="lightgray")
        segments(x0=seq(ages[1], nages), y0=rep(years[1],nages),
                 x1=seq(ages[1], nages), y1=rep(years[nyrs],nages), col = "lightgray", lty = 1)

        for (j in 1:nyrs){
           points(ages, rep((years[1]+j-1), nages), cex=z3[j,], col="black",
              bg = neg.resid.col, pch = 21)
        }
  
        bubble.legend1 <- c(0.05,0.2,0.4)
        bubble.legend2 <- bubble.legend1 * scale.catch.obs
        legend("topright", xpd=T, legend=bubble.legend1, pch=rep(21, 3), 
             pt.cex=bubble.legend2, horiz=T , col='black', pt.bg = neg.resid.col  )
        title (paste(my.title,i, " (", fleet.names[i], ")", sep=""), 
             outer=T, line=-1 ) 
        if (save.plots) savePlot(paste(od, my.save, i, ".", plotf, sep=''), type=plotf)
     } # end catch.yrs test  
  }   #end loop nfleets
}

#------------------------------------
plot.index.input <- function(asap){
  par(mfrow=c(2,1))
  years <- seq(asap$parms$styr,asap$parms$endyr)
  nyears <- length(years)
  indvals <- matrix(NA, nrow=nyears, ncol=asap$parms$nindices)
  for (i in 1:asap$parms$nindices){
    indvals[asap$index.year.counter[[i]],i] <- asap$index.obs[[i]]
  
  }
  # rescale to mean 1 and stdev 1
  rescaled <- indvals
  my.mean <- apply(indvals,2,mean, na.rm=T)
  my.std <- apply(indvals,2,sd, na.rm=T)
  for (i in 1:asap$parms$nindices){
     rescaled[,i] <- (indvals[,i] - my.mean[i]) / my.std[i]
  }  
  my.range <- range(rescaled, na.rm=T)
  plot(years,rescaled[,1],xlab="Year",ylab="Rescaled Indices",ylim=my.range,col=liz.palette[1],type='n')
  for (i in 1:asap$parms$nindices){
     lines(years,rescaled[,i],col=liz.palette[i])
  }
  # now repeat on log scale
  log.indvals <- log(indvals)
  log.rescaled <- log.indvals
  my.log.mean <- apply(log.indvals,2,mean, na.rm=T)
  my.log.std <- apply(log.indvals,2,sd, na.rm=T)
  for (i in 1:asap$parms$nindices){
     log.rescaled[,i] <- (log.indvals[,i] - my.log.mean[i]) / my.log.std[i]
  }
  my.log.range <- range(log.rescaled, na.rm=T)
  plot(years,log.rescaled[,1],xlab="Year",ylab="Rescaled Log(Indices)",ylim=my.log.range,col=liz.palette[1],type='n')
  for (i in 1:asap$parms$nindices){
     lines(years,log.rescaled[,i],col=liz.palette[i])
  }
  if (save.plots) savePlot(paste(od, "index.input.", plotf, sep=''), type=plotf)  
  par(mfrow=c(1,1))
}

#------------------------------------
# Bubble plots of index age comps
plot.index.age.comp.bubbles <- function(asap){
  par(mar=c(4,4,2,2), oma=c(1,1,1,1), mfrow=c(1,1))

  ages=seq(1, asap$parms$nages)
  nages=length(ages)
  years=seq(asap$parms$styr, asap$parms$endyr)
  nyrs=asap$parms$nyears

  for (i in 1:asap$parms$nindices) {
    acomp.obs <- as.data.frame(asap$index.comp.mats[2*i-1])
    index.yrs <- which(asap$index.Neff.init[i,]>0)
    my.title <- "Age Comps for Index "
    my.save <- "obs.index.bubble.plots."
    if (length(index.yrs)>0){
      scale.index.obs <- 5
      z3 <- as.matrix(acomp.obs) * scale.index.obs

      plot(ages, rev(ages),  xlim = c(1, nages), ylim = c(years[nyrs],(years[1]-2)), 
          xlab = "Age", ylab = "", type = "n", axes=F)
        axis(1, at= ages, lab=ages)
        axis(2, at = rev(years), lab = rev(years), cex.axis=0.75, las=1)
        box()
        abline(h=years, col="lightgray")
        segments(x0=seq(ages[1], nages), y0=rep(years[1],nages),
                 x1=seq(ages[1], nages), y1=rep(years[nyrs],nages), col = "lightgray", lty = 1)

        for (j in 1:nyrs){
           points(ages, rep((years[1]+j-1), nages), cex=z3[j,], col="black",
              bg = neg.resid.col, pch = 21)
        }
  
        bubble.legend1 <- c(0.05,0.2,0.4)
        bubble.legend2 <- bubble.legend1 * scale.index.obs
        legend("topright", xpd=T, legend=bubble.legend1, pch=rep(21, 3), 
             pt.cex=bubble.legend2, horiz=T , col='black', pt.bg = neg.resid.col  )
        title (paste(my.title,i, " (", fleet.names[i], ")", sep=""), 
             outer=T, line=-1 ) 
        if (save.plots) savePlot(paste(od, my.save, i, ".", plotf, sep=''), type=plotf)
     } # end index.yrs test  
  }   #end loop nfleets
}

#------------------------------------
plot.waa.matrices <- function(asap){
  years <- seq(asap$parms$styr,asap$parms$endyr)
  n.mats <- length(asap$WAA.mats)
  counter <- 1:n.mats
  waa.set <- rep(NA,n.mats)
  waa.set[1] <- 1
  for (i in 2:n.mats){
    is.new <- T
    for (j in 1:(i-1)){
      if(sum(abs(asap$WAA.mats[[i]]-asap$WAA.mats[[j]]))==0){
        waa.set[i] = waa.set[j]
        is.new <- F
      }  
    }
    if (is.new) waa.set[i] <- max(waa.set, na.rm=T) + 1
  } # end i-loop over n.mats
  
  n.unique <- unique(waa.set)
  for (k in 1:length(n.unique)){
    kk <- min(counter[waa.set==k])
    WAA.plot <- asap$WAA.mats[[kk]]
    plot(years,years,xlab="Year",ylab="Weight",ylim=c(0,1.1*max(WAA.plot)),type='n')
    for (i in 1:asap$parms$nages){
      lines(years,WAA.plot[,i],col=liz.palette[i],lwd=2)
      lines(years,rep(mean(WAA.plot[,i]),length(years)),lty=2,col=liz.palette[i])
    }
    legend('top',legend=names(asap$WAA.mats[counter[waa.set==k]]),cex=0.7,horiz=T)
    title(main=paste("WAA matrix ",k, sep=""), outer=F)
    if (save.plots) savePlot(paste(od, "waa.matrix.", k, ".", plotf, sep=''), type=plotf)
  }  # end k-loop
  
}  # end function

#------------------------------------
plot.M <- function(asap){
  ages <- seq(1,asap$parms$nages)
  meanM <- apply(asap$M,2,mean)
  nyears <- asap$parms$nyears
  yr.col <- rainbow(nyears, start = 0, end = 0.75)

  
  plot(ages,meanM,type='l',lwd=2,xlab="Age",ylab="Natural Mortality Rate",
            ylim=c(0,1.1*max(asap$M)))
  
  if (length(unique(asap$M.age)) > asap$parms$nages) {         
  for (i in 1:length(asap$M[,1])){
    points(jitter(ages, factor=0.25), asap$M[i,],col=yr.col[i])
  }
  legend('top', horiz=T, legend=c("First Year", "Last Year"), pch=c(1,1),
         col=c(yr.col[1], yr.col[nyears])  )
         } # end if-test for time varying M
  title(main="M", outer=F)
  if (save.plots) savePlot(paste(od, "M.", plotf, sep=''), type=plotf)
}

#------------------------------------
plot.maturity <- function(asap){
  ages <- seq(1,asap$parms$nages)
  meanmaturity <- apply(asap$maturity,2,mean)
  nyears <- asap$parms$nyears
  yr.col <- rainbow(nyears, start = 0, end = 0.75)
  
  plot(ages,meanmaturity,type='l',lwd=2,xlab="Age",ylab="Maturity",ylim=c(0,max(asap$maturity)))
    if (length(unique(asap$maturity)) > asap$parms$nages) {         
  for (i in 1:length(asap$maturity[,1])){
    points(jitter(ages, factor=0.25),asap$maturity[i,],col=yr.col[i] )
  }
  legend('topleft', horiz=F, legend=c("First Year", "Last Year"), pch=c(1,1),
         col=c(yr.col[1], yr.col[nyears])  )
          } # end if-test for time-varying maturity
  title(main="Maturity", outer=F)
  if (save.plots) savePlot(paste(od, "maturity.", plotf, sep=''), type=plotf)
}

#------------------------------------
#------------------------------------
#------------------------------------
#------------------------------------
#------------------------------------
#--------Reference Point plots-------
#asap$WAA.mats$WAA.ssb  use this for SPR
# ... how to combine WAA matrices for YPR

#-------Spawners per recruit -----------------------------
s.per.recr<-function(nages,fec.age,mat.age,M.age, F.mult, sel.age, spawn.time ) {

spr=0.0
cum.survive=1.0
z=0.0
for (i in 1:(nages-1)  ) {
  z=M.age[i] + F.mult*sel.age[i]
  z.ts=(M.age[i]+F.mult*sel.age[i])*spawn.time
  spr=spr+cum.survive*fec.age[i]*mat.age[i]*exp(-z.ts)
  cum.survive=cum.survive*exp(-z )

  }

  z= M.age[nages] + F.mult*sel.age[nages]
  z.ts=(M.age[nages]+F.mult*sel.age[nages])*spawn.time
  spr=spr + fec.age[nages]*mat.age[nages]*cum.survive*exp(-z.ts)/( 1- exp(-z ) )

  return(spr)

  }

#-------Yield per recruit -----------------------------
ypr<-function(nages, wgt.age, M.age, F.mult, sel.age ) {

yield=0.0
cum.survive=1.0
z=0.0

for (i in 1:(nages-1)  ) {
  z=M.age[i] + F.mult*sel.age[i]
  yield=yield + wgt.age[i]*F.mult*sel.age[i]*(1-exp(-z) )*cum.survive/z
  cum.survive=cum.survive*exp(-z)
  }

  z= M.age[nages] + F.mult*sel.age[nages]
  yield=yield + wgt.age[nages]*F.mult*sel.age[nages]*cum.survive/z

  return(yield)

  }
#------------------------------------
#-------Equilibrium SSB ------------------------



ssb.eq<-function(recr.par, R0.BH, spr, spr0, is.steepness=T ) {

if (is.steepness==T)  alpha.BH <- 4*recr.par/(1-recr.par)
if (is.steepness==F)  alpha.BH <- recr.par
sprF=spr/spr0

ssb=spr0*R0.BH*(sprF*alpha.BH - 1.0)/(alpha.BH-1.0)


  return(ssb)

  }
#------------------------------------  
 bev.holt.alpha<-function(S,R0,recr.par,spr0, is.steepness=T){

 if (is.steepness==T)  alpha.BH <- 4*recr.par/(1-recr.par)
if (is.steepness==F)  alpha.BH <- recr.par


  y=rep(0,length(S))
  y=R0*S*alpha.BH/(R0*spr0 +(alpha.BH-1.0)*S)
  return(y)

}  
##----  Calculate Fmsy, MSY given selectivity and SR relationship  ------------------------------


plot.MSY.annual <- function(asap) {

h.vals <- asap$SR.annual.parms$steepness.vec
recr.par <- h.vals


if (asap$control.parms$phases$phase.steepness>0 & max(h.vals)<1 ) { # test to make sure steepness was estimated

  nages<- asap$parms$nages
  nyears <- asap$parms$nyears
  years <- seq(asap$parms$styr,asap$parms$endyr)  
  fec.age <- asap$WAA.mats$WAA.ssb
  mat.age <- asap$maturity
  wgt.age <- asap$WAA.mats$WAA.catch.all
  M.age <- asap$M.age
  sel.mat <- asap$F.age/apply(asap$F.age,1,max)

  spawn.time <- asap$options$frac.yr.spawn



  ahat.vals <- 4*h.vals/(1-h.vals)
  R0.vals <- asap$SR.annual.parms$R0.vec
  sr0.vals <- asap$SR.annual.parms$s.per.r.vec
  MSY.soln <- matrix(NA, nrow=9, ncol=nyears)
  rownames(MSY.soln) <- c("Fmsy", "MSY", "SPRmsy", "SSBmsy", "Rmsy", "YPRmsy",
                         "Rel.SSBmsy", "Rel.Rmsy", "Conv.Code")
  colnames(MSY.soln) <-  seq(asap$parms$styr, asap$parms$endyr)

for (i in 1:nyears) { 
     F.start=0.25
   get.yield.f.min <- function(F.start) {
    temp.spr = s.per.recr(nages=nages, fec.age=fec.age[i,], mat.age=mat.age[i,], M.age= M.age[i,], F.mult=F.start, sel.age=sel.mat[i,], spawn.time=spawn.time)   
    temp.ypr = ypr(nages=nages, wgt.age=wgt.age[i,], M.age=M.age[i,],  F.mult=F.start, sel.age=sel.mat[i,] )
    temp.SSB = ssb.eq( recr.par=ahat.vals[i], R0.BH=R0.vals[i], spr=temp.spr, spr0=sr0.vals[i], is.steepness=F )
    yield = temp.ypr*temp.SSB/temp.spr           #harvest in weight
    yy=-1*yield
     return(yy)  
              }   # end get.yield.f.min function

F.nlmin <- nlminb( start=F.start , objective=get.yield.f.min, lower=0.01, upper=3.0,
              control=list(eval.max=500, iter.max=200  )   )



MSY.soln[1, i] <- F.nlmin$par  #Fmsy
MSY.soln[2, i] <- -1*F.nlmin$objective #MSY

# calculate other MSY quantities
spr.nlmin <- s.per.recr(nages=nages, fec.age=fec.age[i,], mat.age=mat.age[i,], M.age= M.age[i,], F.mult=F.nlmin$par, sel.age=sel.mat[i,], spawn.time=spawn.time)   
MSY.soln[3, i] <- spr.nlmin/sr0.vals[i] #SPRmsy

ssb.nlmin <- ssb.eq( recr.par=ahat.vals[i], R0.BH=R0.vals[i], spr=spr.nlmin, spr0=sr0.vals[i], is.steepness=F )
MSY.soln[4, i] <-  ssb.nlmin #SSBmsy


Rmsy.nlmin <- bev.holt.alpha(S=ssb.nlmin,R0=R0.vals[i],recr.par=ahat.vals[i],spr0=sr0.vals[i])
MSY.soln[5,i ] <- Rmsy.nlmin #Rmsy

ypr.nlmin <- ypr(nages=nages, wgt.age=wgt.age[i,], M.age=M.age[i,],  F.mult=F.nlmin$par, sel.age=sel.mat[i,] )
MSY.soln[6,i ] <- ypr.nlmin #YPRmsy

rel.ssb.nlmin <- ssb.nlmin/(R0.vals[i]*sr0.vals[i])
MSY.soln[7, i] <-  rel.ssb.nlmin #SSBmsy/SSB0

MSY.soln[8, i] <- Rmsy.nlmin/R0.vals[i] #Rmsy/R0

MSY.soln[9, i] <-  F.nlmin$convergence  #code=0 indicates convergence
          }  # end i-loop over nyears
          
 f.hist <- hist(MSY.soln[1,], plot=F)
 MSY.hist <- hist(MSY.soln[2,], plot=F)
 R.hist <- hist(MSY.soln[5,], plot=F ) 
 S.hist <- hist(MSY.soln[4,], plot=F ) 

 h.hist <- hist(h.vals, plot=F)
 SPR.hist <- hist(MSY.soln[3,], plot=F)
 R0.hist <- hist(R0.vals, plot=F ) 
 S0.hist <- hist(asap$SR.annual.parms$S0.vec, plot=F ) 

 par(mfrow=c(2,2), mar=c(4,4,2,2), oma=c(0,0,2,0) )

plot(f.hist$mids, f.hist$counts, xlab="Full F", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(f.hist$counts))  )
lines(f.hist$mids, f.hist$counts, lwd=2)
plot(S.hist$mids, S.hist$counts, xlab="SSB.MSY", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(S.hist$counts))  )
lines(S.hist$mids, S.hist$counts, lwd=2)
plot(MSY.hist$mids, MSY.hist$counts, xlab="MSY", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(MSY.hist$counts)) ) 
lines(MSY.hist$mids, MSY.hist$counts, lwd=2)
plot(R.hist$mids, R.hist$counts, xlab="R.MSY", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(R.hist$counts))  )
lines(R.hist$mids, R.hist$counts, lwd=2)
title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
             outer=T, line=-1 ) 
if (save.plots) savePlot(paste(od, "MSY.4panel.hist1.", plotf, sep=''), type=plotf)  


plot(h.hist$mids, h.hist$counts, xlab="steepness", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(h.hist$counts))  )
lines(h.hist$mids, h.hist$counts, lwd=2)
plot(S0.hist$mids, S0.hist$counts, xlab="SSB0", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(S0.hist$counts))  )
lines(S0.hist$mids, S0.hist$counts, lwd=2)
plot(SPR.hist$mids, SPR.hist$counts, xlab="SPR.MSY", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(SPR.hist$counts)) )
lines(SPR.hist$mids, SPR.hist$counts, lwd=2)
plot(R0.hist$mids, R0.hist$counts, xlab="R0", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(R0.hist$counts))  )
lines(R0.hist$mids, R0.hist$counts, lwd=2)
title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
             outer=T, line=-1 ) 
if (save.plots) savePlot(paste(od, "MSY.4panel.hist2.", plotf, sep=''), type=plotf)  


par(mfrow=c(1,1), mar=c(4,4,2,2) )

h.order <- order(h.vals)
steep.spr <- cbind(h.vals[h.order], MSY.soln[3,h.order])

plot(steep.spr[,1], steep.spr[,2], xlab="Steepness", ylab="SPR.MSY", xlim=c(0.2,1), ylim=c(0,1),
       type='p', pch=16, col='blue3' )
points(h.vals[1], MSY.soln[3,1], pch=16, cex=1.5, col="green3")       
points(h.vals[nyears], MSY.soln[3,nyears], pch=16, cex=1.5, col="orange2")       
legend('top', legend=c("First Year", "Last Year"), pch=c(16,16), col=c("green3", "orange2") )
title (paste("Annual Steepness and SPR.MSY (from S-R curve)", sep=""), 
             outer=T, line=-1 ) 

if (save.plots) savePlot(paste(od, "MSY.h.vs.SPR.", plotf, sep=''), type=plotf)         

plot(years, MSY.soln[1,], xlab="Year", ylab="Full F at MSY", ylim=1.1*c(0, max(MSY.soln[1,])), type='l', lwd=2  ) 
points(years, MSY.soln[1,], pch=10)
title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
             outer=T, line=-1 ) 

if (save.plots) savePlot(paste(od, "Annual.Fmsy.", plotf, sep=''), type=plotf)  

plot(years, MSY.soln[2,], xlab="Year", ylab="MSY", ylim=1.1*c(0, max(MSY.soln[2,])), type='l', lwd=2  ) 
points(years, MSY.soln[2,], pch=10)
title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
             outer=T, line=-1 ) 

if (save.plots) savePlot(paste(od, "Annual.MSY.", plotf, sep=''), type=plotf)  

plot(years, MSY.soln[3,], xlab="Year", ylab="%SPR at MSY", ylim=1.1*c(0, 1), type='l', lwd=2  ) 
points(years, MSY.soln[3,], pch=10)
title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
             outer=T, line=-1 ) 

if (save.plots) savePlot(paste(od, "Annual.SPR.MSY.", plotf, sep=''), type=plotf)  

plot(years, MSY.soln[4,], xlab="Year", ylab="SSB at MSY", ylim=1.1*c(0, max(MSY.soln[4,])), type='l', lwd=2  ) 
points(years, MSY.soln[4,], pch=10)
title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
             outer=T, line=-1 ) 

if (save.plots) savePlot(paste(od, "Annual.SSB.MSY.", plotf, sep=''), type=plotf) 

plot(years, MSY.soln[5,], xlab="Year", ylab="Recruitment at MSY", ylim=1.1*c(0, max(MSY.soln[5,])), type='l', lwd=2  ) 
points(years, MSY.soln[5,], pch=10)
title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
             outer=T, line=-1 ) 

if (save.plots) savePlot(paste(od, "Annual.Recr.MSY.", plotf, sep=''), type=plotf) 

frep1 <-asap$options$Freport.agemin
frep2 <-asap$options$Freport.agemax
if (frep1==frep2) freport <-MSY.soln[1,frep1:frep2]*sel.mat[,frep1:frep2]
if (frep2>frep1) freport <-apply(MSY.soln[1,frep1:frep2]*sel.mat[,frep1:frep2],1,mean)
freport.label <- paste("Freport_",frep1,"-",frep2,sep="")
MSY.soln<-  rbind(MSY.soln, freport)
rownames(MSY.soln) <- 
c("Fmsy", "MSY", "SPRmsy", "SSBmsy", "Rmsy", "YPRmsy",
                         "Rel.SSBmsy", "Rel.Rmsy", "Conv.Code",paste("Freport_",frep1,"-",frep2,sep=""))

write.csv( t(MSY.soln), file=paste(od, "MSY.soln.values.csv", sep=""), row.names=T )
 return(MSY.soln) 


        } # end test for steepness phase>0


   }  # end function
  
  
###-------------------------------------------------------------------------------------
#------------------------------------
plot.SPR.table <- function(asap, nyrs.ave) {

spr.targ.values <- seq(0.2, 0.8, by=0.05)
n.spr <- length(spr.targ.values)
nages<- asap$parms$nages
years <- seq(asap$parms$styr,asap$parms$endyr)  
nyears <- asap$parms$nyears
fec.age <- apply(asap$WAA.mats$WAA.ssb[(nyears-nyrs.ave+1):nyears,],2,mean)
mat.age <- apply(asap$maturity[(nyears-nyrs.ave+1):nyears,],2,mean)
wgt.age <- apply(asap$WAA.mats$WAA.catch.all[(nyears-nyrs.ave+1):nyears,],2,mean)
M.age <- apply(asap$M.age[(nyears-nyrs.ave+1):nyears,],2,mean)
sel.mat <- asap$F.age[(nyears-nyrs.ave+1):nyears,]/apply(asap$F.age[(nyears-nyrs.ave+1):nyears,],1,max)
sel.age1<- apply(sel.mat,2,mean) 
sel.age <- sel.age1/max(sel.age1)
spawn.time <- asap$options$frac.yr.spawn

spr0<- s.per.recr(nages=nages, fec.age=fec.age, mat.age=mat.age, M.age= M.age, F.mult=0, sel.age=sel.age, spawn.time=spawn.time)
F.start <-0.11  # starting guess for optimization routine to find F_SPR%

f.spr.vals <- rep(NA, n.spr)
ypr.spr.vals <- rep(NA, n.spr)
conv.vals <- rep(NA, n.spr)

for (i in 1:n.spr) {
   t.spr <- spr.targ.values[i]

   spr.f <- function(F.start) {
           abs(s.per.recr(nages=nages, fec.age=fec.age, mat.age=mat.age, M.age= M.age, F.mult=F.start, sel.age=sel.age, spawn.time=spawn.time)/spr0 - t.spr )
              }
    yyy <- nlminb(start=F.start, objective=spr.f, lower=0, upper=3)
    f.spr.vals[i] <- yyy$par
    ypr.spr.vals[i] <- ypr(nages, wgt.age=wgt.age, M.age=M.age,  F.mult=f.spr.vals[i], sel.age=sel.age )
       
   }  #end i-loop over SPR values

par(mfrow=c(1,1), mar=c(4,4,2,4) )

plot(spr.targ.values, ypr.spr.vals, type='n', xlab="% SPR Target", ylab="Yield per Recruit", lwd=2,
     col="blue3", ylim=c(0,1.2*max(ypr.spr.vals)) )
abline(v=seq(0.2,0.8, by=0.05), col="grey85")
lines(spr.targ.values, ypr.spr.vals, lwd=2, col="blue3" )       
points(spr.targ.values, ypr.spr.vals, pch=19, col="blue3" )  
scale.f.spr <- max(f.spr.vals)/max(ypr.spr.vals)

lines(spr.targ.values, f.spr.vals/scale.f.spr, col="red", lwd=2 )     
axis(side=4, at=seq(0,1,by=0.1)/scale.f.spr, lab=seq(0,1,by=0.1), las=2,
    col='black', col.axis="black") 
mtext(side=4, "F (%SPR)", line=3, col="red")

title (paste("SPR Target Reference Points (Years Avg = ", nyrs.ave,")", sep=""), 
             outer=T, line=-1 ) 

  if (save.plots) savePlot(paste(od, "SPR.Target.Curves.", plotf, sep=''), type=plotf)

par(mfrow=c(1,1), mar=c(2,2,2,2))
plot(seq(1,15), seq(1,15), type='n', axes=F, bty='n',xlab="",ylab="")

text(x=2,y=14, labels="% SPR", font=2, pos=4)
text(x=5, y=14, labels="F(%SPR)" , font=2, pos=4)
text(x=9, y=14, labels="YPR", font=2, pos=4 )
for (i in 1:n.spr) {
   text(x=2, y=seq(n.spr,1, by=-1), labels=round(spr.targ.values,2), cex=1.0, pos=4, font=1)
   text(x=5, y=seq(n.spr,1, by=-1), labels=round(f.spr.vals,4), cex=1.0, pos=4, font=1)
   text(x=9, y=seq(n.spr,1, by=-1), labels=round(ypr.spr.vals,4), cex=1.0, pos=4, font=1)   
    }
title (paste("SPR Target Reference Points (Years Avg = ", nyrs.ave,")", sep=""), 
             outer=T, line=-1 ) 

  if (save.plots) savePlot(paste(od, "SPR.Target.Table.", plotf, sep=''), type=plotf)

frep1 <-asap$options$Freport.agemin
frep2 <-asap$options$Freport.agemax
if (frep1==frep2) freport <-f.spr.vals*sel.age[frep1:frep2]
if (frep2>frep1) freport <-f.spr.vals*mean(sel.age[frep1:frep2])

spr.target.table<- as.data.frame(cbind(spr.targ.values, f.spr.vals, ypr.spr.vals, freport))
colnames(spr.target.table) <- c("%SPR", "F(%SPR)", "YPR", paste("Freport_",frep1,"-",frep2,sep=""))

write.csv( spr.target.table, file=paste(od,"SPR.Target.Table.csv", sep=""),  row.names=F )

} # end function
#------------------------------------
plot.annual.SPR.targets <- function(asap) {

spr.targ.values <- seq(0.2, 0.5, by=0.1)
n.spr <- length(spr.targ.values)
nages<- asap$parms$nages
nyears <- asap$parms$nyears
years <- seq(asap$parms$styr,asap$parms$endyr)  
fec.age <- asap$WAA.mats$WAA.ssb
mat.age <- asap$maturity
wgt.age <- asap$WAA.mats$WAA.catch.all
M.age <- asap$M.age
sel.age <- asap$F.age/apply(asap$F.age,1,max)

spawn.time <- asap$options$frac.yr.spawn

spr0.vals<- asap$SR.annual.parms$s.per.r.vec
F.start <-0.11  # starting guess for optimization routine to find F_SPR%

f.spr.vals <- matrix(NA, nyears, n.spr)
ypr.spr.vals <- matrix(NA, nyears, n.spr)
conv.vals <- matrix(NA, nyears, n.spr)

for (i in 1:n.spr) {
   for (j in 1:nyears) {
   t.spr <- spr.targ.values[i]

   spr.f <- function(F.start) {
           abs(s.per.recr(nages=nages, fec.age=fec.age[j,], mat.age=mat.age[j,], M.age= M.age[j,], F.mult=F.start, sel.age=sel.age[j,], spawn.time=spawn.time)/spr0.vals[j] - t.spr )
              }
    yyy <- nlminb(start=F.start, objective=spr.f, lower=0, upper=3)
    f.spr.vals[j,i] <- yyy$par
    ypr.spr.vals[j,i] <- ypr(nages, wgt.age=wgt.age[j,], M.age=M.age[j,],  F.mult=f.spr.vals[j,i], sel.age=sel.age[j,] )
       
       }  # end j-loop over nyears
   }  #end i-loop over SPR values

par(mfrow=c(1,1), mar=c(4,4,2,4) )
  lty.seq=c(1,2,4,6)
plot(years, f.spr.vals[,1], type='n', xlab="Years", ylab="Full F (%SPR)", lwd=2,
     col="blue3", ylim=c(0,1.2*max(f.spr.vals)) )
  for (i in 1:n.spr) {
lines(years, f.spr.vals[,i], lwd=2, col=i, lty=lty.seq[i] )     
}
legend('top', legend=c("F20%", "F30%", "F40%", "F50%"), col=seq(1,4), lty=lty.seq, 
     horiz=T,lwd=rep(2,4), cex=0.9)
 
title (main="Annual F(%SPR) Reference Points", outer=T, line=-1 ) 

  if (save.plots) savePlot(paste(od, "Annual.FSPR.", plotf, sep=''), type=plotf)


plot(years, ypr.spr.vals[,1], type='n', xlab="Years", ylab="YPR (%SPR)", lwd=2,
     col="blue3", ylim=c(0,1.2*max(ypr.spr.vals)) )
  for (i in 1:n.spr) {
lines(years, ypr.spr.vals[,i], lwd=2, col=i, lty=lty.seq[i] )     
}
legend('top', legend=c("YPR20%", "YPR30%", "YPR40%", "YPR50%"), col=seq(1,4), lty=lty.seq, 
     horiz=T,lwd=rep(2,4), cex=0.9)
 
title (main="Annual YPR(%SPR) Reference Points", outer=T, line=-1 ) 

  if (save.plots) savePlot(paste(od, "Annual.YPR.", plotf, sep=''), type=plotf)

 f.hist.20 <- hist(f.spr.vals[,1], plot=F)
 f.hist.30 <- hist(f.spr.vals[,2], plot=F)
 f.hist.40 <- hist(f.spr.vals[,3], plot=F)
 f.hist.50 <- hist(f.spr.vals[,4], plot=F)
    
 par(mfrow=c(2,2), mar=c(4,4,2,2), oma=c(0,0,2,0) )

plot(f.hist.20$mids, f.hist.20$counts, xlab="Full F20%", ylab="Frequency", type='h', lwd=2, 
     ylim=c(0, max(f.hist.20$counts)), col='blue4'  )
lines(f.hist.20$mids, f.hist.20$counts, lwd=2, col='blue4')
plot(f.hist.30$mids, f.hist.30$counts, xlab="Full F30%", ylab="Frequency", type='h', lwd=2, 
     ylim=c(0, max(f.hist.30$counts)), col='blue4'  )
lines(f.hist.30$mids, f.hist.30$counts, lwd=2, col='blue4')
plot(f.hist.40$mids, f.hist.40$counts, xlab="Full F40%", ylab="Frequency", type='h', lwd=2, 
     ylim=c(0, max(f.hist.40$counts)), col='blue4'  )
lines(f.hist.40$mids, f.hist.40$counts, lwd=2, col='blue4')
plot(f.hist.50$mids, f.hist.50$counts, xlab="Full F50%", ylab="Frequency", type='h', lwd=2, 
     ylim=c(0, max(f.hist.50$counts)), col='blue4'  )
lines(f.hist.50$mids, f.hist.50$counts, lwd=2, col='blue4')

title (main="Annual F (%SPR) Reference Points", outer=T, line=-1 ) 

if (save.plots) savePlot(paste(od, "Annual.F_SPR.4panel.hist.", plotf, sep=''), type=plotf)  


 ypr.hist.20 <- hist(ypr.spr.vals[,1], plot=F)
 ypr.hist.30 <- hist(ypr.spr.vals[,2], plot=F)
 ypr.hist.40 <- hist(ypr.spr.vals[,3], plot=F)
 ypr.hist.50 <- hist(ypr.spr.vals[,4], plot=F)
    
 par(mfrow=c(2,2), mar=c(4,4,2,2), oma=c(0,0,2,0) )

plot(ypr.hist.20$mids, ypr.hist.20$counts, xlab="YPR (F20%)", ylab="Frequency", type='h', lwd=2, 
     ylim=c(0, max(ypr.hist.20$counts)), col='blue4'  )
lines(ypr.hist.20$mids, ypr.hist.20$counts, lwd=2, col='blue4')
plot(ypr.hist.30$mids, ypr.hist.30$counts, xlab="YPR (F30%)", ylab="Frequency", type='h', lwd=2, 
     ylim=c(0, max(ypr.hist.30$counts)), col='blue4'  )
lines(ypr.hist.30$mids, ypr.hist.30$counts, lwd=2, col='blue4')
plot(ypr.hist.40$mids, ypr.hist.40$counts, xlab="YPR (F40%)", ylab="Frequency", type='h', lwd=2, 
     ylim=c(0, max(ypr.hist.40$counts)), col='blue4'  )
lines(ypr.hist.40$mids, ypr.hist.40$counts, lwd=2, col='blue4')
plot(ypr.hist.50$mids, ypr.hist.50$counts, xlab="YPR (F50%)", ylab="Frequency", type='h', lwd=2, 
     ylim=c(0, max(ypr.hist.50$counts)), col='blue4'  )
lines(ypr.hist.50$mids, ypr.hist.50$counts, lwd=2, col='blue4')

title (main="Annual YPR (%SPR) Reference Points", outer=T, line=-1 ) 

if (save.plots) savePlot(paste(od, "Annual.YPR_SPR.4panel.hist.", plotf, sep=''), type=plotf)  


} # end function

#------------------------------------
plot.yield.curves <- function(asap, nyrs.ave) {

nages<- asap$parms$nages
nyears <- asap$parms$nyears
fec.age <- apply(asap$WAA.mats$WAA.ssb[(nyears-nyrs.ave+1):nyears,],2,mean)
mat.age <- apply(asap$maturity[(nyears-nyrs.ave+1):nyears,],2,mean)
wgt.age <- apply(asap$WAA.mats$WAA.catch.all[(nyears-nyrs.ave+1):nyears,],2,mean)
M.age <- apply(asap$M.age[(nyears-nyrs.ave+1):nyears,],2,mean)
sel.mat <- asap$F.age[(nyears-nyrs.ave+1):nyears,]/apply(asap$F.age[(nyears-nyrs.ave+1):nyears,],1,max)
sel.age<- apply(sel.mat,2,mean) 
spawn.time <- asap$options$frac.yr.spawn

F.range <- seq(0,2.0, by=0.01)
nF <- length(F.range)
ypr.vec <- rep(0, nF)
spr.vec <- rep(0, nF)

spr0<- s.per.recr(nages=nages, fec.age=fec.age, mat.age=mat.age, M.age= M.age, F.mult=0, sel.age=sel.age, spawn.time=spawn.time)

for(j in 1:nF) {
    
    spawn.per.recr = s.per.recr(nages=nages, fec.age=fec.age, mat.age=mat.age, M.age= M.age, F.mult=F.range[j], sel.age=sel.age, spawn.time=spawn.time)
    spr.vec[j] = spawn.per.recr/spr0
    ypr.vec[j] = ypr(nages, wgt.age=wgt.age, M.age=M.age,  F.mult=F.range[j], sel.age=sel.age )
      } # end loop over F

par(mfrow=c(1,1), mar=c(4,4,2,4) )

plot(F.range, ypr.vec, type='n', xlab="Full F", ylab="Yield per Recruit", lwd=2,
     col="blue3", ylim=c(0,1.2*max(ypr.vec)))
abline(v=seq(0.1,2.0, by=0.1), col="grey85")
lines(F.range, ypr.vec, lwd=2, col="blue3" )       
points(F.range, ypr.vec, pch=19, col="blue3" )  
scale.spr.vec <- max(spr.vec)/max(ypr.vec)

lines(F.range, spr.vec/scale.spr.vec, col="red", lwd=2 )     
axis(side=4, at=seq(0,1,by=0.1)/scale.spr.vec, lab=seq(0,1,by=0.1), las=2,
    col='black', col.axis="black") 
mtext(side=4, "% SPR", line=3, col="red")
title (paste("YPR-SPR Reference Points (Years Avg = ", nyrs.ave,")", sep=""), 
             outer=T, line=-1 ) 

  if (save.plots) savePlot(paste(od, "YPR.SPR.Curves.", plotf, sep=''), type=plotf)

ypr.table <- as.data.frame(matrix(NA, nrow=nF, ncol=3) )
ypr.table[,1] <- F.range
ypr.table[,2] <- ypr.vec
ypr.table[,3] <- spr.vec

par(mfrow=c(1,1), mar=c(2,2,2,2))
plot(seq(1,42), seq(-3,38), type='n', axes=F, bty='n',xlab="",ylab="")


text(x=0,y=36, labels="F", font=2, pos=4)
text(x=4, y=36, labels="YPR" , font=2, pos=4)
text(x=9, y=36, labels="SPR", font=2, pos=4 )

text(x=15,y=36, labels="F", font=2, pos=4)
text(x=19, y=36, labels="YPR" , font=2, pos=4)
text(x=24, y=36, labels="SPR", font=2, pos=4 )

text(x=30,y=36, labels="F", font=2, pos=4)
text(x=34, y=36, labels="YPR" , font=2, pos=4)
text(x=39, y=36, labels="SPR", font=2, pos=4 )

for (i in 1:35) {
   text(x=0, y=seq(35,1, by=-1), labels=F.range[1:35], cex=0.82, pos=4, font=1 )
   text(x=4, y=seq(35,1, by=-1), labels=round(ypr.vec[1:35],4), cex=0.82, pos=4, font=1 )
   text(x=9, y=seq(35,1, by=-1), labels=round(spr.vec[1:35],4), cex=0.82, pos=4, font=1 )   
    }
for (i in 36:70) {
   text(x=15, y=seq(35,1, by=-1), labels=F.range[36:70], cex=0.82, pos=4, font=1)
   text(x=19, y=seq(35,1, by=-1), labels=round(ypr.vec[36:70],4), cex=0.82, pos=4, font=1)
   text(x=24, y=seq(35,1, by=-1), labels=round(spr.vec[36:70],4), cex=0.82, pos=4, font=1)   
    }
for (i in 71:105) {
   text(x=30, y=seq(35,1, by=-1), labels=F.range[71:105], cex=0.82, pos=4, font=1)
   text(x=34, y=seq(35,1, by=-1), labels=round(ypr.vec[71:105],4), cex=0.82, pos=4, font=1)
   text(x=39, y=seq(35,1, by=-1), labels=round(spr.vec[71:105],4), cex=0.82, pos=4, font=1)   
    }
title (paste("YPR-SPR Reference Points (Years Avg = ", nyrs.ave,")", sep=""), 
             outer=T, line=-1 ) 
    
  if (save.plots) savePlot(paste(od, "YPR.SPR.Table.", plotf, sep=''), type=plotf)   

frep1 <-asap$options$Freport.agemin
frep2 <-asap$options$Freport.agemax
if (frep1==frep2) freport <-F.range*sel.age[frep1:frep2]
if (frep2>frep1) freport <-F.range*mean(sel.age[frep1:frep2])

  
ypr.table<- as.data.frame(cbind(F.range, ypr.vec, spr.vec, spr.vec*spr0,freport))
colnames(ypr.table) <- c("Full.F", "YPR", "SPR", "SSBPR",paste("Freport_",frep1,"-",frep2,sep=""))
write.csv( ypr.table, file=paste(od,"YPR.Table.csv", sep=""), row.names=F )

  
} # end function                                   

#------------------------------------
#------------------------------------
plot.exp.spawn <- function(asap, nyrs.ave) {

nages<- asap$parms$nages
nyears <- asap$parms$nyears
fec.age <- apply(asap$WAA.mats$WAA.ssb[(nyears-nyrs.ave+1):nyears,],2,mean)
mat.age <- apply(asap$maturity[(nyears-nyrs.ave+1):nyears,],2,mean)
wgt.age <- apply(asap$WAA.mats$WAA.catch.all[(nyears-nyrs.ave+1):nyears,],2,mean)
M.age <- apply(asap$M.age[(nyears-nyrs.ave+1):nyears,],2,mean)
sel.mat <- asap$F.age[(nyears-nyrs.ave+1):nyears,]/apply(asap$F.age[(nyears-nyrs.ave+1):nyears,],1,max)
sel.age<- apply(sel.mat,2,mean)
spawn.time <- asap$options$frac.yr.spawn

F.range <- seq(0,2.0, by=0.01)
nF <- length(F.range)
exp.spawn.vec <- rep(0, nF)
spr.vec <- rep(0, nF)

spr0<- s.per.recr(nages=nages, fec.age=fec.age, mat.age=mat.age, M.age= M.age, F.mult=0, sel.age=sel.age, spawn.time=spawn.time)
spawn0<- s.per.recr(nages=nages, fec.age=rep(1,nages), mat.age=mat.age, M.age= M.age, F.mult=0, sel.age=sel.age, spawn.time=spawn.time)
# plot maturity vs selectivity based on "nyrs.ave" average

par(mfrow=c(1,1), mar=c(4,4,2,4) )
plot( seq(1,nages), mat.age, type='l', col='black', lwd=2, ylim=c(0,1.1),
     xlab="Age", ylab="Selectivity or Maturity at age")
lines( seq(1,nages), sel.age, col='orange2', lwd=1)
points( seq(1,nages), sel.age, col='orange2', pch=16)
legend('topleft', legend=c("Maturity", "Selectivity"), col=c("black", "orange2"),
    lwd=c(2,1), pch=c(NA, 16) )
  if (save.plots) savePlot(paste(od, "Selectivity.vs.Maturity.", plotf, sep=''), type=plotf)

for(j in 1:nF) {

    spawn.per.recr = s.per.recr(nages=nages, fec.age=fec.age, mat.age=mat.age, M.age= M.age, F.mult=F.range[j], sel.age=sel.age, spawn.time=spawn.time)
    spr.vec[j] = spawn.per.recr/spr0
    exp.spawn =  s.per.recr(nages=nages, fec.age=rep(1,nages), mat.age=mat.age, M.age= M.age, F.mult=F.range[j], sel.age=sel.age, spawn.time=spawn.time)
    #exp.spawn.vec[j] = exp.spawn/spawn0
    exp.spawn.vec[j] = exp.spawn
          } # end loop over F

par(mfrow=c(1,1), mar=c(4,4,2,4) )

plot(F.range, exp.spawn.vec, type='n', xlab="Full F", ylab="Expected Spawnings", lwd=2,
     col="skyblue3", ylim=c(0,1.2*max(exp.spawn.vec)))
abline(v=seq(0.1,2.0, by=0.1), col="grey85")
abline(h=c(1,2,3), col="skyblue1")
lines(F.range, exp.spawn.vec, lwd=2, col="skyblue3" )
points(F.range, exp.spawn.vec, pch=19, col="skyblue3" )
scale.spr.vec <- max(spr.vec)/max(exp.spawn.vec)

lines(F.range, spr.vec/scale.spr.vec, col="red", lwd=2 )
axis(side=4, at=seq(0,1,by=0.1)/scale.spr.vec, lab=seq(0,1,by=0.1), las=2,
    col='black', col.axis="black")
mtext(side=4, "% SPR", line=3, col="red")
title (paste("Expected Spawnings and SPR Reference Points (Years Avg = ", nyrs.ave,")",
    sep=""), cex=0.9, outer=T, line=-1 )

  if (save.plots) savePlot(paste(od, "Exp.Spawn.SPR.Curves.", plotf, sep=''), type=plotf)

exp.spawn.table <- as.data.frame(matrix(NA, nrow=nF, ncol=3) )
exp.spawn.table[,1] <- F.range
exp.spawn.table[,2] <- exp.spawn.vec
exp.spawn.table[,3] <- spr.vec

par(mfrow=c(1,1), mar=c(2,2,2,2))
plot(seq(1,42), seq(-3,38), type='n', axes=F, bty='n',xlab="",ylab="")


text(x=0,y=36, labels="F", font=2, pos=4)
text(x=4, y=36, labels="E[Sp]" , font=2, pos=4,cex=0.9)
text(x=9, y=36, labels="SPR", font=2, pos=4 )

text(x=15,y=36, labels="F", font=2, pos=4)
text(x=19, y=36, labels="E[Sp]" , font=2, pos=4,cex=0.9)
text(x=24, y=36, labels="SPR", font=2, pos=4 )

text(x=30,y=36, labels="F", font=2, pos=4)
text(x=34, y=36, labels="E[Sp]" , font=2, pos=4,cex=0.9)
text(x=39, y=36, labels="SPR", font=2, pos=4 )

for (i in 1:35) {
   text(x=0, y=seq(35,1, by=-1), labels=F.range[1:35], cex=0.82, pos=4, font=1 )
   text(x=4, y=seq(35,1, by=-1), labels=round(exp.spawn.vec[1:35],4), cex=0.82, pos=4, font=1 )
   text(x=9, y=seq(35,1, by=-1), labels=round(spr.vec[1:35],4), cex=0.82, pos=4, font=1 )
    }
for (i in 36:70) {
   text(x=15, y=seq(35,1, by=-1), labels=F.range[36:70], cex=0.82, pos=4, font=1)
   text(x=19, y=seq(35,1, by=-1), labels=round(exp.spawn.vec[36:70],4), cex=0.82, pos=4, font=1)
   text(x=24, y=seq(35,1, by=-1), labels=round(spr.vec[36:70],4), cex=0.82, pos=4, font=1)
    }
for (i in 71:105) {
   text(x=30, y=seq(35,1, by=-1), labels=F.range[71:105], cex=0.82, pos=4, font=1)
   text(x=34, y=seq(35,1, by=-1), labels=round(exp.spawn.vec[71:105],4), cex=0.82, pos=4, font=1)
   text(x=39, y=seq(35,1, by=-1), labels=round(spr.vec[71:105],4), cex=0.82, pos=4, font=1)
    }
title (paste("Expected Spawnings & SPR Reference Points (Years Avg = ", nyrs.ave,")",
      sep=""), cex=0.9, outer=T, line=-1 )

  if (save.plots) savePlot(paste(od, "Exp.Spawn.SPR.Table.", plotf, sep=''), type=plotf)

frep1 <-asap$options$Freport.agemin
frep2 <-asap$options$Freport.agemax
if (frep1==frep2) freport <-F.range*sel.age[frep1:frep2]
if (frep2>frep1) freport <-F.range*mean(sel.age[frep1:frep2])


exp.spawn.table<- as.data.frame(cbind(F.range, exp.spawn.vec, spr.vec, freport))
colnames(exp.spawn.table) <- c("Full.F", "Exp.Spawn", "SPR", paste("Freport_",frep1,"-",frep2,sep=""))
write.csv( exp.spawn.table, file=paste(od,"Exp.Spawn.Table.csv", sep=""), row.names=F )


} # end function

#------------------------------------

#------------------------------------
#--------------  RETROSPECTIVE PLOTS -----------------------
#-------------------------------------        
get.retro <- function(asap.rts,asap){
  # get the number of peels from the rts file
  ss3 <- shell(paste("dir ", asap.rts, sep=""), intern=T )
  ss4 <- which(ss3=="File Not Found")
  if (length(ss4)>0)  stop(   paste("Retro File Not Found: ", asap.rts, sep="") )
  npeels <- scan(asap.rts,n=1) # includes base run as one of the peels 
  asap.name <- substr(asap.name, 1, (nchar(asap.name)-4)  )

  retro <- list()
  retro$nyears <- asap$parms$nyears
  retro$nages <- asap$parms$nages
  retro$favg <- matrix(NA, nrow=asap$parms$nyears, ncol=npeels)
  retro$ssb <- matrix(NA, nrow=asap$parms$nyears, ncol=npeels)
  retro$jan1b <- matrix(NA, nrow=asap$parms$nyears, ncol=npeels)
  retro$explb <- matrix(NA, nrow=asap$parms$nyears, ncol=npeels)
  retro$stockn <- matrix(NA, nrow=asap$parms$nyears, ncol=npeels)
  retro$stock_age <- list()
  for (j in 1:asap$parms$nages){
     retro$stock_age[[j]] <- matrix(NA, nrow=asap$parms$nyears, ncol=npeels)
  }
  
  for (i in 0:(npeels-1)){
    if (i < 10) i3 <- paste("00",i, sep="")
    else if(i < 100) i3 <- paste("0",i, sep="")
    a1 <- paste(asap.name,'_',i3,'.rdat', sep="")
    rr <- dget(a1)
    retro$favg[1:rr$parms$nyears,(npeels-i)] <- rr$F.report
    retro$ssb[1:rr$parms$nyears,(npeels-i)] <- rr$SSB
    retro$jan1b[1:rr$parms$nyears,(npeels-i)] <- rr$tot.jan1.B  
    retro$explb[1:rr$parms$nyears,(npeels-i)] <- rr$exploitable.B        
    retro$stockn[1:rr$parms$nyears,(npeels-i)] <- apply(rr$N.age,1,sum)
    for (j in 1:asap$parms$nages){
       retro$stock_age[[j]][1:rr$parms$nyears,(npeels-i)] <- rr$N.age[,j]
    }
  }
  retro
}

#-------------------------------------
plot.retro <- function(retro.mat,years,nages,y.lab,y.range1=NA,y.range2=NA){
   nyears <- length(years)
   ncols <- length(retro.mat[1,])
   npeels <- ncols-1
   # standard retro plot
   if(is.na(max(y.range1))) y.range1 <- range(retro.mat, na.rm=T)
   plot(years,retro.mat[,ncols],lwd=2,col="blue",type='l',xlab="Year",ylab=y.lab,ylim=y.range1)
     for (i in 1:npeels){
       lines(years,retro.mat[,(ncols-i)],type='l')
         points(years[nyears-i],retro.mat[nyears-i,ncols-i],pch=16,col="red") 
     }
     
   # relative retro plot
   rel.mat <- retro.mat
   for (i in 1:nyears){
     rel.mat[i,] <- (retro.mat[i,] - retro.mat[i,ncols]) / retro.mat[i,ncols]
   }
   if(is.na(max(y.range2))) y.range2 <- range(rel.mat, na.rm=T)
   plot(years,rel.mat[,ncols],lwd=2,col="blue",type='l',xlab="Year",ylab="Relative Retro",ylim=y.range2)
     rho.vals<-rep(NA,(npeels+1))
     mm <- 0  
     for (i in 1:npeels){
       lines(years,rel.mat[,(ncols-i)],type='l')
         points(years[nyears-i],rel.mat[nyears-i,ncols-i],pch=16,col="red")
         mm <- mm+rel.mat[nyears-i,ncols-i] 
         rho.vals[i] <- rel.mat[nyears-i,ncols-i]
     }
     mohn.rho <- mm/npeels
     rho.vals[(npeels+1)] <- mohn.rho
     text(x=years[1], y=0.9*y.range2[2], label=expression(paste(rho,"= ",sep="")), pos=4)
     text(x=(years[1]+3), y=0.9*y.range2[2], label=round(mohn.rho,3), pos=4)
     return(rho.vals)
}

#------------------------------------------------------------------------------------
plot.retro.wrapper <- function(asap.rts,asap){
  years <- seq(asap$parms$styr,asap$parms$endyr)
  my <- get.retro(asap.rts,asap)
  #Retro.rho <- matrix(NA, 1, (6+asap$parms$nages) )
  par(mfrow=c(3,2),mar=c(4,4,4,2) )
  # F, SSB, R
  f.rho<-plot.retro(my$favg,years,asap$parms$nages,"Average F",y.range1=c(0,max(my$favg, na.rm=T)))
  ssb.rho<-plot.retro(my$ssb,years,asap$parms$nages,"SSB",y.range1=c(0,max(my$ssb, na.rm=T)))
  recr.rho<-plot.retro(my$stock_age[[1]],years,asap$parms$nages,"Recruitment",y.range1=c(0,max(my$stock_age[[1]], na.rm=T)))
  title(main="F, SSB, R", outer=T, line=-1)
  if(save.plots) savePlot(paste(od,"retro_F_SSB_R.",plotf, sep=""), type=plotf)

  # Jan-1 B, Exploitable B, Total Stock N
  jan1b.rho<-plot.retro(my$jan1b,years,asap$parms$nages,"Jan-1 B",y.range1=c(0,max(my$jan1b, na.rm=T)))
  explb.rho<-plot.retro(my$explb,years,asap$parms$nages,"Exploitable B",y.range1=c(0,max(my$explb, na.rm=T)))
  stockn.rho<-plot.retro(my$stockn,years,asap$parms$nages,"Total Stock N",y.range1=c(0,max(my$stockn, na.rm=T)))
  title(main="Jan-1 B, Exploitable B, Total Stock N", outer=T, line=-1)
  if(save.plots) savePlot(paste(od,"retro_Jan1B_ExplB_StockN.",plotf, sep=""), type=plotf)  
  
  # Population numbers at age
  age.rho <- matrix(NA,1,asap$parms$nages)
  for (j in 1:asap$parms$nages){
    temp.rho<-plot.retro(my$stock_age[[j]],years,asap$parms$nages,paste("N at Age ",j, sep=""),y.range1=c(0,max(my$stock_age[[j]], na.rm=T)))
    if (j==1) age.rho<- (temp.rho )
    if (j>1) age.rho<-rbind(age.rho, t(temp.rho) )    
    title(main="Stock Numbers at Age", outer=T, line=-1)
    if (j%%3==0){
       if(save.plots) savePlot(paste(od,"retro_ages_",j-2,"_",j,".",plotf, sep=""), type=plotf)
    }
  }  # end loop over ages
  age.rho<-as.data.frame(t(age.rho))
  colnames(age.rho) <- c(paste(rep("Age",asap$parms$nages),seq(1,asap$parms$nages), sep="."))
  if (asap$parms$nages%%3==1){
    if(save.plots) savePlot(paste(od,"retro_ages_",j,".",plotf, sep=""), type=plotf)
  }
  if (asap$parms$nages%%3==2){
    if(save.plots) savePlot(paste(od,"retro_ages_",j-1,"_",j,".",plotf, sep=""), type=plotf)
  }
  
  par(mfrow=c(1,1))
   Retro.rho <-cbind(f.rho, ssb.rho, recr.rho, jan1b.rho, explb.rho, stockn.rho, age.rho)
   rownames(Retro.rho) <- c(paste("peel", seq(1,(length(f.rho)-1)), sep="."),"Mohn.rho"  )
   
write.csv(Retro.rho, file=paste(od, "Retro.rho.values.csv",sep=""),   row.names=T)
  
}

#-----------------------------------------------------------
#------  write RMSE multipliers to .csv file  ---------------
write.RMSE.multipliers <- function(asap) {

rmse.mult <- unlist(asap$Neff.stage2.mult)
write.csv(rmse.mult, file=paste(od, "NEFF.Mult.Stage2.csv", sep=""),   row.names=T)
} # end function

#-----------------------------------------
#----------- MCMC PLOTS ------------------
plot.mcmc  <- function(asap, asap.name)   {
library(tseries)

f.chain <- paste(asap.name, "MCMC.dat" )
f.chain <- "asap3MCMC.dat"
chain1c <- read.table(paste(wd,f.chain, sep=""), header=T)
## new stuff
niter = dim(chain1c)[1]
chain1b = chain1c[(burn+1):niter,]
niter = dim(chain1b)[1]
chain1a = chain1b[seq(1,niter,by=thin),]

bsn1c <- read.table(paste(wd,asap.name, ".BSN", sep=""), header=T)
niter = dim(bsn1c)[1]
bsn1b = bsn1c[(burn):niter,]
niter = dim(bsn1b)[1]
bsn1a = bsn1b[seq(1,niter,by=thin),]
write.table(bsn1a, file=paste(od, "New_BSN_file.BSN", sep=""), row.names=T)


niter = dim(chain1a)[1]
nyears <- asap$parms$nyears
years <- seq(asap$parms$styr, asap$parms$endyr)

f.chain <- chain1a[,seq(1,nyears)]
ssb.cols1 <- which(substr(names(chain1a),1,3)=="SSB")
ssb.chain <- chain1a[,ssb.cols1[1:(length(ssb.cols1)-2)] ]
fmult.chain <- chain1a[,which(substr(names(chain1a),1,3)=="Fmu")]
totB.chain <- chain1a[,which(substr(names(chain1a),1,3)=="tot")]
MSY.chain <- chain1a[,which(substr(names(chain1a),1,3)=="MSY")]
MSY.col <- which(substr(names(chain1a),1,3)=="MSY")
SSBmsy.chain <- chain1a[,(MSY.col+1)]
Fmsy.chain <- chain1a[,(MSY.col+2) ]
SSBmsy.ratio.chain <- chain1a[,(MSY.col+3) ]
Fmsy.ratio.chain <- chain1a[,(MSY.col+4 ) ]


# examine Trace in first and last year

par(mfcol=c(2,1),mar=c(4,4,2,2), oma=c(1,1,1,1))
plot(seq(1,niter), ssb.chain[,1], type='l', xlab="Iteration", ylab=paste("SSB",years[1], sep="") )
plot(seq(1,niter), ssb.chain[,nyears], type='l', xlab="Iteration", ylab=paste("SSB",years[nyears], sep="") )
if (save.plots==T)  savePlot(paste(od,'Trace.SSB.first.last.yr.png', sep=""),type=plotf)

par(mfcol=c(2,1),mar=c(4,4,2,2), oma=c(1,1,1,1))
plot(seq(1,niter), f.chain[,1], type='l', xlab="Iteration", ylab=paste("Freport",years[1], sep="") )
plot(seq(1,niter), f.chain[,nyears], type='l', xlab="Iteration", ylab=paste("Freport",years[nyears], sep="") )
if (save.plots==T)  savePlot(paste(od,'Trace.Freport.first.last.yr.png', sep=""),type=plotf)


# look at auto-correlation plot
par(mfcol=c(2,1),mar=c(4,4,2,2))
ac.ssb1<-acf(ssb.chain[,1], lag.max=10, plot=F)
ac.ssb2<-acf(ssb.chain[,nyears], lag.max=10, plot=F)
ylims <- c(1.1*min(ac.ssb1$acf,-2/sqrt(niter)), 1 ) 
plot(seq(0,10), ac.ssb1$acf, xlab=paste("Lag SSB",years[1],sep=""), ylab="ACF", type="h",ylim=ylims )
abline(h=0, lwd=2, col='black')
abline(h=2/sqrt(niter), col='red', lty=2)
abline(h=-2/sqrt(niter), col='red', lty=2)

plot(seq(0,10), ac.ssb2$acf, xlab=paste("Lag SSB",years[nyears],sep=""), ylab="ACF", type="h",ylim=ylims )
abline(h=0, lwd=2, col='black')
abline(h=2/sqrt(niter), col='red', lty=2)
abline(h=-2/sqrt(niter), col='red', lty=2)

if (save.plots==T)  savePlot(file=paste(od, "lag.autocorrelation.SSB.png",sep=""), type=plotf)

par(mfcol=c(2,1),mar=c(4,4,2,2))
ac.f1<-acf(f.chain[,1], lag.max=10, plot=F)
ac.f2<-acf(f.chain[,nyears], lag.max=10, plot=F)
ylims <- c(1.1*min(ac.f1$acf,-2/sqrt(niter)), 1 ) 
plot(seq(0,10), ac.f1$acf, xlab=paste("Lag F",years[1],sep=""), ylab="ACF", type="h",ylim=ylims )
abline(h=0, lwd=2, col='black')
abline(h=2/sqrt(niter), col='red', lty=2)
abline(h=-2/sqrt(niter), col='red', lty=2)

plot(seq(0,10), ac.f2$acf, xlab=paste("Lag F",years[nyears],sep=""), ylab="ACF", type="h",ylim=ylims )
abline(h=0, lwd=2, col='black')
abline(h=2/sqrt(niter), col='red', lty=2)
abline(h=-2/sqrt(niter), col='red', lty=2)

if (save.plots==T)  savePlot(file=paste(od, "lag.autocorrelation.Freport.png",sep=""), type=plotf)


# examine Distribution in first and last year
ssb1.hist<-hist(ssb.chain[,1],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
ssb2.hist<-hist(ssb.chain[,nyears],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
xlims <- c(min(ssb1.hist$mids, ssb2.hist$mids), max(ssb1.hist$mids, ssb2.hist$mids))

x1=(ssb1.hist$mids)
y1=(ssb1.hist$counts) 
x2=(ssb2.hist$mids)
y2=(ssb2.hist$counts) 

par(mfrow=c(2,1) )
plot(x1,y1, type="l",lty=2,col="blue",lwd=4,xlab=paste("SSB", years[1],sep=""), ylab="Freq", 
  ylim=c(0, max( 1.02*y1)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
abline(v=asap$SSB[1], col='red', lty=4)
legend('topleft', legend=c("MCMC", "Point Est."), col=c("blue", "red"), lwd=c(2,2),
         lty=c(1,4), cex=0.85 )

plot(x2,y2, type="l",lty=2,col="blue",lwd=4,xlab=paste("SSB", years[nyears],sep=""), ylab="Freq", 
  ylim=c(0, max( 1.02*y2)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
abline(v=asap$SSB[nyears], col='red', lty=4)
if (save.plots==T)  savePlot(paste(od, 'Distribution.SSB.first.last.yr.png', sep=""), type=plotf)



f1.hist<-hist(f.chain[,1],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
f2.hist<-hist(f.chain[,nyears],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
xlims <- c(min(f1.hist$mids, f2.hist$mids), max(f1.hist$mids, f2.hist$mids))

x1=(f1.hist$mids)
y1=(f1.hist$counts) 
x2=(f2.hist$mids)
y2=(f2.hist$counts) 

par(mfrow=c(2,1) )
plot(x1,y1, type="l",lty=2,col="blue",lwd=4,xlab=paste("Freport", years[1],sep=""), ylab="Freq", 
  ylim=c(0, max( 1.02*y1)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
abline(v=asap$F.report[1], col='red', lty=4)
legend('topleft', legend=c("MCMC", "Point Est."), col=c("blue", "red"), lwd=c(2,2),
         lty=c(1,4), cex=0.85 )


plot(x2,y2, type="l",lty=2,col="blue",lwd=4,xlab=paste("Freport", years[nyears],sep=""), ylab="Freq", 
  ylim=c(0, max( 1.02*y2)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
abline(v=asap$F.report[nyears], col='red', lty=4)

if (save.plots==T)  savePlot(paste(od, 'Distribution.Freport.first.last.yr.png', sep=""), type=plotf)



fm1.hist<-hist(fmult.chain[,1],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
fm2.hist<-hist(fmult.chain[,nyears],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
xlims <- c(min(fm1.hist$mids, fm2.hist$mids), max(fm1.hist$mids, fm2.hist$mids))

x1=(fm1.hist$mids)
y1=(fm1.hist$counts) 
x2=(fm2.hist$mids)
y2=(fm2.hist$counts) 
full.f <-apply(asap$F.age,1,max)

par(mfrow=c(2,1) )
plot(x1,y1, type="l",lty=2,col="blue",lwd=4,xlab=paste("Full F", years[1],sep=""), ylab="Freq", 
  ylim=c(0, max( 1.02*y1)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
abline(v=full.f[1], col='red', lty=4)
legend('topleft', legend=c("MCMC", "Point Est."), col=c("blue", "red"), lwd=c(2,2),
         lty=c(1,4), cex=0.85 )


plot(x2,y2, type="l",lty=2,col="blue",lwd=4,xlab=paste("Full F", years[nyears],sep=""), ylab="Freq", 
  ylim=c(0, max( 1.02*y2)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
abline(v=full.f[nyears], col='red', lty=4)

if (save.plots==T)  savePlot(paste(od, 'Distribution.Fmult.first.last.yr.png', sep=""), type=plotf)


b1.hist<-hist(totB.chain[,1],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
b2.hist<-hist(totB.chain[,nyears],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
xlims <- c(min(b1.hist$mids, b2.hist$mids), max(b1.hist$mids, b2.hist$mids))

x1=(b1.hist$mids)
y1=(b1.hist$counts) 
x2=(b2.hist$mids)
y2=(b2.hist$counts) 
tot.B <- asap$tot.jan1.B

par(mfrow=c(2,1) )
plot(x1,y1, type="l",lty=2,col="blue",lwd=4,xlab=paste("Jan-1 B", years[1],sep=""), ylab="Freq", 
  ylim=c(0, max( 1.02*y1)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
abline(v=tot.B[1], col='red', lty=4)
legend('topleft', legend=c("MCMC", "Point Est."), col=c("blue", "red"), lwd=c(2,2),
         lty=c(1,4), cex=0.85 )


plot(x2,y2, type="l",lty=2,col="blue",lwd=4,xlab=paste("Jan-1 B", years[nyears],sep=""), ylab="Freq", 
  ylim=c(0, max( 1.02*y2)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
abline(v=tot.B[nyears], col='red', lty=4)

if (save.plots==T)  savePlot(paste(od, 'Distribution.Jan1.B.first.last.yr.png', sep=""), type=plotf)


####   Probability Interval Plots
#plot 95% PI

 #sort 
par(mfrow=c(1,1), mar=c(4,4,2,3), oma=c(1,1,1,1)  )

ssb.sort<- (apply(ssb.chain,2,sort ))
p5 = trunc( dim(ssb.sort)[1] *.05)
p95= trunc( dim(ssb.sort)[1] *.95)

p50=median(dim(ssb.sort)[1])
p10 = trunc( dim(ssb.sort)[1] *.10)
p90 = trunc( dim(ssb.sort)[1] *.90)


plot(years, ssb.sort[p5,], type='l', col='grey35', lwd=2, xlab='Year',
    ylab='', ylim=c(0,1.03*max(ssb.sort[p95,])), axes=F  )
axis(side=1, at=years[seq(1,nyears,by=2)], labels=years[seq(1,nyears,by=2)], las=2)
axis(side=2, at=pretty(seq(0,1.01*max(ssb.sort)), n=10) , 
    labels=format(pretty(seq(0,1.01*max(ssb.sort)), n=10), scientific=T), las=1)
axis(side=4, at=pretty(seq(0,1.01*max(ssb.sort)), n=10) , 
    labels=format(pretty(seq(0,1.01*max(ssb.sort)), n=10), scientific=T), las=1)
box()
mtext(side=2, text="SSB", outer=T)    
lines( years, ssb.sort[p95,] , col='grey35', lwd=2)  
lines( years, apply(ssb.sort,2,median) , col='red', lwd=2)      
lines( years, asap$SSB , col='green3', lwd=1)      
points( years, asap$SSB , col='green3', pch=17, cex=0.7)  

legend('topleft', horiz=T, legend=c("5th, 95th", "Median","Point Est."), 
      col=c("grey35", "red", "green3"), lwd=c(2,2,2), lty=c(1,1,1), cex=0.85,
       pch=c(1,1,17), pt.cex=c(0,0,1) )
    
if (save.plots==T)  savePlot(paste(od, "SSB.90PI.", plotf,sep=""), type=plotf)


ssb.pi <- cbind(years, "5th"=ssb.sort[p5,], "Median"=apply(ssb.sort,2,median), "95th"=ssb.sort[p95,])
write.csv(ssb.pi, file=paste(od, "ssb.90pi.csv",sep=""), 
             row.names=F )



f.sort<- (apply(f.chain,2,sort ))
p5 = trunc( dim(f.sort)[1] *.05)
p95= trunc( dim(f.sort)[1] *.95)

p50=median(dim(f.sort)[1])
p10 = trunc( dim(f.sort)[1] *.10)
p90 = trunc( dim(f.sort)[1] *.90)

par(mfrow=c(1,1), mar=c(4,4,2,3), oma=c(1,1,1,1)  )
plot(years, f.sort[p5,], type='l', col='grey35', lwd=2, xlab='Year',
    ylab='Freport', ylim=c(0,1.1*max(f.sort[p95,])), axes=F  )
axis(side=1, at=years[seq(1,nyears,by=2)], labels=years[seq(1,nyears,by=2)], las=2)
axis(side=2, at=pretty(seq(0,1.01*max(f.sort), by=0.1), n=10) , 
    labels=pretty(seq(0,1.01*max(f.sort), by=0.1), n=10), las=1)
axis(side=4, at=pretty(seq(0,1.01*max(f.sort), by=0.1), n=10) , 
    labels=pretty(seq(0,1.01*max(f.sort), by=0.1), n=10), las=1)
box()
    
lines( years, f.sort[p95,] , col='grey35', lwd=2)  
lines( years, apply(f.sort,2,median) , col='red', lwd=2)      
lines( years, asap$F.report , col='green3', lwd=1)      
points( years, asap$F.report , col='green3', pch=17, cex=0.7)      
legend('topleft', horiz=T, legend=c("5th, 95th", "Median","Point Est."), 
      col=c("grey35", "red", "green3"), lwd=c(2,2,2), lty=c(1,1,1), cex=0.85,
       pch=c(1,1,17), pt.cex=c(0,0,1) )

if (save.plots==T)  savePlot(paste(od, "Freport.90PI.", plotf,sep=""), type=plotf)


Freport.pi <- cbind(years, "5th"=f.sort[p5,], "Median"=apply(f.sort,2,median), "95th"=f.sort[p95,])
write.csv(Freport.pi, file=paste(od, "Freport.90pi.csv",sep=""),   row.names=F)
          



fm.sort<- (apply(fmult.chain,2,sort ))
p5 = trunc( dim(fm.sort)[1] *.05)
p95= trunc( dim(fm.sort)[1] *.95)

p50=median(dim(fm.sort)[1])
p10 = trunc( dim(fm.sort)[1] *.10)
p90 = trunc( dim(fm.sort)[1] *.90)

par(mfrow=c(1,1), mar=c(4,4,2,3), oma=c(1,1,1,1)  )
plot(years, fm.sort[p5,], type='l', col='grey35', lwd=2, xlab='Year',
    ylab='Full F', ylim=c(0,1.1*max(fm.sort[p95,])), axes=F  )
axis(side=1, at=years[seq(1,nyears,by=2)], labels=years[seq(1,nyears,by=2)], las=2)
axis(side=2, at=pretty(seq(0,1.01*max(fm.sort), by=0.1), n=10) , 
    labels=pretty(seq(0,1.01*max(fm.sort), by=0.1), n=10), las=1)
axis(side=4, at=pretty(seq(0,1.01*max(fm.sort), by=0.1), n=10) , 
    labels=pretty(seq(0,1.01*max(fm.sort), by=0.1), n=10), las=1)
box()
    
lines( years, fm.sort[p95,] , col='grey35', lwd=2)  
lines( years, apply(fm.sort,2,median) , col='red', lwd=2)      
lines( years, full.f , col='green3', lwd=1)      
points( years, full.f, col='green3', pch=17, cex=0.7)     
legend('topleft', horiz=T, legend=c("5th, 95th", "Median","Point Est."), 
      col=c("grey35", "red", "green3"), lwd=c(2,2,2), lty=c(1,1,1), cex=0.85,
       pch=c(1,1,17), pt.cex=c(0,0,1) )
 
if (save.plots==T)  savePlot(paste(od, "Full.F.90PI.", plotf,sep=""), type=plotf)


Full.F.pi <- cbind(years, "5th"=fm.sort[p5,], "Median"=apply(fm.sort,2,median), "95th"=fm.sort[p95,])
write.csv(Full.F.pi, file=paste(od, "Full.F.90pi.csv",sep=""), 
         row.names=F)



tb.sort<- (apply(totB.chain,2,sort ))
p5 = trunc( dim(tb.sort)[1] *.05)
p95= trunc( dim(tb.sort)[1] *.95)

p50=median(dim(tb.sort)[1])
p10 = trunc( dim(tb.sort)[1] *.10)
p90 = trunc( dim(tb.sort)[1] *.90)

par(mfrow=c(1,1), mar=c(4,4,2,3), oma=c(1,1,1,1)  )
plot(years, tb.sort[p5,], type='l', col='grey35', lwd=2, xlab='Year',
    ylab='', ylim=c(0,1.03*max(tb.sort[p95,])), axes=F  )
axis(side=1, at=years[seq(1,nyears,by=2)], labels=years[seq(1,nyears,by=2)], las=2)
axis(side=2, at=pretty(seq(0,1.01*max(tb.sort), by=max(tb.sort)/10), n=10) , 
    labels=format(pretty(seq(0,1.01*max(tb.sort), by=max(tb.sort)/10), n=10), scientific=T), las=1)
axis(side=4, at=pretty(seq(0,1.01*max(tb.sort), by=max(tb.sort)/10), n=10) , 
    labels=format(pretty(seq(0,1.01*max(tb.sort), by=max(tb.sort)/10), n=10), scientific=T), las=1)

box()
mtext(side=2, text="Jan-1 Biomass", outer=T)    
lines( years, tb.sort[p95,] , col='grey35', lwd=2)  
lines( years, apply(tb.sort,2,median) , col='red', lwd=2)      
lines( years, asap$tot.jan1.B , col='green3', lwd=1)      
points( years, asap$tot.jan1.B, col='green3', pch=17, cex=0.7) 
legend('topleft', horiz=T, legend=c("5th, 95th", "Median","Point Est."), 
      col=c("grey35", "red", "green3"), lwd=c(2,2,2), lty=c(1,1,1), cex=0.85,
       pch=c(1,1,17), pt.cex=c(0,0,1) )
     
if (save.plots==T)  savePlot(paste(od, "Jan1.B.90PI.", plotf,sep=""), type=plotf)


Tot.B.pi <- cbind(years, "5th"=tb.sort[p5,], "Median"=apply(tb.sort,2,median), "95th"=tb.sort[p95,])
write.csv(Tot.B.pi, file=paste(od, "Jan1.B.90pi.csv",sep=""), 
       row.names=F)


}  # end function
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----Catch curve and cohort correspondence plots-------------------------------
#-------------------------------------------------------------------------------
# function to compute catch at age (matrix) 
# from proportions at age (matrix), weight at age (matrix) and total weight (vector)
wtprop2caa <- function(totwt,waa,props){
  caa <- props
  for (i in 1:length(totwt)){
    if (sum(props[i,]) == 0) caa[i,] = 0
    if (sum(props[i,]) > 0) caa[i,] <- props[i,] * (totwt[i] / sum(props[i,] * waa[i,]))
  }
  return(caa)
}

#-------------------------------------------------------------------------------
# function replace zeros and take log
rep0log <- function(mat){
  mat1 <- mat
  for (i in 1:length(mat1[,1])){
    mat1[i,mat1[i,]==0] <- NA
    mat1[i,] <- log(mat1[i,])
  }
  return(mat1)
}
#-------------------------------------------------------------------------------

# function make cohorts
makecohorts <- function(mat){
  NAmat <- matrix(NA, nrow=length(mat[1,]), ncol=length(mat[1,]))
  matcoh1 <- rbind(NAmat,mat,NAmat)
  nr <- length(matcoh1[,1])
  nc <- length(mat[1,])
  matcoh <- matrix(NA, nrow=nr, ncol=nc)
  for (i in 1:nc){
    matcoh[1:(nr-i),i] <- matcoh1[i:(nr-1),i]
  }
  return(matcoh)
}
#-------------------------------------------------------------------------------

# function to plot all ages by all ages of data by cohorts
# assumes matrix has ages as columns and that cohorts are in rows
plotcoh <- function(matcoh,mytitle="",mylabels=NA){
  nc <- length(matcoh[1,])
  my.cor <- cor(matcoh,use="pairwise.complete.obs")
  my.cor.round <- round(my.cor,2)
  usr <- par("usr"); on.exit(par(usr))
  par(mfcol=c(nc,nc))
  par(oma=c(0,0,3,1),mar=c(1,1,0,0))
  for (i in 1:nc){
    for (j in nc:1){
      if (i == j){
        plot(1:10,1:10,type='n',axes=F)
          if (is.na(mylabels[1])) text(5,5,paste("age-",i,sep=""),cex=1.4)
          if (is.na(mylabels[1])==F) text(5,5,mylabels[i],cex=1.4)
      }
      if (i < j){
        if (!is.na(my.cor[i,j])){
          plot(matcoh[,i],matcoh[,j],axes=F) # make sure have some data to plot
          xx <- matcoh[,i]
          yy <- matcoh[,j]
          my.fit <- lm(yy~xx)
          if (!is.na(my.fit$coefficients[2])) abline(my.fit,col="red")
          xrng <- data.frame(xx = seq(min(xx,na.rm=T),max(xx,na.rm=T),length.out=100))
          zz <- predict(my.fit,xrng,interval="confidence")
          lines(xrng[,1],zz[,2],col="blue")
          lines(xrng[,1],zz[,3],col="blue")
          box()
        }
        if (is.na(my.cor[i,j])){  # if not data, just make empty box
          plot(1:10,1:10,type='n',axes=F)
          box()
        }
      }
      if (i > j){
        plot(1:10,1:10,type='n',axes=F)
          txt <- format(my.cor.round[i,j], nsmall=2)
          text(5,5,txt)
          box()
      }
    }
  }
  title(mytitle, outer=T)
  if (save.plots) savePlot(paste(od,"cohort_age_matrix_",mytitle,".",plotf, sep=""), type=plotf)
  return(my.cor)
}
#-------------------------------------------------------------------------------

plot_catch_at_age_consistency <- function(asap){
# create plots of ages vs each other (correctly lagged) on log scale for catch by fleet
  cat.corr <- list()
  for (ifleet in 1:asap$parms$nfleets){
    if (asap$parms$nfleets == 1) title1 = "Catch"
    if (asap$parms$nfleets >= 2) title1 = paste("Catch for Fleet ",ifleet, sep="")
  
    # get catch at age
    catchob <- wtprop2caa(asap$catch.obs[ifleet,],  asap$WAA.mats[[(ifleet*2-1)]], asap$catch.comp.mats[[(ifleet*4-3)]])
    catchpr <- wtprop2caa(asap$catch.pred[ifleet,], asap$WAA.mats[[(ifleet*2-1)]], asap$catch.comp.mats[[(ifleet*4-2)]])

    # replace zeros with NA and take logs
    cob <- rep0log(catchob)
    cpr <- rep0log(catchpr)

    # make cohorts
    cob.coh <- makecohorts(cob)
    cpr.coh <- makecohorts(cpr)

    # make the plots
    cob.cor <- plotcoh(cob.coh,mytitle=paste(title1," Observed", sep=""))
    cpr.cor <- plotcoh(cpr.coh,mytitle=paste(title1," Predicted", sep=""))
    cat.corr[[ifleet]] <- list(cob.cor,cpr.cor)
  }
  return(cat.corr)
}
#-------------------------------------------------------------------------------

convert_survey_to_at_age <- function(asap){
# takes West Coast style surveys and converts them to catch at age matrices
 index.mats <- list()
 weight.mat.counter <- 0
 for (ind in 1:asap$parms$nindices){
  if (asap$control.parms$index.age.comp.flag[ind] == 1){  # used age composition for the index
    # get the aggregate index observed and predicted time series
    agg.ob <- asap$index.obs[[ind]]
    agg.pr <- asap$index.pred[[ind]]
    
    # get proportions for correct years and ages only
    age.min <- asap$control.parms$index.sel.start.age[ind]
    age.max <- asap$control.parms$index.sel.end.age[ind]
    props.ob <- asap$index.comp.mats[[(ind*2-1)]][asap$index.year.counter[[ind]],age.min:age.max]
    props.pr <- asap$index.comp.mats[[(ind*2)]][asap$index.year.counter[[ind]],age.min:age.max]
    
    # figure out units for aggregate and proportions
    agg.units <- asap$control.parms$index.units.aggregate[ind]
    prp.units <- asap$control.parms$index.units.proportions[ind]

    # get weight (matrix if necessary)
    if (agg.units==1 || prp.units==1){  # either in weight
      weight.mat.counter <- weight.mat.counter+1
      use.me <- weight.mat.counter + (asap$parms$nfleets * 2) + 4
      waa <- asap$WAA.mats[[use.me]][asap$index.year.counter[[ind]],age.min:age.max] 
    }
    
    # create index.obs and pred based on which of the four possible combinations of units is used for this index
    if (agg.units==1 && prp.units==1){  # both in weight
      index.ob <- agg.ob * props.ob / waa
      index.pr <- agg.pr * props.pr / waa
    }
    if (agg.units==1 && prp.units==2){  # agg in weight, props in numbers
      index.ob <- wtprop2caa(agg.ob,waa,props.ob)  # use catch function
      index.pr <- wtprop2caa(agg.pr,waa,props.pr)
    }
    if (agg.units==2 && prp.units==1){  # agg in numbers, props in weight
      # need to search for correct agg total in weight to result in observed agg total in number
      # for now just use simple approximation that agg.wt = sum(waa*prop) *ctot and then solve using both in weight approach
      agg.wt.ob <- apply((waa * props.ob),1,sum) * agg.ob
      agg.wt.pr <- apply((waa * props.pr),1,sum) * agg.pr
      index.ob <- agg.wt.ob * props.ob / waa
      index.pr <- agg.wt.pr * props.pr / waa
    }
    if (agg.units==2 && prp.units==2){  # both in numbers
      index.ob <- agg.ob * props.ob
      index.pr <- agg.pr * props.pr
    }
    
    # put matrices into full year matrix (ages only for selected ages though)
    # need to do this to account for missing years of data interspersed in time series
    index.ob.full <- matrix(NA, nrow=asap$parms$nyears, ncol=(age.max-age.min+1))
    index.pr.full <- matrix(NA, nrow=asap$parms$nyears, ncol=(age.max-age.min+1))
    for (i in 1:asap$index.nobs[ind]){
      index.ob.full[asap$index.year.counter[[ind]][i],] <- as.vector(index.ob[i,])
      index.pr.full[asap$index.year.counter[[ind]][i],] <- as.vector(index.pr[i,])
    }
    
    # save the results for this index
    index.mats$ob[[ind]] <- index.ob.full
    index.mats$pr[[ind]] <- index.pr.full  
  }
  if (asap$control.parms$index.age.comp.flag[ind] != 1){  # cannot use this index
    index.mats$ob[[ind]] <- NA
    index.mats$pr[[ind]] <- NA
  }
 }
 return(index.mats)  
}
#-------------------------------------------------------------------------------

plot_index_at_age_consistency <- function(asap){
# now loop through indices, check to make sure actually estimating proportions at age
# also need to use only the age range selected in the proportions
  index.corr <- list()
  
  # convert the west coast style indices to catch at age matrices
  index.mats <- convert_survey_to_at_age(asap)  

  # loop through all the indices
  for (ind in 1:asap$parms$nindices){
    if (asap$control.parms$index.age.comp.flag[ind] == 1){  # used age composition for the index
      title1 <- paste("Index ",ind, sep="")
    
      # replace zeros with NA and take logs
      iob <- rep0log(index.mats$ob[[ind]])
      ipr <- rep0log(index.mats$pr[[ind]])

      # make cohorts
      iob.coh <- makecohorts(iob)
      ipr.coh <- makecohorts(ipr)

      # get age range for index
      age.min <- asap$control.parms$index.sel.start.age[ind]
      age.max <- asap$control.parms$index.sel.end.age[ind]

      # create labels for plot (if necessary)
      mylabels=NA
      if (age.min != 1 || age.max != asap$parms$nages){
        mylabels <- paste("age-",age.min:age.max, sep="")
      }
      
      # make the plots
      iob.cor <- plotcoh(iob.coh,mytitle=paste(title1," Observed", sep=""),mylabels)
      ipr.cor <- plotcoh(ipr.coh,mytitle=paste(title1," Predicted", sep=""),mylabels)
      index.corr[[ind]] <- list(iob.cor,ipr.cor)
    }
  }
  return(index.corr)
}
#-------------------------------------------------------------------------------

find_peak_age <- function(cohmat){
# determines peak age within each cohort and replaces younger ages with NA
  ages <- seq(1,length(cohmat[1,]))
#  temp.sum <- apply(cohmat,1,function(x) sum(x, na.rm=T)) # this is the offending line, replaced with the one below
  temp.sum <- apply(cohmat,1,function(x) sum(exp(x), na.rm=T))  # note have to use exp because took logs in earlier function rep0log
  for (i in 1:length(cohmat[,1])){
    if (temp.sum[i] > 0){  # necessary to avoid cohorts that are all NA
      age.peak <- max(ages[cohmat[i,]==max(cohmat[i,],na.rm=T)], na.rm=T)  # find the peak age
      if (age.peak > 1) cohmat[i,1:(age.peak-1)] <- NA  # replace ages < peak.age with NA
    }
  }
  return(cohmat)
}
#-------------------------------------------------------------------------------

calc_Z_cohort <- function(cohmat){
# calculate Z along cohort using linear regression and return point estimate and 80% confidence interval
  ages <- seq(1,length(cohmat[1,])) 
  z <- matrix(NA, nrow=length(cohmat[,1]), ncol=3)
  for (i in 1:length(cohmat[,1])){
    if (length(cohmat[i,!is.na(cohmat[i,])]) >= 2){  # make sure there are at least 2 data points for point estimate
      z.fit <- lm(cohmat[i,]~ages)  # linear regression of cohort abundance vs age
      z[i,1] <- -1 * z.fit$coefficients[2]  # note change in sign for Z
      if (length(cohmat[i,!is.na(cohmat[i,])]) >= 3){  # need at least 3 data points for CI
        z[i,2:3] <- -1 * rev(confint(z.fit, "ages", level=0.80))  # note change in sign and order for Z CI
      }  
    }
  }
  return(z)
}
#-------------------------------------------------------------------------------

plot_catch_curves_for_catch <- function(asap,first.age=-999){
# create catch curve plots for catch by fleet
  usr <- par("usr"); on.exit(par(usr))
  par(oma=c(1,1,1,1),mar=c(4,4,1,0.5))
  catch.curve.cat <- list()
  cohort <- seq(asap$parms$styr-asap$parms$nages-1, asap$parms$endyr+asap$parms$nages-1)
  ages <- seq(1,asap$parms$nages)
  my.col <- rep(c("blue","red","green","orange","gray50"),50)
  for (ifleet in 1:asap$parms$nfleets){
    if (asap$parms$nfleets == 1) title1 = "Catch"
    if (asap$parms$nfleets >= 2) title1 = paste("Catch for Fleet ",ifleet, sep="")

    # get catch at age
    catchob <- wtprop2caa(asap$catch.obs[ifleet,],  asap$WAA.mats[[(ifleet*2-1)]], asap$catch.comp.mats[[(ifleet*4-3)]])
    catchpr <- wtprop2caa(asap$catch.pred[ifleet,], asap$WAA.mats[[(ifleet*2-1)]], asap$catch.comp.mats[[(ifleet*4-2)]])

    # replace zeros with NA and take logs
    cob <- rep0log(catchob)
    cpr <- rep0log(catchpr)

    # make cohorts
    cob.coh <- makecohorts(cob)
    cpr.coh <- makecohorts(cpr)
    
    # drop plus group
    cob.coh[,asap$parms$nages] <- NA
    cpr.coh[,asap$parms$nages] <- NA
    
    first.age.label <- 1
    if (first.age==1) title1 <- paste(title1," First Age = 1", sep="") 
    
    # determine which ages to use for each cohort (default)
    if (first.age == -999){
      cob.coh <- find_peak_age(cob.coh)
      cpr.coh <- find_peak_age(cpr.coh)
      first.age.label <- "find_peak"
     title1 <- paste(title1," (Peak Age)", sep="")       
    }
    
    # or drop youngest ages based on user control
    if (first.age > 1) {
      cob.coh[,1:(first.age-1)] <- NA
      cpr.coh[,1:(first.age-1)] <- NA
      title1 <- paste(title1," First Age = ",first.age, sep="")
      first.age.label <- first.age
    }
    
    # compute Z by cohort
    z.ob <- calc_Z_cohort(cob.coh)
    z.pr <- calc_Z_cohort(cpr.coh)
    
    # make the plots
    par(mfrow=c(2,1))
    plot(cohort,cohort,type='n',ylim=range(c(cob.coh,cpr.coh),na.rm=T),xlab="",ylab="Log(Catch)",main=paste(title1," Observed", sep=""))
    for (i in 1:length(cob.coh[,1])){
      lines(seq(cohort[i],cohort[i]+asap$parms$nages-1),cob.coh[i,],type='p',lty=1,pch=seq(1,asap$parms$nages),col="gray50")
      lines(seq(cohort[i],cohort[i]+asap$parms$nages-1),cob.coh[i,],type='l',lty=1,col=my.col[i])
    }
    
    errbar(cohort,z.ob[,1],z.ob[,3],z.ob[,2],xlab="Year Class",ylab="Z",ylim=range(c(z.ob,z.pr),na.rm=T))

    if (save.plots) savePlot(paste(od,"catch_curve_",title1,"_Observed_first_age_",first.age.label,".",plotf, sep=""), type=plotf)
    
    plot(cohort,cohort,type='n',ylim=range(c(cob.coh,cpr.coh),na.rm=T),xlab="",ylab="Log(Catch)",main=paste(title1," Predicted", sep=""))
    for (i in 1:length(cob.coh[,1])){
      lines(seq(cohort[i],cohort[i]+asap$parms$nages-1),cpr.coh[i,],type='p',lty=1,pch=seq(1,asap$parms$nages),col="gray50")
      lines(seq(cohort[i],cohort[i]+asap$parms$nages-1),cpr.coh[i,],type='l',lty=1,col=my.col[i])
    }

    errbar(cohort,z.pr[,1],z.pr[,3],z.pr[,2],xlab="Year Class",ylab="Z",ylim=range(c(z.ob,z.pr),na.rm=T))

    if (save.plots) savePlot(paste(od,"catch_curve_",title1,"_Predicted_first_age_",first.age.label,".",plotf, sep=""), type=plotf)


    # write out .csv files for Z, one file for each fleet
    colnames(z.ob) <-c("Z.obs","low.80%", "high.80%")
    write.csv(z.ob, file=paste(od,"Z.Ob.Fleet.",ifleet,".csv", sep=""),
         row.names=cohort)
        
    colnames(z.pr) <-c("Z.pred","low.80%", "high.80%")
    write.csv(z.pr, file=paste(od,"Z.Pr.Fleet.",ifleet,".csv", sep=""),
        row.names=cohort)

  }
  return(catch.curve.cat)
}
#-------------------------------------------------------------------------------

plot_catch_curves_for_index <- function(asap,first.age=-999){
# create catch curve plots for each west coast style index
  usr <- par("usr"); on.exit(par(usr))

  catch.curve.ind <- list()
  my.col <- rep(c("blue","red","green","orange","gray50"),50)

  # convert the west coast style indices to catch at age matrices
  index.mats <- convert_survey_to_at_age(asap)

  # loop through all the indices
  for (ind in 1:asap$parms$nindices){
    if (asap$control.parms$index.age.comp.flag[ind] == 1){  # used age composition for the index
      title1 <- paste("Index ",ind, sep="")

      min.age <- asap$control.parms$index.sel.start.age[ind]
      max.age <- asap$control.parms$index.sel.end.age[ind]
      ages <- seq(min.age, max.age)
      nages.i <- max.age - min.age + 1
      cohort <- seq(asap$parms$styr-nages.i-min.age, asap$parms$endyr+nages.i-min.age)
      
      # replace zeros with NA and take logs
      iob <- rep0log(index.mats$ob[[ind]])
      ipr <- rep0log(index.mats$pr[[ind]])

      # make cohorts
      iob.coh <- makecohorts(iob)
      ipr.coh <- makecohorts(ipr)

      # drop plus group
      if (asap$control.parms$index.sel.end.age[ind] == asap$parms$nages){
        iob.coh[,length(iob.coh[1,])] <- NA
        ipr.coh[,length(iob.coh[1,])] <- NA
      }

    first.age.label <- 1
    if (first.age==1) title1 <- paste(title1," First Age = 1", sep="") 
    
      # determine which ages to use for each cohort (default)
      if (first.age == -999){
        iob.coh <- find_peak_age(iob.coh)
        ipr.coh <- find_peak_age(ipr.coh)
        first.age.label <- "find_peak"
       title1 <- paste(title1," (Peak Age)", sep="")       
        
      }

      # or drop youngest ages based on user control
      if (first.age > min.age) {
        iob.coh[,1:(first.age-min.age)] <- NA
        ipr.coh[,1:(first.age-min.age)] <- NA
        title1 <- paste(title1," First Age = ",first.age, sep="")
        first.age.label <- first.age        
      }

      # compute Z by cohort
      z.ob <- calc_Z_cohort(iob.coh)
      z.pr <- calc_Z_cohort(ipr.coh)

      # make the plots
      par(mfrow=c(2,1))
      plot(cohort,cohort,type='n',ylim=range(c(iob.coh,ipr.coh),na.rm=T),xlab="",ylab="Log(Index)",main=paste(title1," Observed", sep=""))
      for (i in 1:length(iob.coh[,1])){
        lines(seq(cohort[i],cohort[i]+nages.i-1),iob.coh[i,],type='p',lty=1,pch=seq(1,nages.i),col="gray50")
        lines(seq(cohort[i],cohort[i]+nages.i-1),iob.coh[i,],type='l',lty=1,col=my.col[i])
      }

      errbar(cohort,z.ob[,1],z.ob[,3],z.ob[,2],xlab="Year Class",ylab="Z",ylim=range(c(z.ob,z.pr),na.rm=T))

   if (save.plots) savePlot(paste(od,"catch_curve_",title1,"_Observed_first_age_",first.age.label,".",plotf, sep=""), type=plotf)

      plot(cohort,cohort,type='n',ylim=range(c(iob.coh,ipr.coh),na.rm=T),xlab="",ylab="Log(Index)",main=paste(title1," Predicted", sep=""))
      for (i in 1:length(iob.coh[,1])){
        lines(seq(cohort[i],cohort[i]+nages.i-1),ipr.coh[i,],type='p',lty=1,pch=seq(1,nages.i),col="gray50")
        lines(seq(cohort[i],cohort[i]+nages.i-1),ipr.coh[i,],type='l',lty=1,col=my.col[i])
      }

      errbar(cohort,z.pr[,1],z.pr[,3],z.pr[,2],xlab="Year Class",ylab="Z",ylim=range(c(z.ob,z.pr),na.rm=T))

    if (save.plots) savePlot(paste(od,"catch_curve_",title1,"_Predicted_first_age_",first.age.label,".",plotf, sep=""), type=plotf)

    # write out .csv files for Z, one file for each fleet
    colnames(z.ob) <-c("Z.obs","low.80%", "high.80%")
    write.csv(z.ob, file=paste(od,"Z.Ob.Index.",ind,".csv", sep=""),
         row.names=cohort)

    colnames(z.pr) <-c("Z.pred","low.80%", "high.80%")
    write.csv(z.pr, file=paste(od,"Z.Pr.Index.",ind,".csv", sep=""),
         row.names=cohort)

    }
    
  }   # end loop over nindices
  return(catch.curve.ind)
}

#-------------------------------------------------------------------------------



#-------------------------------------
#------------------------------------
#------  PLOTS  ---------------


graphics.off()

windows( height=12, width=10, record=T)
#--- Model Diagnostics
gn <- grab.names(asap.name,asap)
  fleet.names <- gn$fleet.names
  index.names <- gn$index.names

a1<-grab.aux.files(asap.name, asap)
     if (exists("a1")!=F) p1b<-plot.high.corr(a1$asap.cor.mat, correlation.limit)
     if (exists("a1")!=F) npar<-a1$npar     
     if (exists("a1")!=F) max.grad<-a1$max.grad

plot.main.likelihoods(asap)
#plot.additional.likelihoods(asap,seq(5,18))
plot.RMSE.table(asap)
plot.RMSE.95CI(asap)   #plot RMSE with 95% CI
plot.catch.4.panel(asap)
#plot.discard.4.panel(asap)
plot.catch.age.comp(asap)     #fits to annual observed comp
plot.catch.age.comp.resids(asap)  #bubble plots
#plot.catch.age.comp(asap,is.catch.flag=F) # plot discard age comp
#plot.catch.age.comp.resids(asap,is.catch.flag=F) # plot discard age comp residuals
plot.fleet.Neff(asap)
plot.francis.fleet(asap,is.catch.flag=T)
plot.fleet.Neff(asap,is.catch.flag=F) # plot discard Neff
plot.francis.fleet(asap,is.catch.flag=F)
plot.indices.4panel(asap)
plot.index.age.comp.resids(asap)
plot.index.Neff(asap)
plot.francis.index(asap)


#--- Model Results
plot.fleet.sel.blocks(asap)
plot.fleet.Fmult(asap)
plot.index.selectivities(asap)
   # 3/19/2013 (new)
plot_catch_curves_for_catch(asap, first.age=cc.age ) # cc.age is user-defined peak age (-999 is default)
# test to ensure at least one index has age composition data
if(sum(asap$control.parms$index.age.comp.flag) > 0) plot_catch_curves_for_index(asap, first.age=cc.age)

plot_catch_at_age_consistency(asap)
# test to ensure at least one index has age composition data
if(sum(asap$control.parms$index.age.comp.flag) > 0) plot_index_at_age_consistency(asap)
   # 3/19/2013 ( end new)

plot.catchability(asap, a1)
plot.SSB.F.trend(asap)
plot.all.biomass.types(asap)
plot.SSB.AA(asap)
plot.NAA(asap)
plot.recruitment.devs(asap)
plot.recr.ssb.yr(asap)
plot.SR.pred.line(asap)
plot.SARC.R.SSB(asap)     
 if (exists("a1")!=F)   plot.cv(asap, a1)
if(retro.flag==T) plot.retro.wrapper(asap.rts,asap)


#--- Reference Points
plot.yield.curves(asap, nyrs.ave) # plots YPR - SPR curves and prints table of values
plot.SPR.table(asap, nyrs.ave)  # prints table of SPR values
plot.exp.spawn(asap, nyrs.ave)  # calculates expected lifetime spawning under various F values
plot.annual.SPR.targets(asap)
msy <- plot.MSY.annual(asap) # returns matrix of annual MSY estimates from SR curve


#--- MCMC results (assume user only does 1 chain)
if (asap$options$do.mcmc>0)  plot.mcmc(asap, asap.name) 



#--- Input Data
plot.catch.by.fleet(asap)
plot.catch.age.comp.bubbles(asap)                  # (won't appear if all input ESS=0)
plot.catch.age.comp.bubbles(asap,is.catch.flag=F)  # dicards age comp (won't appear if all input ESS=0)
plot.index.input(asap)
plot.index.age.comp.bubbles(asap)
plot.waa.matrices(asap)
plot.M(asap)
plot.maturity(asap)


write.RMSE.multipliers
#------------------------------------
#------  PDF Files  ---------------

# option to generate pdfs
if (make.one.pdf==T) {
graphics.off()

pdf(file=paste(od,asap.name,".ALL.PLOTS.pdf",sep=""), onefile=T)

save.plots=F
plot.main.likelihoods(asap)
a1<-grab.aux.files(asap.name, asap)
     if (exists("a1")!=F) p1b<-plot.high.corr(a1$asap.cor.mat, correlation.limit)
     if (exists("a1")!=F) npar<-a1$npar     
     if (exists("a1")!=F) max.grad<-a1$max.grad

#plot.additional.likelihoods(asap,seq(5,18))
plot.RMSE.table(asap)
plot.RMSE.95CI(asap)   #plot RMSE with 95% CI
plot.catch.4.panel(asap)
plot.discard.4.panel(asap)
plot.catch.age.comp(asap)     #fits to annual observed comp
plot.catch.age.comp.resids(asap)  #bubble plots
plot.catch.age.comp(asap,is.catch.flag=F) # plot discard age comp
plot.catch.age.comp.resids(asap,is.catch.flag=F) # plot discard age comp residuals
plot.fleet.Neff(asap)
plot.francis.fleet(asap,is.catch.flag=T)
plot.fleet.Neff(asap,is.catch.flag=F) # plot discard Neff
plot.francis.fleet(asap,is.catch.flag=F)
plot.indices.4panel(asap)
plot.index.age.comp.resids(asap)
plot.index.Neff(asap)
plot.francis.index(asap)


#write.RMSE.multipliers(asap)

#--- Results
plot.fleet.sel.blocks(asap)
plot.fleet.Fmult(asap)
plot.index.selectivities(asap)

   # 3/19/2013 (new)
plot_catch_curves_for_catch(asap, first.age=cc.age ) # cc.age is user-defined peak age (-999 is default)
# test to ensure at least one index has age composition data
if(sum(asap$control.parms$index.age.comp.flag) > 0) plot_catch_curves_for_index(asap, first.age=cc.age)

plot_catch_at_age_consistency(asap)
# test to ensure at least one index has age composition data
if(sum(asap$control.parms$index.age.comp.flag) > 0) plot_index_at_age_consistency(asap)
   # 3/19/2013 ( end new)

plot.catchability(asap, a1)
plot.SSB.F.trend(asap)
plot.all.biomass.types(asap)
plot.SSB.AA(asap)
plot.NAA(asap)
plot.recruitment.devs(asap)
plot.recr.ssb.yr(asap)
plot.SR.pred.line(asap)
plot.SARC.R.SSB(asap)     
 if (exists("a1")!=F)   plot.cv(asap, a1)
if(retro.flag==T) plot.retro.wrapper(asap.rts,asap)


#--- Reference Points
plot.yield.curves(asap, nyrs.ave) # plots YPR - SPR curves and prints table of values
plot.SPR.table(asap, nyrs.ave)  # prints table of SPR values
plot.exp.spawn(asap, nyrs.ave)  # calculates expected lifetime spawning under various F values
plot.annual.SPR.targets(asap)
msy <- plot.MSY.annual(asap) # returns matrix of annual MSY estimates from SR curve

#--- MCMC results (assume user only does 1 chain)
if (asap$options$do.mcmc>0)   plot.mcmc(asap, asap.name)  

#--- Input Data
plot.catch.by.fleet(asap)
plot.catch.age.comp.bubbles(asap)                  # (won't appear if all input ESS=0)
plot.catch.age.comp.bubbles(asap,is.catch.flag=F)  # dicards age comp (won't appear if all input ESS=0)
plot.index.input(asap)
plot.index.age.comp.bubbles(asap)
plot.waa.matrices(asap)
plot.M(asap)
plot.maturity(asap)


dev.off()      
graphics.off()
}  # end make.one.pdf

#--------------------------------------------------
#--------Now make individual pdfs------------------
#_________DIAGNOSTIC PLOTS___________________


graphics.off()
pdf(file=paste(od,asap.name,".DIAGNOSTIC.PLOTS.pdf",sep=""), onefile=T)

save.plots=F
plot.main.likelihoods(asap)
a1<-grab.aux.files(asap.name, asap)
     if (exists("a1")!=F) p1b<-plot.high.corr(a1$asap.cor.mat, correlation.limit)
     if (exists("a1")!=F) npar<-a1$npar     
     if (exists("a1")!=F) max.grad<-a1$max.grad

#plot.additional.likelihoods(asap,seq(5,18))
plot.RMSE.table(asap)
plot.RMSE.95CI(asap)   #plot RMSE with 95% CI
plot.catch.4.panel(asap)
plot.discard.4.panel(asap)
plot.catch.age.comp(asap)     #fits to annual observed comp
plot.catch.age.comp.resids(asap)  #bubble plots
plot.catch.age.comp(asap,is.catch.flag=F) # plot discard age comp
plot.catch.age.comp.resids(asap,is.catch.flag=F) # plot discard age comp residuals
plot.fleet.Neff(asap)
plot.francis.fleet(asap,is.catch.flag=T)
plot.fleet.Neff(asap,is.catch.flag=F) # plot discard Neff
plot.francis.fleet(asap,is.catch.flag=F)
plot.indices.4panel(asap)
plot.index.age.comp.resids(asap)
plot.index.Neff(asap)
plot.francis.index(asap)

dev.off()      
graphics.off()

#_________RESULTS___________________


graphics.off()
windows()
pdf(file=paste(od,asap.name,".RESULTS.PLOTS.pdf",sep=""), onefile=T)

plot.fleet.sel.blocks(asap)
plot.fleet.Fmult(asap)
plot.index.selectivities(asap)

   # 3/19/2013 (new)
plot_catch_curves_for_catch(asap, first.age=cc.age ) # cc.age is user-defined peak age (-999 is default)
# test to ensure at least one index has age composition data
if(sum(asap$control.parms$index.age.comp.flag) > 0) plot_catch_curves_for_index(asap, first.age=cc.age)

plot_catch_at_age_consistency(asap)
# test to ensure at least one index has age composition data
if(sum(asap$control.parms$index.age.comp.flag) > 0) plot_index_at_age_consistency(asap)
   # 3/19/2013 ( end new)
   
plot.catchability(asap, a1)
plot.SSB.F.trend(asap)
plot.all.biomass.types(asap)
plot.SSB.AA(asap)
plot.NAA(asap)
plot.recruitment.devs(asap)
plot.recr.ssb.yr(asap)
plot.SR.pred.line(asap)
plot.SARC.R.SSB(asap)     
 if (exists("a1")!=F)   plot.cv(asap, a1)
if(retro.flag==T) plot.retro.wrapper(asap.rts,asap)

dev.off()      
graphics.off()


#_________  MCMC   ___________________
if (asap$options$do.mcmc>0) {
graphics.off()
windows()
pdf(file=paste(od,asap.name,".MCMC.PLOTS.pdf",sep=""), onefile=T)

plot.mcmc(asap, asap.name) 

dev.off()      
graphics.off()

}

#_________REFERENCE POINTS___________________

graphics.off()
windows()
pdf(file=paste(od,asap.name,".REF.POINTS.PLOTS.pdf",sep=""), onefile=T )
plot.yield.curves(asap, nyrs.ave) # plots YPR - SPR curves and prints table of values
plot.SPR.table(asap, nyrs.ave)  # prints table of SPR values
plot.exp.spawn(asap, nyrs.ave)  # calculates expected lifetime spawning under various F values
plot.annual.SPR.targets(asap)
msy <- plot.MSY.annual(asap) # returns matrix of annual MSY estimates from SR curve

dev.off()      
graphics.off()


#_________INPUTS___________________

graphics.off()
windows()
pdf(file=paste(od,asap.name,".DATA.PLOTS.pdf",sep=""), onefile=T)

plot.catch.by.fleet(asap)
plot.catch.age.comp.bubbles(asap)                #   (won't appear if all input ESS=0)
plot.catch.age.comp.bubbles(asap,is.catch.flag=F)  # dicards age comp (won't appear if all input ESS=0)
plot.index.input(asap)
plot.index.age.comp.bubbles(asap)
plot.waa.matrices(asap)
plot.M(asap)
plot.maturity(asap)

dev.off()      
graphics.off()








