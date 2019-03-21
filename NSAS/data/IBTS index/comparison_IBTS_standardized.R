rm(list=ls())
graphics.off()

library(FLSAM)
library(FLEDA)
library(FLFleet)

# local path
#path <- "D:/Repository/ICES_HAWG/wg_HAWG/NSAS/"
path <- "C:/git/wg_HAWG/NSAS/"
try(setwd(path),silent=TRUE)

dataIBTSQ1 <- file.path(".","data/IBTS index/IBTSQ1")
dataIBTSQ3 <- file.path(".","data/IBTS index/IBTSQ3")

resPath <- file.path(".","data/IBTS index")

outputName <- 'IBTS_comparison_'

PDF <- FALSE
PNG <- ifelse(PDF,F,T)
if(PDF) pdf(file.path(resPath,paste0(outputName,".pdf")))
if(PNG) png(file.path(resPath,paste0(outputName,"_%02d.png")),
            units = "px",
            height=800,
            width=672,
            bg = "white")

oldDataIBTSQ <- file.path(".","data/IBTS index/old_standardized_index/2018/")

IBTSQ1_new <- read.table(file.path(dataIBTSQ1,'IBTSQ1_output.txt'))
IBTSQ1_new <- IBTSQ1_new[,2:7]
rownames(IBTSQ1_new) <- ac(1984:2019)
colnames(IBTSQ1_new) <- ac(1:6)
IBTSQ3_new <- read.table(file.path(dataIBTSQ3,'IBTSQ3_output.txt'))
IBTSQ3_new <- IBTSQ3_new[,2:8]
rownames(IBTSQ3_new) <- ac(1998:2018)
colnames(IBTSQ3_new) <- ac(0:6)

IBTSQ1_old <- read.table(file.path(oldDataIBTSQ,'IBTSQ1_output.txt'))
IBTSQ1_old <- IBTSQ1_old[,2:7]
rownames(IBTSQ1_old) <- ac(1984:2018)
colnames(IBTSQ1_old) <- ac(1:6)
IBTSQ3_old <- read.table(file.path(oldDataIBTSQ,'IBTSQ3_output.txt'))
IBTSQ3_old <- IBTSQ3_old[,2:8]
rownames(IBTSQ3_old) <- ac(1998:2017)
colnames(IBTSQ3_old) <- ac(0:6)

### plotting IBTSQ1

ageIBTSQ1   <- an(colnames(IBTSQ1_old))
yearsIBTSQ1_new <- an(rownames(IBTSQ1_new))
yearsIBTSQ1_old <- an(rownames(IBTSQ1_old))

#par(mfrow=c(3,2))
for(cAge in ageIBTSQ1){
  plot(yearsIBTSQ1_new,IBTSQ1_new[,ac(cAge)],type='l',col='blue',ylab=paste0('standardized index - age ',cAge),xlab='years')
  lines(yearsIBTSQ1_old,IBTSQ1_old[,ac(cAge)],type='l',col='green')
  legend(1990,mean(IBTSQ1_new[,ac(cAge)]), legend=c("2019", "2018"),
         col=c("blue", "green"), lty=1:2, cex=0.8)
}

### plotting IBTSQ3

ageIBTSQ3   <- an(colnames(IBTSQ3_old))
yearsIBTSQ3_new <- an(rownames(IBTSQ3_new))
yearsIBTSQ3_old <- an(rownames(IBTSQ3_old))

#par(mfrow=c(4,2))
for(cAge in ageIBTSQ3){
  plot(yearsIBTSQ3_new,IBTSQ3_new[,ac(cAge)],type='l',col='blue',ylab=paste0('standardized index - age ',cAge),xlab='years')
  lines(yearsIBTSQ3_old,IBTSQ3_old[,ac(cAge)],type='l',col='green')
  legend(2000,mean(IBTSQ3_new[,ac(cAge)]), legend=c("2019", "2018"),
         col=c("blue", "green"), lty=1:2, cex=0.8)
}




dev.off()
