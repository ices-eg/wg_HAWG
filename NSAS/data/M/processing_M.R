rm(list=ls())

library(ggplot2)

path <- "J:/git/wg_HAWG/NSAS/"
#path <- "C:/git/2020_her.27.3a47d_multifleet/"
#path <- "C:/git/2020_her.27.3a47d_assessment/"
#path <- "C:/git/2020_her.27.3a47d_forecast/"
#path <- "D:/git/wg_HAWG/NSAS/"
try(setwd(path),silent=TRUE)

data.dir            <-  file.path(".","data/")              # result directory

M2_smooth_2016    <- read.csv(file.path(data.dir,"M","M_NSAS_smoothedSpan50_notExtrapolated_2016.csv"),header=TRUE,check.names = FALSE)
M2_raw_2016       <- read.csv(file.path(data.dir,"M","M_NSAS_raw_notExtrapolated_2016.csv"),header=TRUE,check.names = FALSE)
M2_raw_2021       <- read.csv(file.path(data.dir,"M","M_NSAS_raw_notExtrapolated_2019.csv"),header=TRUE,check.names = FALSE)

M2_raw_2021 <- gather(M2_raw_2021,'year','values',-age)
M2_raw_2021$year  <- as.numeric(M2_raw_2021$year)
M2_raw_2021$run   <- '2021'

M2_raw_2016 <- gather(M2_raw_2016,'year','values',-age)
M2_raw_2016$year <- as.numeric(M2_raw_2016$year)
M2_raw_2016$run   <- '2016'
M2_raw_2016$values <- M2_raw_2016$values - 0.1

M2_raw_all <- rbind(M2_raw_2016,M2_raw_2021)


ggplot(M2_raw_all,aes(x=year,y=values,color=run))+
  geom_line()+
  expand_limits(y=0)+
  xlim(1974,2020)+
  facet_wrap(~age,scales='free')
