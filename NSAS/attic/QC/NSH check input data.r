################################################################################
# NSH_SAM Assessment; checking input and output data
#
# Install FLR packages (including FLSAM and FLEDA)
# source("http://flr-project.org/R/instFLR.R")
#
# 15/03/2017: First coding
# 14/03/2018: updated for HAWG 2018
################################################################################

rm(list=ls())

#Import externals
library(FLSAM); 
library(FLEDA)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(directlabels)
library(scales)

options(max.print=999999)


### ============================================================================
### current assessment
### ============================================================================
path    <- "D:/GIT/wg_HAWG/NSAS/"

try(setwd(path),silent=TRUE)

# Read current assessment
source(file.path("setupAssessmentObjects.r"))

# Load crayola code
source(file.path("..","_Common","crayola.r"))

# Load publication theme
source("file:///D:/XXX/PRF/r/theme_publication.r")

# Load the assessment results file
# load(file.path("results/HAWG2018.RData"))

### ============================================================================
### Read last years data
### ============================================================================

source(file.path("D:/HAWG/2018/09. Personal Folders/MartinP/R/setupAssessmentObjects lastyear.r"))

### ============================================================================
### Compare datasets
### ============================================================================

# min(ssb(trim(NSH, year=1996:2016)))
# fbar(trim(NSH, year=1996:2016, age=2:6))
# stk@stock.n

# compare stock objects

rm(t1, t2)

# new assessment
for(i in c("catch","catch.n","catch.wt", "stock.wt","m","mat","m.spwn") ) {
  print(i)
  
  tmp <- 
    as.data.frame(trim(slot(NSH,i),     year=1947:2017)) %>% 
    mutate(age = as.character(age),
           var = i) %>% 
    dplyr::select(var, age, year, area, data) 
  
  if(i == "catch") { t1 <- tmp } 
  else             { t1 <- rbind(t1, tmp)}
}

# Old assessment
for(i in c("catch","catch.n","catch.wt", "stock.wt","m","mat","m.spwn") ) {
  print(i)
  
  tmp <- 
    as.data.frame(trim(slot(NSH_old,i),     year=1947:2016)) %>% 
    mutate(age = as.character(age),
           var = i) %>%
    rename(data_old = data) %>% 
    dplyr::select(var, age, year, area, data_old) 
  
  if(i == "catch") { t2 <- tmp } 
  else             { t2 <- rbind(t2, tmp)}
}


stock <-
  t1 %>% 
  full_join(t2,by=c("var", "age","year", "area")) %>% 
  mutate(diff  = data_old - data) %>% 
  mutate(pdiff = (data_old - data)/data_old) 

stock %>% 
  ggplot(aes(year, pdiff)) +
  theme_bw() +
  geom_hline(aes(yintercept = 0), colour="blue", size=0.2) +
  geom_line() +
  facet_grid(age~var) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.5, 0.5), breaks=c(-0.5,0,0.5)) 


# -----------------------------------------------------------------------------

rm(t1)

for(i in (1:length(names(NSH.tun)))) {
  print(paste(i, names(NSH.tun)[i]))

  tmp <- 
    as.data.frame(slot(NSH.tun[[i]],"index")) %>% 
    mutate(age = as.character(age),
           var = names(NSH.tun)[i]) %>% 
    dplyr::select(var, age, year, area, data) 
  
  if(i == 1) { t1 <- tmp } 
  else       { t1 <- rbind(t1, tmp)}
}

rm(t2)


for(i in (1:length(names(NSH_old.tun)))) {
  print(paste(names(NSH_old.tun)[i]))
  
  tmp <- 
    as.data.frame(slot(NSH_old.tun[[i]],"index")) %>% 
    rename(data_old=data) %>% 
    mutate(age = as.character(age),
           var = names(NSH_old.tun)[i]) %>%  
    mutate(data_old= ifelse(var == "IBTS-Q1", 
                            data_old*1000, 
                            data_old)) %>%
    dplyr::select(var, age, year, area, data_old) 
  
  if(i == 1) { t2 <- tmp } 
  else       { t2 <- rbind(t2, tmp)}
}


# Compare tuning series
tun <-
  t1 %>% 
  full_join(t2,by=c("var", "age","year","area")) %>% 
  mutate(diff  = data_old - data,
         pdiff = (data_old - data)/data_old) 

tun %>% 
  filter(var %in% c("HERAS","IBTS-Q1","IBTS-Q3", "IBTS0")) %>% 
  
  ggplot(aes(year, pdiff)) +
  theme_bw() +
  geom_hline(aes(yintercept = 0), colour="blue", size=0.2) +
  geom_line() +
  facet_grid(age~var) +
  scale_y_continuous(labels = scales::percent, limits = c(-1.0, 1.0), breaks=c(-1,0,1)) 


filter(tun, var == "IBTS-Q1") %>% View()




# 2018-314 NOT CHANGED FROM HERE DOWN
# -----------------------------------------------------------------------------

# Compare SCAI index
t1 <- as.data.frame(slot(SCAI.idx[[1]] ,"index"))
t2 <- as.data.frame(slot(SCAI.idx[[1]],"index"))

t <-
  t1 %>% 
  full_join(t2,by=c("age","year","unit", "season","area","iter")) %>% 
  mutate(diff  = data.y - data.x,
         pdiff = (data.y - data.x)/data.y ) 

plot(t$year, t$pdiff)

plot(slot(NSH,"catch.wt"))
plot(slot(NSH,"stock.wt"))
plot(slot(NSH,"catch.n"))



# Crayola catch data
p1 <-
  crayola(
  dplyr::select(as.data.frame(trim(slot(NSH,"catch.n"), year=1989:2016, age=0:8)), year, age, number = data) %>%
           filter(number != -1),
         t = "North Sea herring catch at age")

# Crayola HERAS survey
crayola (dplyr::select(as.data.frame(NSH.tun[["HERAS"]]@index),
                year, age, number = data) %>%
           filter(number != -1),
         t = "North Sea herring HERAS survey")

# Crayola stock numbers
crayola (dplyr::select(as.data.frame(trim(slot(NSH,"stock.n"), year=1989:2016)),
                       year, age, number = data) %>%
           filter(number != -1),
         t = "North Sea herring stock numbers at age")


d <- dplyr::select(as.data.frame(NSH.tun[["HERAS"]]@index),
              year, age, number = data) %>% filter(number != -1)
t <- "HERAS"

d %>% 
  filter(age %in% 4:7) %>% 
  group_by(age) %>% 
  mutate(prop = number/max(number, na.rm=T)) %>% 
  ggplot(aes(year,prop, group=age)) +
  geom_line(aes(colour=factor(age))) +
  facet_wrap(~age)


# plot catch weight at age
as.data.frame(slot(NSH,"catch.wt")) %>% 
  mutate(decade = 10 * (year %/% 10 )) %>% 
  
  ggplot(aes(year,data)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_point(aes(colour=factor(decade))) +
  geom_line(aes(colour=factor(decade)), size=0.5) +
  facet_wrap(~age) 


# plot catch weight at age 
as.data.frame(slot(NSH,"catch.wt")) %>% 
  mutate(decade = 10 * (year %/% 10 )) %>% 
  filter(year >= 1980) %>% 
  filter(age %in% 6:8) %>% 
  group_by(decade, year) %>% 
  summarize(weight = mean(data, na.rm=T)) %>% 
  
  ggplot(aes(year,weight)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_line() +
  
#  geom_point(aes(colour=factor(decade))) +
#  geom_line(aes(colour=factor(decade)), size=0.5) +
  expand_limits(y=0)


load("E:/2017_VIaHerring.Rdata")

p2 <-
  crayola (dplyr::select(as.data.frame(trim(slot(MSH,"catch.n"),  year=1989:2016, age=1:8)),
                year, age, number = data) %>%
           filter(number != -1),
         t = "Malin Shelf herring catch at age")

multiplot(p1, p2, cols=2)

plot(slot(MSH, "catch"))

# Comparing data from different assessments

load("D:/GIT/HAWG/NSAS/results/North Sea Herring.RData");     
load("D:/GIT/HAWG/VIa/results/Final_2017_VIaHerring.Rdata"); 
load("D:/GIT/HAWG/WBSS/WBSS_HAWG2017.RData");                 
load("W:/2017 Meeting docs/06. Data/ISH/ISH_assessment 2017.RData")

ish.sr <- as.FLSR(NSH)
plot(ISH)
model(ish.sr)<-bevholt()
plot(ish.sr)

# Canum
NSH.canum  <- trim(slot(NSH     , "catch.n"), year=1980:2016, age=1:7)
MSH.canum  <- trim(slot(MSH     , "catch.n"), year=1980:2016, age=1:7)
IS.canum   <- trim(slot(ISH     , "catch.n"), year=1980:2016, age=1:7) 
CSH.canum  <- trim(readVPAFile("D:/HAWG/2017/06. Data/CSH/canum.txt"), year=1980:2016, age=1:7)

MSH.stock  <- trim(slot(MSH     , "stock.n"), year=1980:2016, age=1:7)
IS.stock   <- trim(slot(ISH     , "stock.n"), year=1980:2016, age=1:7) 
CSH.stock  <- trim(readVPAFile("D:/HAWG/2017/06. Data/CSH/canum.txt"), year=1980:2016, age=1:7)

all.canum  <- MSH.canum + IS.canum + CSH.canum
MSH.stock.n  <- slot(MSH     , "stock.n")

crayola (dplyr::select(as.data.frame(all.canum),
                       year, age, number = data) %>%
           filter(number != -1),
         t = "All west herring catch at age")

segreg(ISH)
data(ISH)


p2 <-
  crayola (dplyr::select(as.data.frame(NSH.canum + MSH.canum + CSH.canum),
                         year, age, number = data) %>%
             filter(number != -1),
           t = "All herring catch at age")

# Mean weight in the catch for age 6-8
NSH.catch.wt  <- slot(NSH     , "catch.wt")
MSH.catch.wt  <- slot(MSH     , "catch.wt")
WBSS.catch.wt <- slot(wbss.stk, "catch.wt") 
IS.catch.wt   <- slot(ISH     , "catch.wt") 
CSH.catch.wt  <- readVPAFile("D:/HAWG/2017/06. Data/CSH/weca.txt")

catch.wt <-
        mutate(as.data.frame(NSH.catch.wt) , stock="NSAS") %>% 
  rbind(mutate(as.data.frame(MSH.catch.wt) , stock="MSH")) %>% 
  rbind(mutate(as.data.frame(IS.catch.wt)  , stock="ISH")) %>% 
  rbind(mutate(as.data.frame(WBSS.catch.wt), stock="WBSS")) %>% 
  rbind(mutate(as.data.frame(CSH.catch.wt) , stock="CSH"))

catch.wt %>% 
  group_by(stock, year) %>% 
  filter(age %in% 6:9) %>% 
  summarize(avgweight = mean(data, na.rm=T)) %>% 
  
  ggplot(aes(year, avgweight)) +
  theme_publication() +
  geom_point(aes(colour=factor(stock))) +
  geom_smooth(aes(colour=factor(stock))) +
  facet_wrap(~stock, scales="free_y") +
  labs(title="Mean weight in the catch (6-8)")



stock.wt %>% 
  group_by(stock, year) %>% 
  filter(age %in% 6:9) %>% 
  summarize(avgweight = mean(data, na.rm=T)) %>% 
  View()


# Plot Mean weight in the stock for ages 6-8
NSH.stock.wt  <- slot(NSH     , "stock.wt")
MSH.stock.wt  <- slot(MSH     , "stock.wt")
WBSS.stock.wt <- slot(wbss.stk, "stock.wt") 
IS.stock.wt   <- slot(ISH     , "stock.wt") 
CSH.stock.wt  <- readVPAFile("D:/HAWG/2017/06. Data/CSH/west.txt")

stock.wt <-
        mutate(as.data.frame(NSH.stock.wt) , stock="NSAS") %>% 
  rbind(mutate(as.data.frame(MSH.stock.wt) , stock="MSH")) %>% 
  rbind(mutate(as.data.frame(IS.stock.wt)  , stock="ISH")) %>% 
  rbind(mutate(as.data.frame(WBSS.stock.wt), stock="WBSS")) %>% 
  rbind(mutate(as.data.frame(CSH.stock.wt) , stock="CSH"))

stock.wt %>% 
  group_by(stock, year) %>% 
  filter(age %in% 6:9) %>% 
  summarize(avgweight = mean(data, na.rm=T)) %>% 
  
  ggplot(aes(year, avgweight)) +
  theme_publication() +
  geom_point(aes(colour=factor(stock))) +
  geom_smooth(aes(colour=factor(stock))) +
  facet_wrap(~stock, scales="free_y") +
  labs(title="Weight in the stock (6-8)")

trim(MSH.stock.wt, age=6:8)
stock.wt %>% 
  group_by(stock, year) %>% 
  filter(age %in% 6:9) %>% 
  summarize(avgweight = mean(data, na.rm=T)) %>% 
  View()


# plot SSB trends
WBSS.ssb  <- ssb(wbss)
MSH.ssb   <- ssb(MSH)
ssb <-
  mutate(as.data.frame(WBSS.ssb), stock="WBSS") %>%
  rbind(mutate(as.data.frame(MSH.ssb), stock="MSH")) 

ssb %>% 
  ggplot(aes(year, data)) +
  theme_publication() +
  geom_line(aes(colour=factor(stock))) +
  facet_wrap(~stock, scales="free_y") +
  expand_limits(y = 0)


  
# plot harvest for NSAS

dplyr::select(as.data.frame(trim(slot(NSH,"harvest"), year=1980:2016)), year, age, number = data) %>%
  filter(number != -1) %>% 
  mutate(group = ifelse(age %in% 0:1, "0-1", NA),
         group = ifelse(age %in% 2:6, "2-6", group)) %>% 
  group_by(year, group) %>% 
  filter(!is.na(group)) %>% 
  summarize(fmean = mean(number, na.rm=T)) %>% 
  
  ggplot(aes(year, fmean)) +
  theme_publication() +
  geom_line() +
  facet_wrap(~group, scales="free_y") +
  expand_limits(y = 0)

  
dplyr::select(as.data.frame(trim(slot(NSH,"harvest"), year=1980:2016)), year, age, number = data) %>%
  filter(number != -1) %>% 

  ggplot(aes(year, number)) +
  theme_publication() +
  geom_line() +
  facet_wrap(~age, scales="free_y") +
  expand_limits(y = 0)

slotNames(NSH)

# plot natural mortality
dplyr::select(as.data.frame(slot(NSH,"m")), year, age, number = data) %>%
  filter(number != -1) %>% 

  ggplot(aes(year, number, group=age)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_line(aes(colour=factor(age))) +
  expand_limits(y = 0) +
  geom_dl(aes(label=age, colour=factor(age)), method=list(dl.combine("last.points"), cex = 0.8)) 


# Explore the short term forecast
stf@stock.n

rn <- read.csv("D:/HAWG/2017/06. Data/NSAS/NSAS Stock data Richard Nash.csv")

rn %>% 
  mutate(decade = 10 * floor(Year.Class/10)) %>% 
  ggplot(aes(SSB, Recruits, group=decade)) +
  theme_publication() +
  geom_point(aes(colour=factor(decade)), size=2) +
  geom_line(aes(colour=factor(decade))) +
  geom_smooth(aes(SSB, Recruits), inherit.aes=FALSE) +
  expand_limits(x=0, y=0)


rn %>% 
  mutate(decade = 10 * floor(Year.Class/10)) %>%
  filter(Year.Class > 1980) %>% 
  
  ggplot(aes(SSB, Recruits, group=decade)) +
  theme_publication() +
  geom_point(aes(colour=factor(decade)), size=3) +
  geom_line(aes(colour=factor(decade))) +
  geom_smooth(aes(SSB, Recruits), inherit.aes=FALSE) +
  expand_limits(x=0, y=0)

rn %>% 
  mutate(decade = 10 * floor(Year.Class/10)) %>%
  filter(Year.Class > 1980) %>% 
  
  ggplot(aes(SSB, RecruitPerSpawner, group=decade)) +
  theme_publication() +
  geom_point(aes(colour=factor(decade)), size=3) +
  geom_line(aes(colour=factor(decade))) +
  geom_smooth(aes(SSB, RecruitPerSpawner), inherit.aes=FALSE) +
  expand_limits(x=0, y=0)

rn %>% 
  mutate(decade = 10 * floor(Year.Class/10),
         period = ifelse(Year.Class < 1987, "47-86",
                         ifelse(Year.Class<2003, "87-02","03-16"))) %>%

  ggplot(aes(Year.Class, RecruitPerSpawner, group=period)) +
  theme_publication() +
  geom_point(aes(colour=factor(period)), size=3) +
  geom_line(aes(colour=factor(period))) +
  geom_smooth(aes(Year.Class, RecruitPerSpawner), inherit.aes=FALSE) +
  expand_limits(y=0)
