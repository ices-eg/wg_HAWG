#-------------------------------------------------------------------------------
# STF visualizer
#
# 04/04/2020 Martin Pastoors
#-------------------------------------------------------------------------------

library(FLCore)
library(FLSAM)
library(minpack.lm)  # install.packages("minpack.lm")
library(msm)         # install.packages("msm")
library(tidyverse)

#Read in data                           1

# path <- "D:/Repository/HAWG/wg_HAWG.git/trunk/NSAS/"
# path <- "C:/DATA/GIT/HAWG/NSAS/"
# path <- "D:/Repository/ICES_HAWG/wg_HAWG/NSAS/"
# path <- "D:/git/wg_HAWG/NSAS/"

# try(setwd(path),silent=FALSE)
y1 <- 2018
y2 <- 2019
y3 <- 2020

resy1 <-
  get(load("NSAS/results/stf/2018/NSAS_stf_2018_FmsyAR.RData"))[["stf"]]

resy2 <- 
  get(load("NSAS/results/stf/2019/NSAS_stf_2019_FmsyAR.RData"))[["stf"]]

resy3 <- 
  get(load("NSAS/results/stf/NSAS_stf_2020_FmsyAR.RData"))[["stf"]]

resy1.df <- as.data.frame(resy1) %>% mutate(stf=ac(y1))
resy2.df <- as.data.frame(resy2) %>% mutate(stf=ac(y2))
resy3.df <- as.data.frame(resy3) %>% mutate(stf=ac(y3))

ssby1    <- as.data.frame(resy1@stock.n * resy1@stock.wt * resy1@mat) %>% mutate(stf=ac(y1))
ssby2    <- as.data.frame(resy2@stock.n * resy2@stock.wt * resy2@mat) %>% mutate(stf=ac(y2))
ssby3    <- as.data.frame(resy3@stock.n * resy3@stock.wt * resy3@mat) %>% mutate(stf=ac(y3))

stocky1  <- as.data.frame(resy1@stock.n * resy1@stock.wt) %>% mutate(stf=ac(y1))
stocky2  <- as.data.frame(resy2@stock.n * resy2@stock.wt) %>% mutate(stf=ac(y2))
stocky3  <- as.data.frame(resy3@stock.n * resy3@stock.wt) %>% mutate(stf=ac(y3))

catchy1  <- as.data.frame(resy1@catch.n * resy1@catch.wt) %>% mutate(stf=ac(y1))
catchy2  <- as.data.frame(resy2@catch.n * resy2@catch.wt) %>% mutate(stf=ac(y2))
catchy3  <- as.data.frame(resy3@catch.n * resy3@catch.wt) %>% mutate(stf=ac(y3))

harvesty1  <- as.data.frame(resy1@harvest) %>% mutate(stf=ac(y1))
harvesty2  <- as.data.frame(resy2@harvest) %>% mutate(stf=ac(y2))
harvesty3  <- as.data.frame(resy3@harvest) %>% mutate(stf=ac(y3))

# plot SSB at age - lines
bind_rows(ssby2,ssby3) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_line(aes(colour=stf)) +
  facet_wrap(~age)

# plot stockbiomass at age - lines
bind_rows(stocky2,stocky3) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_line(aes(colour=stf)) +
  facet_wrap(~age)

# plot stockbiomass at age - bars
bind_rows(stocky1,stocky2,stocky3) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_bar(aes(fill=as.character(age)), stat="identity") +
  labs(title="stock", fill="age (WR)") +
  facet_wrap(~stf)

# plot harvest at age - line
bind_rows(resy2.df, resy3.df) %>%
  filter(unit=="A") %>% 
  filter(slot=="harvest") %>% 
  ggplot(aes(x=year, y=data)) +
  theme_bw() +
  geom_line(aes(colour=factor(age))) +
  labs(title="harvest", fill="age (WR)") +
  facet_wrap(~stf)

# plot catch biomass at age - line
bind_rows(catchy2,catchy3) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_line(aes(colour=stf)) +
  facet_wrap(~age)

# plot catch biomass at age - bar
bind_rows(catchy1, catchy2,catchy3) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_bar(aes(fill=as.character(age)), stat="identity", colour="white") +
  labs(title="catch", fill="age (WR)") +
  facet_wrap(~stf)

# plot catch biomass at age - percentage bar
bind_rows(catchy2,catchy3) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_bar(aes(fill=as.character(age)), stat="identity", position="fill", colour="white") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="catch") +
  facet_wrap(~stf)

# catch weight age age - line
bind_rows(mutate(as.data.frame(resy2@catch.wt), stf=ac(y2)),
          mutate(as.data.frame(resy3@catch.wt), stf=ac(y3)) ) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_line(aes(group=age, colour=as.character(age))) +
  labs(title="catch weight A fleet") +
  facet_wrap(~stf)

# catch number at age - line
bind_rows(mutate(as.data.frame(resy2@catch.n), stf=ac(y2)), 
          mutate(as.data.frame(resy3@catch.n), stf=ac(y3))) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  expand_limits(y=0) +
  geom_line(aes(colour=stf)) +
  facet_wrap(~age, scales="free_y")

# plot harvest at age - lines
bind_rows(harvesty1,harvesty2,harvesty3) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=age)) +
  theme_bw() +
  geom_line(aes(colour=factor(age))) +
  labs(title="F", colour="age (WR)") +
  facet_wrap(~stf)
