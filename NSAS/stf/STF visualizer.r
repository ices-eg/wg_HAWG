#-------------------------------------------------------------------------------
# STF visualizer
#
# 04/04/2019 Martin Pastoors
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
path <- "D:/git/wg_HAWG/NSAS/"

try(setwd(path),silent=FALSE)

res2018 <- get(load("D:/GIT/wg_HAWG/NSAS/stf/results/STF2018 FmsyAR.RData"))[["stf"]]
res2019 <- get(load("D:/GIT/wg_HAWG/NSAS/stf/results/STF2019 FmsyAR.RData"))[["stf"]]

res2018.df <- as.data.frame(res2018) %>% mutate(stf="2018")
res2019.df <- as.data.frame(res2019) %>% mutate(stf="2019")

ssb2018    <- as.data.frame(res2018@stock.n * res2018@stock.wt * res2018@mat) %>% mutate(stf="2018")
ssb2019    <- as.data.frame(res2019@stock.n * res2019@stock.wt * res2019@mat) %>% mutate(stf="2019")

stock2018  <- as.data.frame(res2018@stock.n * res2018@stock.wt) %>% mutate(stf="2018")
stock2019  <- as.data.frame(res2019@stock.n * res2019@stock.wt) %>% mutate(stf="2019")

catch2018  <- as.data.frame(res2018@catch.n * res2018@catch.wt) %>% mutate(stf="2018")
catch2019  <- as.data.frame(res2019@catch.n * res2019@catch.wt) %>% mutate(stf="2019")

# plot SSB at age - lines
bind_rows(ssb2018,ssb2019) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_line(aes(colour=stf)) +
  facet_wrap(~age)

# plot stockbiomass at age - lines
bind_rows(stock2018,stock2019) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_line(aes(colour=stf)) +
  facet_wrap(~age)

# plot stockbiomass at age - bars
bind_rows(stock2018,stock2019) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_bar(aes(fill=as.character(age)), stat="identity") +
  labs(title="stock") +
  facet_wrap(~stf)

# plot harvest at age - line
bind_rows(res2018.df, res2019.df) %>%
  filter(unit=="A") %>% 
  filter(slot=="harvest") %>% 
  ggplot(aes(x=year, y=data)) +
  theme_bw() +
  geom_line(aes(colour=factor(age))) +
  labs(title="harvest") +
  facet_wrap(~stf)

# plot catch biomass at age - line
bind_rows(catch2018,catch2019) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_line(aes(colour=stf)) +
  facet_wrap(~age)

# plot catch biomass at age - bar
bind_rows(catch2018,catch2019) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_bar(aes(fill=as.character(age)), stat="identity", colour="white") +
  labs(title="catch") +
  facet_wrap(~stf)

# plot catch biomass at age - percentage bar
bind_rows(catch2018,catch2019) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_bar(aes(fill=as.character(age)), stat="identity", position="fill", colour="white") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="catch") +
  facet_wrap(~stf)

# catch weight age age - line
bind_rows(mutate(as.data.frame(res2018@catch.wt), stf="2018"),
          mutate(as.data.frame(res2019@catch.wt), stf="2019") ) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  geom_line(aes(group=age, colour=as.character(age))) +
  labs(title="catch weight A fleet") +
  facet_wrap(~stf)

# catch number at age - line
bind_rows(mutate(as.data.frame(res2018@catch.n), stf="2018"), 
          mutate(as.data.frame(res2019@catch.n), stf="2019")) %>% 
  filter(unit=="A") %>% 
  ggplot(aes(x=year, y=data, group=stf)) +
  theme_bw() +
  expand_limits(y=0) +
  geom_line(aes(colour=stf)) +
  facet_wrap(~age, scales="free_y")

