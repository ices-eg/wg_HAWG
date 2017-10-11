

`
tr<-readRDS(paste(dir_data, "tr_her.rds", sep=""))
hh<-readRDS(paste(dir_data, "hh_her.rds", sep=""))
sl<-readRDS(paste(dir_data, "sl_her.rds", sep=""))
hl<-readRDS(paste(dir_data, "hl_her.rds", sep=""))
ca<-readRDS(paste(dir_data, "ca_her.rds", sep=""))

trhhslhl<-inner_join(inner_join(inner_join(tr,hh),sl),hl)
trhhslhl<-subset(trhhslhl, year>2008)


##Add fleets

```{r}
trhhslhlf<-  mutate(trhhslhl, fleet = ifelse(substr(foCatEu6,1,3) %in% c("GNS","GTR"), "Gillnet",
                                             ifelse(substr(foCatEu6,1,3) %in% c("FPN","FYK","FPO"), "Fyke", 
                                                    ifelse(substr(foCatEu6,1,2) %in% c("PS","SB"), "Purse",
                                                           ifelse(substr(foCatEu6,9,10) %in% c("16"), "Trawl<32",
                                                                  ifelse(substr(foCatEu6,9,11) %in% c("<16"), "Trawl<32",
                                                                         ifelse(substr(foCatEu6,9,10) > 31 & substr(foCatEu6,5,7)=="SPF", "Trawl_SPF>=32", 
                                                                                ifelse(substr(foCatEu6,9,10) > 31 & substr(foCatEu6,5,7)!="SPF", "Trawl_OTH>=32",
                                                                                       ifelse(substr(foCatEu6,11,13) %in% c("105","120") & substr(foCatEu6,5,7)!="SPF", "Trawl_OTH>=32", "NA")))))))))