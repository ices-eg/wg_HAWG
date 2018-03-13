
##
#  script to compare the output of the assessment using the new Fprop and the 2017 assessment

### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA);library(ggplot2); library(grid)

rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

# local path
path <- "D:/git/wg_HAWG/NSAS/benchmark/"
try(setwd(path),silent=TRUE)

data.source <- file.path(".","data")    #Data source, not code or package source!!!

### ======================================================================================================
### Comparing time series IBTS0
### ======================================================================================================

#  compare the M vectors
#library(ggplotFL)
#M<-FLQuants(fit1.stck@m,fit2.stck@m, fit3.stck@m, fit4.stck@m)
#names(M) <- st.names
#ggplot(M , aes (x =year ,y =data  , colour = qname)) + geom_line() + facet_wrap(~age) + scale_colour_discrete(name = "ASSESSMENT")

########################## compare IBTSQ1 time series at all ages ##########################
IBTSQ0 <- readFLIndices(file.path(data.source, "fleet.txt"))
IBTSQ0_new <- readFLIndices(file.path(data.source, "fleet_3c_newIBTSQ3.txt"))

IBTSQ0 <- as.data.frame(IBTSQ0)
IBTSQ0 <- as.data.frame( IBTSQ0[IBTSQ0$cname == "IBTS0" & 
                                IBTSQ0$slot == "index" &
                                !is.na(IBTSQ0$data),])

IBTSQ0$cname <- rep("IBTS0_old",dim(IBTSQ0)[1])

IBTSQ0_new <- as.data.frame(IBTSQ0_new)
IBTSQ0_new <- as.data.frame( IBTSQ0_new[IBTSQ0_new$cname == "IBTS0" & 
                                        IBTSQ0_new$slot == "index" &
                                        !is.na(IBTSQ0_new$data),])

IBTSQ0_new$cname <- rep("IBTS0_new",dim(IBTSQ0_new)[1])

IBTS0_all <- rbind(IBTSQ0, IBTSQ0_new)

windows()
g <- ggplot(IBTS0_all, aes (x =year ,y =data)) + geom_line(aes(color = cname), size = 1) + xlab("year") + ylab("IBTS0 index")
g
savePlot(file.path(".","results","miscellaneous","new IBTS0.png"),type="png")

#dev.off()

########################### Plotting multi-fleet time series ##############################

library(ggplot2)
library(grid)
library(gridExtra)
caaF  <- read.table("./data/Historic ABCD-Fleet NSAS v2.txt",stringsAsFactors=F,header=T)
caA   <- matrix(subset(caaF,Fleet=="A")[,"numbers"],nrow=length(0:9),ncol=length(1997:2016),dimnames=list(age=0:9,year=1997:2016))*1000
caB   <- matrix(subset(caaF,Fleet=="B")[,"numbers"],nrow=length(0:9),ncol=length(1997:2016),dimnames=list(age=0:9,year=1997:2016))*1000
caC   <- matrix(subset(caaF,Fleet=="C")[,"numbers"],nrow=length(0:9),ncol=length(1997:2016),dimnames=list(age=0:9,year=1997:2016))*1000
caD   <- matrix(subset(caaF,Fleet=="D")[,"numbers"],nrow=length(0:9),ncol=length(1997:2016),dimnames=list(age=0:9,year=1997:2016))*1000
cwA   <- matrix(subset(caaF,Fleet=="A")[,"weight"],nrow=length(0:9),ncol=length(1997:2016),dimnames=list(age=0:9,year=1997:2016))
cwB   <- matrix(subset(caaF,Fleet=="B")[,"weight"],nrow=length(0:9),ncol=length(1997:2016),dimnames=list(age=0:9,year=1997:2016))
cwC   <- matrix(subset(caaF,Fleet=="C")[,"weight"],nrow=length(0:9),ncol=length(1997:2016),dimnames=list(age=0:9,year=1997:2016))
cwD   <- matrix(subset(caaF,Fleet=="D")[,"weight"],nrow=length(0:9),ncol=length(1997:2016),dimnames=list(age=0:9,year=1997:2016))

fleetType <- unique(caaF$Fleet)
temp <- caaF[caaF$Fleet == "A",]
temp$weight[temp$weight == 0] <- NA

count <- 1
g <- list()

#pl = list(qplot(1,1), qplot(2,2))
for(k in fleetType){
  temp <- caaF[caaF$Fleet == k,]
  temp$weight[temp$weight == 0] <- NA
  
  g[[count]] <- ggplot(data = temp , aes(x = year , y  = catch, colour = wr))
  g[[count]]   <-  g[[count]]  +  geom_line() + theme(legend.position="bottom") + ggtitle(paste("fleet-", k))
  g[[count]]
  count <- count + 1
  
}
#g[[count-1]] <- g[[count-1]] + theme()

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

grid_arrange_shared_legend(g[[1]], g[[2]], g[[3]], g[[4]], g[[5]])
savePlot(file.path(".","results","miscellaneous","fleets_time_series.png"),type="png")


#do.call(grid.arrange,g)

#grid.arrange(g, ncol(2))