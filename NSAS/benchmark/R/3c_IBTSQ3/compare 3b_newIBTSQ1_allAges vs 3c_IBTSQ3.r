
##
#  script to compare the output of the assessment using the new Fprop and the 2017 assessment

### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA);library(ggplot2); library(grid); library(FLEDA); library(FLBRP);

rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)


### ======================================================================================================
### Define parameters and paths for use in the assessment code
### ======================================================================================================

# choose the assessments to be compared
assess1 <- "3b_newIBTSQ1_allAges"
#assess1 <- "3a_newIBTSQ1_age1"

assess2 <- "3c_IBTSQ3"

# local path
#path <- "C:/Users/brune001/my git files/wg_HAWG/NSAS/benchmark/"
path <- "D:/git/wg_HAWG/NSAS/benchmark/"
try(setwd(path),silent=TRUE)

# paths
output.dir          <-  file.path(".","results/3c_IBTSQ3/")                #figures directory
output.base         <-  file.path(output.dir,"NSH Assessment")  #Output base filename, including directory. Other output filenames are built by appending onto this one
data.source <- file.path(".","data")    #Data source, not code or package source!!!

# load previous assessment in sequence
load(file.path(".","results",assess1,"NSH.RData")  )
fit1.stck  <-NSH
fit1.flsam <-NSH.sam

# load current assessment in sequence
load(file.path(".","results",assess2,"NSH.RData")  )
fit2.stck  <-NSH
fit2.flsam <-NSH.sam

plot(NSH.tun[["IBTS-Q1"]],type="internal")

#logLik(fit1.flsam)
#logLik(fit2)

#1-pchisq(2*(logLik(fit2)-logLik(fit1)),6)

#AIC(fit1)
#AIC(fit2)


### ======================================================================================================
### Comparing IBTSQ1 and Q3 time series
### ======================================================================================================

########################## compare IBTSQ1 time series at all ages ##########################
#data_surveys_old <- readFLIndices(file.path(data.source, "fleet.txt"))
#data_surveys <- readFLIndices(file.path(data.source, "fleet_3c_newIBTSQ3.txt"))

#data_IBTSQ3 <- as.data.frame(data_surveys)
#data_IBTSQ3 <- as.data.frame( data_IBTSQ3[data_IBTSQ3$cname == "IBTS-Q3" & 
#                                          data_IBTSQ3$slot == "index" &
#                                          !is.na(data_IBTSQ3$data),])

#data_IBTSQ1 <- as.data.frame(data_surveys)
#data_IBTSQ1 <- as.data.frame( data_IBTSQ1[data_IBTSQ1$cname == "IBTS-Q1" & 
#                                            data_IBTSQ1$slot == "index" &
#                                        !is.na(data_IBTSQ1$data),])

#data_IBTSQ1_old <- as.data.frame(data_surveys_old)
#data_IBTSQ1_old <- as.data.frame( data_IBTSQ1_old[data_IBTSQ1_old$cname == "IBTS-Q1" & 
#                                                  data_IBTSQ1_old$slot == "index" &
#                                                  !is.na(data_IBTSQ1_old$data),])

#data_HERAS <- as.data.frame(data_surveys)
#data_HERAS <- as.data.frame( data_HERAS[data_HERAS$cname == "HERAS" & 
#                                        data_HERAS$slot == "index" &
#                                        !is.na(data_HERAS$data),])

#write.csv(data_IBTSQ3, file.path(".","results",assess2,"data_IBTSQ3.csv"))
#write.csv(data_IBTSQ1, file.path(".","results",assess2,"data_IBTSQ1.csv"))
#write.csv(data_IBTSQ1_old, file.path(".","results",assess2,"data_IBTSQ1_old.csv"))
#write.csv(data_HERAS, file.path(".","results",assess2,"data_HERAS.csv"))

### ======================================================================================================
### plotting IBTSQ3 time series
### ======================================================================================================

########################## plot IBTSQ3 time series at all ages ##########################
#idxIBTSQ3 <- readFLIndices(file.path(data.source, "fleet_3c_newIBTSQ3.txt"))

#idxIBTSQ3 <- as.data.frame(idxIBTSQ3)
#idxIBTSQ3 <- as.data.frame( idxIBTSQ3[  idxIBTSQ3$cname == "IBTS-Q3" & 
#                                        idxIBTSQ3$slot == "index" &
#                                        !is.na(idxIBTSQ3$data),])
#png(filename = file.path(output.dir,paste("IBTSQ3 time series per age.png")), 
#    units = "px",
#    width = 672, height = 800)

#i <- 1
#g <- list()
#for(idxAge in unique(idxIBTSQ3$age)){
#  g[[i]] <- ggplot(idxIBTSQ3[idxIBTSQ3$age == idxAge,], aes(year, data)) + 
#            geom_line() + 
#            ggtitle(paste("age", idxAge)) +
#            ylab("")
#  
#  i <- i+1
#}
#
#
#grid.arrange(g[[1]], g[[2]], g[[3]], g[[4]], g[[5]], g[[6]], g[[7]])
#
#dev.off()

#####################################################################################################################

## open png print

png(file.path(output.dir,paste(name(NSH.sam),"figures_comparison - %02d.png")),units = "px", height=800,width=672, bg = "white")

########################## compare stock trajectories ##########################
st.names <- c(assess1,assess2)
stc <- FLStocks(fit1.stck,fit2.stck)
names(stc) <- st.names
flsam <- FLSAMs(fit1.flsam,fit2.flsam)
names(flsam) <- st.names

print(plot(stc))
#png(filename = file.path(output.dir,paste("comparison of stock trajectories.png")), 
#    units = "px",
#    width = 672, height = 800)
#dev.off()
#savePlot(file.path(".","results",assess2,"comparison of stock trajectories.png"),type="png")



### ======================================================================================================
### parameter values
### ======================================================================================================

################# catchability values ##########################
catch1 <- catchabilities(fit1.flsam)
catch2 <- catchabilities(fit2.flsam)
catch1$assess <- assess1
catch2$assess <- assess2

catchab<-rbind(catch1,catch2)
catchab$age[is.na(catchab$age)] <- "all"
catchab$label <- paste(catchab$fleet,catchab$age,sep="_")

g <- ggplot(data = catchab , aes(label , value , fill = assess))
g   <-  g  +  geom_bar(aes(fill = assess   ), position = "dodge", stat="identity")
g   <-  g  +  ggtitle("survey catchability")  + xlab("")  + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g   <-  g  +  geom_errorbar(aes(ymin=lbnd, ymax=ubnd),width=1, position=position_dodge(.9))
g   <-  g  +  facet_grid(fleet~.,scales = "free") + scale_colour_discrete(name = "ASSESSMENT")
print(g)

#png(filename = file.path(output.dir,paste("comparison of catchabilities.png")), 
#    units = "px",
#    width = 672, height = 800)
#dev.off()
#savePlot(file.path(".","results",assess2,"comparison of catchabilities.png"),type="png")

################# observation variance values ##########################
obs1 <- obs.var(fit1.flsam)
obs2 <- obs.var(fit2.flsam)
obs1$assess <- assess1
obs2$assess <- assess2
obs<-rbind(obs1,obs2)
obs$age[is.na(obs$age)] <- "all"
obs$label <- paste(obs$fleet,obs$age,sep="_")

g <- ggplot(data = obs , aes(label , value , fill = assess))
g   <-  g  +  geom_bar(aes(fill = assess   ), position = "dodge", stat="identity")
g   <-  g  +  ggtitle("observation variances")  + xlab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g   <-  g  +  geom_errorbar(aes(ymin=lbnd, ymax=ubnd),width=1, position=position_dodge(.9))
g   <-  g  +  facet_grid(fleet~.,scales = "free") + scale_colour_discrete(name = "ASSESSMENT")
print(g)

#savePlot(file.path(".","results",assess2,"comparison of obs.vars.png"),type="png")

################# process variances ##########################
mvars <- c("logSdLogFsta","logSdLogN") 

pvar1<- params(fit1.flsam)[is.element(params(fit1.flsam )$name,mvars),]
pvar2<- params(fit2.flsam)[is.element(params(fit2.flsam )$name,mvars),]
pvar1$assess <- assess1
pvar1$dummy <- 1:dim(pvar1)[1]
pvar2$assess <- assess2
pvar2$dummy <- 1:dim(pvar2)[1]
pvar  <-  rbind(pvar1,pvar2)
pvar$lbnd <- with(pvar , value - 2 * std.dev) 
pvar$ubnd <- with(pvar , value + 2 * std.dev)
pvar$value <- exp(pvar$value)
pvar$lbnd <- exp(pvar$lbnd)
pvar$ubnd <- exp(pvar$ubnd)
pvar$label<- paste(pvar$name,pvar$dummy,sep="_") 
pvar$name <- gsub("log","",pvar$name)


g <- ggplot(data = pvar , aes(label , value , fill = assess))
g   <-  g  +  geom_bar(aes(fill = assess   ), position = "dodge", stat="identity")
g   <-  g  +  ggtitle("process variances")  + xlab("")   + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g   <-  g  +  geom_errorbar(aes(ymin=lbnd, ymax=ubnd),width=1, position=position_dodge(.9))
g   <-  g  +  facet_grid(name~.,scales = "free")  + scale_colour_discrete(name = "ASSESSMENT")
print(g)

#savePlot(file.path(".","results",assess2,"comparison of process.vars.png"),type="png")


################# uncertainty ##########################
CV.yrs <- ssb(fit1.flsam)$year
CV.dat <- data.frame(year = CV.yrs,SSB=ssb(fit1.flsam)$CV,
                     Fbar=fbar(fit1.flsam)$CV,Rec=rec(fit1.flsam)$CV)
CV.dat1<-tidyr::gather (CV.dat , key = "var" , value = "value", 2:4)               
CV.dat1$assess <- assess1

CV.yrs <- ssb(fit1.flsam)$year
CV.dat <- data.frame(year = CV.yrs,SSB=ssb(fit2.flsam)$CV,
                     Fbar=fbar(fit2.flsam)$CV,Rec=rec(fit2.flsam)$CV)
CV.dat2<-tidyr::gather (CV.dat , key = "var" , value = "value", 2:4)               
CV.dat2$assess <- assess2

CV.dat <- rbind(CV.dat1,CV.dat2)


g <- ggplot(data = CV.dat , aes(x = year , y  = value , colour = assess))
g   <-  g  +  geom_line(aes(colour = assess   ))
g   <-  g  +  ggtitle("assessment uncertainty")  + xlab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g   <-  g  +  facet_grid(var~.,scales = "free")   + scale_colour_discrete(name = "ASSESSMENT")
print(g)

################# residual plot  for all ages ##########################
dat <- subset(residuals(NSH.sam),fleet=="IBTS-Q3")
xyplot(age ~ year,data=dat,cex=dat$std.res,col="black",main="Residuals by year IBTS-Q3",
       panel=function(...){
         lst <- list(...)
         panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex>0,1,19),col="black",cex=3*abs(lst$cex))
       })


#savePlot(file.path(".","results",assess2,"comparison of model uncertainty.png"),type="png")

dev.off()