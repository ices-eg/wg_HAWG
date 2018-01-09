fits<-list(fit1,fit2)


Cods<-FLStocks()
Pars <- list()
ii<-0

for (st in st.names)
{
st.name  <- st
ii<-ii+1

Cod.sam  <- flsam[[st]]
Cods[[st]]<-stc[[st]]

pars <-   Cod.sam@params
is.par  <-  cumsum(Cod.sam@params$name == "logF")
pars  <- pars[is.par==0,]
pars$CIlow  <- pars$value  - 2* pars$std.dev
pars$CIup   <- pars$value  + 2* pars$std.dev
pars$value  <- exp(pars$value)
pars$CIlow  <- exp(pars$CIlow)
pars$CIup   <- exp(pars$CIup)


pars$type[pars$name == "sigmaObsParUS"  ]      <-  "error cov."

pars$assess  <- st
pars$type <- "observation variances"
pars$type[is.element(pars$name,c("logSdLogFsta" , "logSdLogN"))] <- "process variances"
pars$type[is.element(pars$name,c("logFpar" , "logScaleSSB" , "logitReleaseSurvival"))] <- "catchability"
pars$type[pars$name == "sigmaObsParUS"  ]      <-  "error cov."
pars$name <- gsub("logit", "", pars$name)
pars$name <- gsub("log", "", pars$name)




   pars$ind <- 1
   for (i in 2:dim(pars)[1]) if (pars$name[i-1]==pars$name[i]) pars$ind[i] <- pars$ind[i-1] + 1
   pars$name <- paste (pars$name,pars$ind,sep="")
#    
#

pars$name2<-pars$name
ff<-fits[[ii]]
keyC   <- length(unique(ff$conf$keyVarObs[1,]))
keyEU  <- length(unique(ff$conf$keyVarObs[2,])) 
keyCan <- length(unique(ff$conf$keyVarObs[3,]))
key <- c(rep("catch",keyC),rep("EU",keyEU),rep("Can",keyCan))

aCatch     <- aggregate(1:8 , by=list(ff$conf$keyVarObs[1,]) , function(x) range(x))
aCatch$age <- paste("age-",aCatch[,2][,1],"-",aCatch[,2][,2],sep="") 
aEU    <- aggregate(1:8 , by=list(ff$conf$keyVarObs[2,]) , function(x) range(x))
aEU$age<-paste("age-",aEU[,2][,1],"-",aEU[,2][,2],sep="") 
aCan   <- aggregate(1:8 , by=list(ff$conf$keyVarObs[3,]) , function(x) range(x))
aCan$age<-paste("age-",aCan[,2][,1],"-",aCan[,2][,2],sep="") 
a<-c(aCatch$age,aEU$age,aCan$age)
key<-paste(key , a)

pars$name2[pars$type=="observation variances"]   <-  key



keyEU  <- length(unique(ff$conf$keyLogFpar[2,])) 
keyCan <- length(unique(ff$conf$keyLogFpar[3,]))
key    <- c(rep("EU",keyEU),rep("Can",keyCan))

aEU    <- aggregate(1:8 , by=list(ff$conf$keyLogFpar[2,]) , function(x) range(x))
aEU$age<-paste("age-",aEU[,2][,1],"-",aEU[,2][,2],sep="") 
aCan   <- aggregate(1:8 , by=list(ff$conf$keyLogFpar[3,]) , function(x) range(x))
aCan$age<-paste("age-",aCan[,2][,1],"-",aCan[,2][,2],sep="") 
a<-c(aEU$age,aCan$age)
key<-paste(key , a)

pars$name2[pars$type=="catchability"]   <-  key

Pars[[st]]<-pars
}

pars  <-  do.call(rbind, lapply(Pars, data.frame, stringsAsFactors=FALSE))


summary(pars)
#

#


pars$name2 <- gsub("SdLogFsta" , "Sd F RW" , pars$name2)
pars$name2[pars$name2 == "SdLogN1"]  <- "Rec Var"
pars$name2[pars$name2 == "SdLogN2"]  <- "proc. error"




library(ggplot2)




library(grid)

prs  <- pars[pars$type == "observation variances" , ]
g   <-  ggplot (prs,aes(name2,value,fill=assess))
g   <-  g  +  geom_bar(aes(fill = assess   ), position = "dodge", stat="identity")
g   <-  g  +  ggtitle("observation \nstd dev")  + xlab("")  + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g1   <-  g  +  geom_errorbar(aes(ymin=CIlow, ymax=CIup),width=1, position=position_dodge(.9))
g1

prs  <- pars[pars$type == "process variances" , ]
g   <-  ggplot (prs,aes(name2,value,fill=assess))
g   <-  g  +  geom_bar(aes(fill = assess   ), position = "dodge", stat="identity")
g   <-  g  +  geom_errorbar(aes(ymin=CIlow, ymax=CIup),width=1, position=position_dodge(.9))
g2  <-  g  +  ggtitle("process \nstd dev")  + xlab("")      + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g2

prs  <- pars[pars$type == "catchability" , ]
g   <-  ggplot (prs,aes(name2,value,fill=assess))
g   <-  g  +  geom_bar(aes(fill = assess   ), position = "dodge", stat="identity")
g   <-  g  +  geom_errorbar(aes(ymin=CIlow, ymax=CIup),width=1, position=position_dodge(.9))
g3  <-  g  +  ggtitle("scaling \nparameters") + xlab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g3

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 10)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(g1, vp = vplayout(1, 1:3))  # key is to define vplayout
print(g2, vp = vplayout(1, 4:5))
print(g3, vp = vplayout(1, 6:10))

#
#
#
#### plot the sd on the prameters
#x11()
#
#
#
#library(grid)
#
#prs  <- pars[pars$type == "observation variances" , ]
#g   <-  ggplot (prs,aes(name,std.dev,fill=assess))
#g   <-  g  +  geom_bar(aes(fill = assess   ), position = "dodge", stat="identity")
#g1  <-  g  +  ggtitle("observation std dev")  + xlab("")  + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#
#prs  <- pars[pars$type == "tag variances" , ]
#g   <-  ggplot (prs,aes(name,std.dev,fill=assess))
#g   <-  g  +  geom_bar(aes(fill = assess   ), position = "dodge", stat="identity")
#g12  <-  g  +  ggtitle("recapture \noverdips.")  + xlab("")  + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#
#
#prs  <- pars[pars$type == "process variances" , ]
#g   <-  ggplot (prs,aes(name,std.dev,fill=assess))
#g   <-  g  +  geom_bar(aes(fill = assess   ), position = "dodge", stat="identity")
#g2  <-  g  +  ggtitle("process variances")  + xlab("")      + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#
#
#prs  <- pars[pars$type == "scaling parameters" , ]
#g   <-  ggplot (prs,aes(name,std.dev,fill=assess))
#g   <-  g  +  geom_bar(aes(fill = assess   ), position = "dodge", stat="identity")
#g3  <-  g  +  ggtitle("scaling parameters") + xlab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#
#
#
#grid.newpage()
#pushViewport(viewport(layout = grid.layout(1, 13)))
#vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
#print(g1, vp = vplayout(1, 1:2))  # key is to define vplayout
#print(g12, vp = vplayout(1, 3:4))
#print(g2, vp = vplayout(1, 5:6))
#print(g3, vp = vplayout(1, 7:13))
#
#
# prs  <- pars[pars$type == "error cov." , ]
#g   <-  ggplot (prs,aes(name,std.dev,fill=assess))
#g   <-  g  +  geom_bar(aes(fill = assess   ), position = "dodge", stat="identity")
#g3  <-  g  +  ggtitle("scaling parameters") + xlab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#
#  g3
#
#
stc<-Cods




