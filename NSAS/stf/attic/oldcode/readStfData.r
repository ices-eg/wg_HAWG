#-------------------------------------------------------------------------------
#- TAC overfishing underfishing ratio's
#-------------------------------------------------------------------------------
OOF   <- read.csv(paste("./data/over_underfishing",DtY,".csv",sep=""),header=T); dims.OOF <- dim(OOF)

#-------------------------------------------------------------------------------
#- Partial Ns per fleet to partial Fs
#-------------------------------------------------------------------------------

# NOTE: THIS NEEDS TO BE TAKEN FROM THE MULTIFLEET ASSESSMENT

Ns            <- read.csv(paste("./data/partial_ns",ac(an(ImY)-1),".csv",sep=""),header=T)
Ns1           <- read.csv(paste("./data/partial_ns",ac(an(ImY)-2),".csv",sep=""),header=T)
Ns2           <- read.csv(paste("./data/partial_ns",ac(an(ImY)-3),".csv",sep=""),header=T)
Nstot         <- cbind(Ns2,Ns1,Ns)

#-Apply plusgroup
Ns[ac(9),]    <- apply(Ns[ac(9:10),],2,sum); Ns <- Ns[ac(1:9),]

#-------------------------------------------------------------------------------
#- Partial Ws per fleet
#-------------------------------------------------------------------------------

# NOTE: THIS NEEDS TO BE TAKEN FROM THE MULTIFLEET ASSESSMENT

Ws            <- read.csv(paste("./data/partial_ws",ac(an(ImY)-1),".csv",sep=""),header=T)

#- Leave these options below in case we move back to 9+ group
WA            <- apply(Ws[,paste("A",ac((an(DtY)-2):DtY),sep="")],1,mean,na.rm=T)
WB            <- apply(Ws[,paste("B",ac((an(DtY)-2):DtY),sep="")],1,mean,na.rm=T)
WC            <- apply(Ws[,paste("C",ac((an(DtY)-2):DtY),sep="")],1,mean,na.rm=T)
WD            <- apply(Ws[,paste("D",ac((an(DtY)-2):DtY),sep="")],1,mean,na.rm=T)

WA            <- apply(Ws[,paste("A",ac((an(DtY)-2):DtY),sep="")] * Nstot[,paste("A",ac((an(DtY)-2):DtY),sep="")],1,sum,na.rm=T) / apply(Nstot[,paste("A",ac((an(DtY)-2):DtY),sep="")],1,sum,na.rm=T)
WA[9]         <- sum(Ws[9:10,paste("A",ac((an(DtY)-2):DtY),sep="")] * Nstot[9:10,paste("A",ac((an(DtY)-2):DtY),sep="")],na.rm=T) / sum(Nstot[9:10,paste("A",ac((an(DtY)-2):DtY),sep="")],na.rm=T)
WA            <- WA[1:9]; WA[is.na(WA)] <- 0

WB            <- apply(Ws[,paste("B",ac((an(DtY)-2):DtY),sep="")] * Nstot[,paste("B",ac((an(DtY)-2):DtY),sep="")],1,sum,na.rm=T) / apply(Nstot[,paste("B",ac((an(DtY)-2):DtY),sep="")],1,sum,na.rm=T)
WB[9]         <- sum(Ws[9:10,paste("B",ac((an(DtY)-2):DtY),sep="")] * Nstot[9:10,paste("B",ac((an(DtY)-2):DtY),sep="")],na.rm=T) / sum(Nstot[9:10,paste("B",ac((an(DtY)-2):DtY),sep="")],na.rm=T)
WB            <- WB[1:9]; WB[is.na(WB)] <- 0

WC            <- apply(Ws[,paste("C",ac((an(DtY)-2):DtY),sep="")] * Nstot[,paste("C",ac((an(DtY)-2):DtY),sep="")],1,sum,na.rm=T) / apply(Nstot[,paste("C",ac((an(DtY)-2):DtY),sep="")],1,sum,na.rm=T)
WC[9]         <- sum(Ws[9:10,paste("C",ac((an(DtY)-2):DtY),sep="")] * Nstot[9:10,paste("C",ac((an(DtY)-2):DtY),sep="")],na.rm=T) / sum(Nstot[9:10,paste("C",ac((an(DtY)-2):DtY),sep="")],na.rm=T)
WC            <- WC[1:9]; WC[is.na(WC)] <- 0

WD            <- apply(Ws[,paste("D",ac((an(DtY)-2):DtY),sep="")] * Nstot[,paste("D",ac((an(DtY)-2):DtY),sep="")],1,sum,na.rm=T) / apply(Nstot[,paste("D",ac((an(DtY)-2):DtY),sep="")],1,sum,na.rm=T)
WD[9]         <- sum(Ws[9:10,paste("D",ac((an(DtY)-2):DtY),sep="")] * Nstot[9:10,paste("D",ac((an(DtY)-2):DtY),sep="")],na.rm=T) / sum(Nstot[9:10,paste("D",ac((an(DtY)-2):DtY),sep="")],na.rm=T)
WD            <- WD[1:9]; WD[is.na(WD)] <- 0

dmns          <- dimnames(NSH@catch.wt)
WA            <- FLQuant(WA,dimnames=list(age=dmns$age,year="unique",unit="A",season=dmns$season,area=dmns$area,iter=dmns$iter))
WB            <- FLQuant(WB,dimnames=list(age=dmns$age,year="unique",unit="B",season=dmns$season,area=dmns$area,iter=dmns$iter))
WC            <- FLQuant(WC,dimnames=list(age=dmns$age,year="unique",unit="C",season=dmns$season,area=dmns$area,iter=dmns$iter))
WD            <- FLQuant(WD,dimnames=list(age=dmns$age,year="unique",unit="D",season=dmns$season,area=dmns$area,iter=dmns$iter))


