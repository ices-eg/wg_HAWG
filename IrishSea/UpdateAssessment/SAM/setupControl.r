#-------------------------------------------------------------------------------
#- Setup control
#-------------------------------------------------------------------------------
ISH.ctrl                                        <- FLSAM.control(ISH,ISH.tun)

#FISHing mortality random walk coupling
ISH.ctrl@states["catch",]                       <- c(1:6,7,7)               #Couple age 8+ Fs
ISH.ctrl@f.vars["catch",]                       <- c(rep(1,8))                   #All have the same variance across ages

#Log N random walk variances
ISH.ctrl@logN.vars                              <- c(1,rep(2,7))

#Catchability models
ISH.ctrl@catchabilities["AC(VIIaN)",ac(1:8)]    <- c(1:3,rep(4,5))

#Observation model parameters
ISH.ctrl@obs.vars["catch",ac(1:8)]              <- c(1,2,3,3,4,4,4,4)
ISH.ctrl@obs.vars["AC(VIIaN)",ac(1:8)]          <- c(1,2,2,3,3,4,4,4)+4 #1-8

# # Stock recruitment model code (0=RW, 1=Ricker, 2=BH, ... more in time
ISH.ctrl@srr                                    <- as.integer(0)

