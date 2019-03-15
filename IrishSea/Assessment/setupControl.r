#-------------------------------------------------------------------------------
#- Setup control
#-------------------------------------------------------------------------------
ISH.ctrl                                        <- FLSAM.control(ISH,ISH.tun)

#FISHing mortality random walk coupling
ISH.ctrl@states["catch unique",]                       <- c(1:6,7,7)               #Couple age 8+ Fs
ISH.ctrl@f.vars["catch unique",]                       <- c(1,1,2,2,2,3,4,4)       #All have the same variance across ages

#Log N random walk variances
ISH.ctrl@logN.vars                              <- 1

#Catchability models
ISH.ctrl@catchabilities["AC(VIIaN)",ac(1:8)]    <- c(1:3,rep(4,5))

#Observation model parameters
ISH.ctrl@obs.vars["catch unique",ac(1:8)]              <- c(1,2,2,2,3,3,3,3)
ISH.ctrl@obs.vars["AC(VIIaN)",ac(1:8)]          <- c(4,rep(5,4),rep(6,3))
ISH.ctrl@obs.vars["VIIaNSpawn",ac(1)]          <- 7

# # Stock recruitment model code (0=RW, 1=Ricker, 2=BH, ... more in time
ISH.ctrl@srr                                    <- as.integer(0)
ISH.ctrl                                        <- update(ISH.ctrl)