#getwd()
#load("workspace.RData")


CSH.retro


#Mohn's Rho SSB, F
#calcualtes Mohn's Rho for for either SSB, Fbar or Numbers at Age
#Valid types are "ssb", "f", "numatage"


mohns.rho <- function(retro, type = "ssb", plt = FALSE){
  
  tot.range <- retro@.Data[[1]]@range[c(4,5)] 
  ages <- 1:dim(CSH.retro@.Data[[1]]@stock.n@.Data)[1]
    
  if(type == "ssb"){
    rho <- array()
    ref <- cbind(tot.range[1]:tot.range[2], retro@.Data[[1]]@params[retro@.Data[[1]]@params$name == "ssb",])
    colnames(ref)[1] <- "year"
    if(plt) {plot(x = ref$year, y = ref$value, main = unique(ref$name), type = "l", ylab = "SSB", xlab = "Year")}
    
    for(i in 1:length(retro@.Data)){
      range <- retro@.Data[[i]]@range[c(4,5)]
      tip <- cbind(range[1]:range[2], retro@.Data[[i]]@params[retro@.Data[[i]]@params$name == "ssb",])
      colnames(tip)[1] <- "year"
      rho[i] <- (tip$value[tip$year == range[2]] - ref$value[ref$year == range[2]])/ref$value[ref$year == range[2]]
      if(plt) {lines(x = tip$year, y = tip$value, col = i)}
    }
    #return(sum(rho))
    return(mean(rho[2:length(retro@.Data)]))
       
  }
  
  if(type == "f"){
    rho <- array()
    ref <- cbind(tot.range[1]:tot.range[2], retro@.Data[[1]]@params[retro@.Data[[1]]@params$name == "fbar",])
    colnames(ref)[1] <- "year"
    if(plt) {plot(x = ref$year, y = ref$value, main = unique(ref$name), type = "l", ylab = "F", xlab = "Year")}
    
    for(i in 1:length(retro@.Data)){
      range <- retro@.Data[[i]]@range[c(4,5)]
      tip <- cbind(range[1]:range[2], retro@.Data[[i]]@params[retro@.Data[[i]]@params$name == "fbar",])
      colnames(tip)[1] <- "year"
      rho[i] <- (tip$value[tip$year == range[2]] - ref$value[ref$year == range[2]])/ref$value[ref$year == range[2]]
      if(plt) {lines(x = tip$year, y = tip$value, col = i)}
    }
    #return(sum(rho)) 
    return(mean(rho[2:length(retro@.Data)]))
  }
  
  if(type == "numatage"){
    rho <- array()
    for(j in ages){
      rho.age <- array()
      ref <- retro@.Data[[1]]@stock.n@.Data[j,,1,1,1,1]
      if(plt){plot(x = tot.range[1]:tot.range[2], y = ref, type = "l", main = paste("Num At Age", j, "(mil)"), ylab = "num", xlab = "Year")}
      
      for(i in 1:length(retro@.Data)){
        range <- retro@.Data[[i]]@range[c(4,5)]
        tip <- retro@.Data[[i]]@stock.n@.Data[j,,1,1,1,1]
        rho.age[i] <- (tail(tip, 1) - tail(ref, i)[1])/tail(ref,i)[1]
        if(plt){lines(x= range[1]:range[2], y = tip, col = i)}
      }
     #rho[j] <- sum(rho.age)
     rho[j] <- mean(rho.age[2:length(retro@.Data)])
    }
    
  return(rho)      
  }
   
}


mohns.rho(CSH.retro, type = "ssb")
mohns.rho(CSH.retro, type = "ssb", plt = TRUE)
mohns.rho(CSH.retro, type = "f")
mohns.rho(CSH.retro, type = "f", plt = T)
mohns.rho(CSH.retro, type = "numatage")
mohns.rho(CSH.retro, type = "numatage", plt = T)




# To very crudely spit out the raw data
#Numbers at age
#write.table(CSH.retro@.Data[[1]]@stock.n@.Data, file = "numbers at age.csv")
#for (i in 2:8){
#  write.table(CSH.retro@.Data[[i]]@stock.n@.Data, append = TRUE, file = "numbers at age.csv")
#}

#SSB and f
#CSH.retro@.Data[[1]]@params[CSH.retro@.Data[[1]]@params$name == "ssb",]

#ssb <- cbind(1958:2013, CSH.retro@.Data[[1]]@params[CSH.retro@.Data[[1]]@params$name == "ssb",])
#f <- CSH.retro@.Data[[1]]@params[CSH.retro@.Data[[1]]@params$name == "fbar",]

#both <- cbind(ssb, f)
#write.csv(both, file = "ssb2013.csv")
#
#for(i in 2:8){
#  ssb <-  CSH.retro@.Data[[i]]@params[CSH.retro@.Data[[i]]@params$name == "ssb",]
#  f <- CSH.retro@.Data[[i]]@params[CSH.retro@.Data[[i]]@params$name == "fbar",]
  
#  both <- cbind(ssb, f)
#  write.csv(both, file = paste("ssb and F", i ,".csv"))
#}











