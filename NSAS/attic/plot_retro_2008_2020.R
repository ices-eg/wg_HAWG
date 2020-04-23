rm(list=ls())

library(ggplot2)
library(FLSAM)
library(FLEDA)
library(reshape2)
library(icesAdvice)

#path          <- "D:/git/wg_HAWG/NSAS/"
path          <- "C:/git/wg_HAWG/NSAS/"

try(setwd(path),silent=TRUE)

output.dir    <-  file.path(".","results//") # result directory
figure.dir    <-  file.path(".","results","plots_singlefleet") # figure directory


load(file.path(output.dir,'NSH_HAWG2020_sf_retro.Rdata'))
#load(file.path(output.dir,'NSH_HAWG2018_sf_retro.Rdata'))

start_year  <- 2000
end_year    <- 2020#an(colnames(NSH@catch[,dim(NSH@catch)[2]]))

assessment_name_singlefleet     <- 'HAWG2020_singlefleet'


# select data to plot retro pattern plots
for(idxRetro in 1:length(NSH.retro)){
  currentNSH  <- NSH.retro[[idxRetro]]
  
  if(idxRetro == 1){
    fbar.retro            <- cbind(fbar(currentNSH),rep(currentNSH@name,dim(fbar(currentNSH))[1]))
    colnames(fbar.retro)  <- c(colnames(fbar(currentNSH)),'ItY')
    
    f.retro               <- cbind(f(currentNSH),rep(currentNSH@name,dim(f(currentNSH))[1]))
    colnames(f.retro)     <- c(colnames(f(currentNSH)),'ItY')
    
    ssb.retro             <- cbind(ssb(currentNSH),rep(currentNSH@name,dim(ssb(currentNSH))[1]))
    colnames(ssb.retro)   <- c(colnames(ssb(currentNSH)),'ItY')
    
    rec.retro             <- cbind(rec(currentNSH),rep(currentNSH@name,dim(rec(currentNSH))[1]))
    colnames(rec.retro)   <- c(colnames(rec(currentNSH)),'ItY')
  }else{
    currentVar            <- cbind(fbar(currentNSH),rep(currentNSH@name,dim(fbar(currentNSH))[1]))
    colnames(currentVar)  <- c(colnames(fbar(currentNSH)),'ItY')
    fbar.retro            <- rbind(fbar.retro,currentVar)
    
    currentVar            <- cbind(f(currentNSH),rep(currentNSH@name,dim(f(currentNSH))[1]))
    colnames(currentVar)  <- c(colnames(f(currentNSH)),'ItY')
    f.retro               <- rbind(f.retro,currentVar)
    
    currentVar            <- cbind(ssb(currentNSH),rep(currentNSH@name,dim(ssb(currentNSH))[1]))
    colnames(currentVar)  <- c(colnames(ssb(currentNSH)),'ItY')
    ssb.retro             <- rbind(ssb.retro,currentVar)
    
    currentVar            <- cbind(rec(currentNSH),rep(currentNSH@name,dim(rec(currentNSH))[1]))
    colnames(currentVar)  <- c(colnames(rec(currentNSH)),'ItY')
    rec.retro             <- rbind(rec.retro,currentVar)
  }
}

#mat_ssb.retro <- dcast(subset(ssb.retro,year > 2005), year~ItY ,value.var = 'value')
#rownames(mat_ssb.retro) <- mat_ssb.retro$year
#mat_ssb.retro <- mat_ssb.retro[,2:dim(mat_ssb.retro)[2]]

#mohn(x = mat_ssb.retro,peels = 1,plot = TRUE)

#a <- mohns.rho(retro = NSH.retro,ref.year = 2019,span = 7,type = 'ssb')

#################################################################
################### plot stuff from here ########################
#################################################################

windows()

p <-  ggplot(data = ssb.retro,mapping=aes(x=year,y=value,color=ItY))+
      geom_line()+
      xlim(start_year, end_year)+
      ylim(0.8*1e6, 3.5*1e6)+
      ylab('SSB')+xlab('year')+
      geom_ribbon(aes(ymin=lbnd, ymax=ubnd,fill=ItY), alpha=0.1,colour = NA)

print(p)

savePlot(paste(figure.dir,"/",assessment_name_singlefleet,"_33_long_retro_SSB.png",sep = ""),type="png")

p <-  ggplot(data = fbar.retro,mapping=aes(x=year,y=value,color=ItY))+
      geom_line()+
      xlim(start_year, end_year)+
      ylim(0, 0.5)+
      ylab('fbar')+xlab('year')+
      geom_ribbon(aes(ymin=lbnd, ymax=ubnd,fill=ItY), alpha=0.1,colour = NA)

print(p)

savePlot(paste(figure.dir,"/",assessment_name_singlefleet,"_34_long_retro_fbar.png",sep = ""),type="png")

p <-  ggplot(data = rec.retro,mapping=aes(x=year,y=value,color=ItY))+
      geom_line()+
      xlim(start_year, end_year)+
      ylab('rec')+xlab('year')+
      geom_ribbon(aes(ymin=lbnd, ymax=ubnd,fill=ItY), alpha=0.1,colour = NA)

print(p)

savePlot(paste(figure.dir,"/",assessment_name_singlefleet,"_35_long_retro_rec.png",sep = ""),type="png")


# retro on f at age

for(idxAge in unique(f.retro$age)){
  p <-  ggplot(data = subset(f.retro,age==idxAge),mapping=aes(x=year,y=value,color=ItY))+
    geom_line()+
    xlim(start_year, end_year)+
    ylab('f')+xlab('year')+
    geom_ribbon(aes(ymin=lbnd, ymax=ubnd,fill=ItY), alpha=0.1,colour = NA)+
    ggtitle(paste('F age ',idxAge))
  
  print(p)
  
  savePlot(paste(figure.dir,"/",assessment_name_singlefleet,"_36_long_retro_bounds_f_", idxAge,".png",sep = ""),type="png")
  
  # reshape data to compute mohn rho (for f at age)
  mat_f.retro <- dcast(subset(f.retro,age==idxAge&year > 2005), year~ItY ,value.var = 'value')
  rownames(mat_f.retro) <- mat_f.retro$year
  mat_f.retro <- mat_f.retro[,2:dim(mat_f.retro)[2]]
  
  print(mohn(x = mat_f.retro,peels = 5,plot = TRUE))
  title(paste('age ',idxAge,'-mohn rho (5 year peel) = ',mohn(x = mat_f.retro,peels = 5,plot = TRUE)))
  savePlot(paste(figure.dir,"/",assessment_name_singlefleet,"_36_long_retro_f_", idxAge,".png",sep = ""),type="png")
}

dev.off()

#mohn(subset(f.retro,age==idxAge), peels = 5)

