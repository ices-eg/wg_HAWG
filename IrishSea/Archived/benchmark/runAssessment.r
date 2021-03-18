library(FLCore)
library(FLSAM)

rm(list=ls())

#- Set paths
my.path           <- "D:/Repository/ICES_HAWG/wg_HAWG/IrishSea/"
output.dir        <- file.path(my.path,"results")
res.dir           <- file.path(my.path,"benchmark")
data.source       <- file.path(my.path,"data")

#- Setup stock, tun and control
scenario          <- list(); scenario$trimTS   <- F
source(file.path(my.path,"benchmark/setupStockTun.r"))
source(file.path(my.path,"benchmark/setupControl.r"))

#-------------------------------------------------------------------------------
#- Run default
#-------------------------------------------------------------------------------

ISH.sam           <- FLSAM(ISH,ISH.tun,ISH.ctrl)
ISH               <- ISH + ISH.sam
save(ISH,ISH.tun,ISH.ctrl,ISH.sam,file=file.path(res.dir,"default.RData"))

pdf(file=file.path(res.dir,"default.pdf"))
print(procerr.plot(ISH,weight="stock.wt",type="n",rel=T))
print(procerr.plot(ISH,weight="stock.wt",type="mort",rel=T))
print(procerr.plot(ISH,weight="stock.wt",type="tsb",rel=T))
dev.off()

#-------------------------------------------------------------------------------
#- Run scenario
#-------------------------------------------------------------------------------

#- Run VPA
scenario          <- list(name="VPA",trimTS=F)
scenario$saveName <- scenario$name
source(file.path(my.path,"benchmark/runVPA.r"))
pdf(file=file.path(res.dir,paste(scenario$name,".pdf",sep=""))); plot(stcks); dev.off()

scenario          <- list(name="VPA",trimTS=T)
scenario$saveName <- scenario$name
source(file.path(my.path,"benchmark/runVPA.r"))
pdf(file=file.path(res.dir,paste(scenario$name,"TS1980.pdf",sep=""))); plot(stcks); dev.off()


#- Plus group check
scenario          <- list(name="plusgroup",setting=c(5,6,7,8),trimTS=F)
scenario$saveName <- scenario$name
source(file.path(my.path,"benchmark/runPlusgroup.r"))
pdf(file=file.path(res.dir,paste(scenario$name,".pdf",sep=""))); plot(as(pg.sams,"FLSAMs")); dev.off()

scenario          <- list(name="plusgroup",setting=c(5,6,7,8),trimTS=T)
scenario$saveName <- paste(scenario$name,"TS1980",sep="")
source(file.path(my.path,"benchmark/runPlusgroup.r"))
pdf(file=file.path(res.dir,paste(scenario$name,"TS1980.pdf",sep=""))); plot(as(pg.sams,"FLSAMs")); dev.off()

#- Natural mortality check
scenario          <- list(name="natmort",trimTS=F)
scenario$saveName <- scenario$name
source(file.path(my.path,"benchmark/runM.r"))
pdf(file=file.path(res.dir,paste(scenario$name,".pdf",sep=""))); plot(as(m.sams,"FLSAMs")); dev.off()

scenario          <- list(name="natmort",trimTS=T)
scenario$saveName <- paste(scenario$name,"TS1980",sep="")
source(file.path(my.path,"benchmark/runM.r"))
pdf(file=file.path(res.dir,paste(scenario$name,"TS1980.pdf",sep=""))); plot(as(m.sams,"FLSAMs")); dev.off()

#- Run some retrospectives
scenario          <- list(name="retrospective",nyears=5,trimTS=F)
scenario$saveName <- scenario$name
source(file.path(my.path,"benchmark/runRetro.r"))
pdf(file=file.path(res.dir,paste(scenario$name,".pdf",sep=""))); plot(as(r.sams,"FLSAMs")); dev.off()

scenario          <- list(name="retrospective",nyears=5,trimTS=T)
scenario$saveName <- paste(scenario$name,"TS1980",sep="")
source(file.path(my.path,"benchmark/runRetro.r"))
pdf(file=file.path(res.dir,paste(scenario$name,"TS1980.pdf",sep=""))); plot(as(r.sams,"FLSAMs")); dev.off()

#- Run cohort analyses
scenario          <- list(name="cohort",trimTS=F)
scenario$saveName <- scenario$name
source(file.path(my.path,"benchmark/runCohortAnalyses.r"))

scenario          <- list(name="cohort",trimTS=T)
scenario$saveName <- paste(scenario$name,"TS1980",sep="")
source(file.path(my.path,"benchmark/runCohortAnalyses.r"))

#- Run new survey analyses
scenario          <- list(name="newSurveyAge",trimTS=F)
scenario$saveName <- scenario$name
source(file.path(my.path,"benchmark/runNewSurveyAge.r"))

scenario          <- list(name="newSurveyAge",trimTS=T)
scenario$saveName <- paste(scenario$name,"TS1980",sep="")
source(file.path(my.path,"benchmark/runNewSurveyAge.r"))

scenario          <- list(name="newSurveySSB",trimTS=F)
scenario$saveName <- scenario$name
source(file.path(my.path,"benchmark/runNewSurveySSB.r"))

scenario          <- list(name="newSurveySSB",trimTS=T)
scenario$saveName <- paste(scenario$name,"TS1980",sep="")
source(file.path(my.path,"benchmark/runNewSurveySSB.r"))

#- Remove 2001 survey index from acoustic survey
scenario          <- list(name="remove2001Survey",trimTS=F)
scenario$saveName <- scenario$name
source(file.path(my.path,"benchmark/runRemoveSurvey.r"))

scenario          <- list(name="remove2001Survey",trimTS=T)
scenario$saveName <- paste(scenario$name,"TS1980",sep="")
source(file.path(my.path,"benchmark/runRemoveSurvey.r"))

#- No catchability for acoustic survey (so q = 1)
scenario          <- list(name="noQSurvey",trimTS=T)
scenario$saveName <- paste(scenario$name,"TS1980",sep="")
source(file.path(my.path,"benchmark/runNoQ.r"))
pdf(file=file.path(res.dir,paste(scenario$name,"TS1980.pdf",sep=""))); plot(as(noQ.sams,"FLSAMs")); dev.off()

#- Increase weight ssb survey
scenario          <- list(name="ssbVar",trimTS=T)
scenario$saveName <- paste(scenario$name,"TS1980",sep="")
source(file.path(my.path,"benchmark/runNewSurveySSBWeight.r"))

#- No process error
scenario          <- list(name="noProcError",trimTS=T)
scenario$saveName <- paste(scenario$name,"TS1980",sep="")
source(file.path(my.path,"benchmark/runNoProcError.r"))

#- New SSB survey, q=1,var=0.2
scenario          <- list(name="newISH",trimTS=T)
scenario$saveName <- paste(scenario$name,"TS1980",sep="")
source(file.path(my.path,"benchmark/runNewSurveyq1var02.r"))



- projectForward in retrospect
- look at catch prediction
- hidden m


