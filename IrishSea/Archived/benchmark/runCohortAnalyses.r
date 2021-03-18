source(file.path(my.path,"benchmark/setupStockTun.r"))
source(file.path(my.path,"benchmark/setupControl.r"))

#-------------------------------------------------------------------------------
#- Run scenario
#-------------------------------------------------------------------------------
require(reshape)

#- Survey internal consistency compared to catch data
pdf(file=file.path(res.dir,paste(scenario$saveName,".pdf",sep="")))
print(plot(ISH.tun[["AC(VIIaN)"]],main="Survey",type="internal"))

#- Convert age data to cohort data
stockCohort   <- as(ISH@stock.n,"FLCohort")
catchCohort   <- as(ISH@catch.n,"FLCohort")
survCohort    <- as(ISH.tun[[1]]@index,"FLCohort")

#- Calculate total mortality
zstockCohort  <- -1*log(stockCohort[2:8,]/stockCohort[1:7,])
zcatchCohort  <- -1*log(catchCohort[2:8,]/catchCohort[1:7,])
zsurvCohort   <- -1*log(survCohort[2:8,]/survCohort[1:7,])

#- Plot the numbers at age
print(plot(log(survCohort[,ac(1986:2013)]),main="Numbers in cohort survey"))
print(plot(log(stockCohort[,ac(1986:2013)]),main="Numbers in cohort stock"))
print(plot(log(catchCohort[,ac(1986:2013)]),main="Numbers in cohort catch"))

res           <- rbind(
                       cbind(as.data.frame(zstockCohort),type="stock"),
                       cbind(as.data.frame(zsurvCohort),type="survey"),
                       cbind(as.data.frame(zcatchCohort),type="catch"))

#- Plot the total mortality at age
print(xyplot(data ~ age | as.factor(cohort),group=type,data=subset(res,cohort > 1990),type="b",auto.key=T,pch=".",ylab="log(z-at-age)",main="Original"))
res$age[which(res$type == "survey")] <- res$age[which(res$type == "survey")]+1
print(xyplot(data ~ age | as.factor(cohort),group=type,data=subset(res,cohort > 1990),type="b",auto.key=T,pch=".",ylab="log(N-at-age)",main="Survey shifted 1 age-group"))

#- Look at numbers in the cohort and do correlation (scale within a cohort)
res           <- rbind(cbind(melt(as.data.frame(apply(catchCohort[,ac(1986:2013)],2,scale))),age=rep(1:8,length(1986:2013)),type="catch"),
                       cbind(melt(as.data.frame(apply(survCohort[,ac(1986:2013)], 2,scale))),age=rep(1:8,length(1986:2013)),type="survey"))
colnames(res) <- c("year","value","age","type")
resCol        <- cbind(cbind(melt(as.data.frame(apply(catchCohort[,ac(1986:2013)],2,scale))),age=rep(1:8,length(1986:2013)),type="catch")[,c("variable","value","age")],
                       cbind(melt(as.data.frame(apply(survCohort[,ac(1986:2013)], 2,scale))),age=rep(1:8,length(1986:2013)),type="survey")[,"value"])
colnames(resCol)  <- c("year","valueCatch","age","valueSurvey")
idx               <- which(is.na(resCol$valueCatch)==F & is.na(resCol$valueSurvey)==F)
cor(resCol$valueCatch[idx],resCol$valueSurvey[idx])
print(xyplot(value ~ age | year,group=type,data=res,type="b",auto.key=T,pch=".",ylab="N-at-age (scaled)",main="Original"))

#- Shift by one year
res           <- rbind(cbind(melt(as.data.frame(apply(catchCohort[,ac(1986:2013)],2,scale))),age=rep(1:8,length(1986:2013)),type="catch"),
                       cbind(melt(as.data.frame(apply(survCohort[,ac(1986:2013)], 2,scale))),age=rep(1:8,length(1986:2013)),type="survey"))
colnames(res) <- c("year","value","age","type")
res$year      <- anf(res$year); res$age <- anf(res$age)
idx           <- which(res$type == "survey" & res$year >= 1994 & res$year <= 2003 & res$year != 1995 & res$year != 1999)
res$age[idx]  <- res$age[idx]+1
resCol        <- merge(subset(res,type=="catch"),subset(res,type=="survey"),by=c("year","age"))
colnames(resCol) <- c("year","age","valueCatch","typeCatch","valueSurvey","typeSurvey")
idx               <- which(is.na(resCol$valueCatch)==F & is.na(resCol$valueSurvey)==F)
cor(resCol$valueCatch[idx],resCol$valueSurvey[idx])
print(xyplot(value ~ age | as.factor(year),group=type,data=res,type="b",auto.key=T,pch=".",ylab="N-at-age (scaled)",main="Survey shifted 1 age-group"))


#- Look at numbers in the cohort and do correlation (scale within an age)
res           <- rbind(cbind(melt(as.data.frame(apply(catchCohort[,ac(1986:2013)],1,scale))),year=rep(1986:2013,8),type="catch"),
                       cbind(melt(as.data.frame(apply(survCohort[,ac(1986:2013)], 1,scale))),year=rep(1986:2013,8),type="survey")); res <- res[,c("year","value","variable","type")]
colnames(res) <- c("year","value","age","type")
resCol        <- cbind(cbind(melt(as.data.frame(apply(catchCohort[,ac(1986:2013)],1,scale))),year=rep(1986:2013,8),type="catch")[,c("variable","value","year")],
                       cbind(melt(as.data.frame(apply(survCohort[,ac(1986:2013)], 1,scale))),year=rep(1986:2013,8),type="survey")[,"value"])
colnames(resCol)  <- c("year","valueCatch","age","valueSurvey")
idx               <- which(is.na(resCol$valueCatch)==F & is.na(resCol$valueSurvey)==F)
cor(resCol$valueCatch[idx],resCol$valueSurvey[idx])
print(xyplot(value ~ age | as.factor(year),group=type,data=res,type="b",auto.key=T,pch=".",ylab="N-at-age (scaled)",main="Original (scaled by age)"))
print(xyplot(value ~ year | as.factor(age),group=type,data=res,type="b",auto.key=T,pch=".",ylab="N-at-age (scaled)",main="Original (scaled by age)"))


colnames(resCol)  <- c("age","valueCatch","year","valueSurvey")
res           <- rbind(cbind(melt(as.data.frame(apply(catchCohort[,ac(1986:2013)],1,scale))),year=rep(1986:2013,8),type="catch"),
                       cbind(melt(as.data.frame(apply(survCohort[,ac(1986:2013)], 1,scale))),year=rep(1986:2013,8),type="survey")); res <- res[,c("year","value","variable","type")]
colnames(res) <- c("year","value","age","type")
res$year      <- anf(res$year); res$age <- anf(res$age)
idx           <- which(res$type == "survey" & res$year >= 1994 & res$year <= 2003)
res$age[idx]  <- res$age[idx]+1
resCol        <- merge(subset(res,type=="catch"),subset(res,type=="survey"),by=c("year","age"))
colnames(resCol) <- c("year","age","valueCatch","typeCatch","valueSurvey","typeSurvey")
idx               <- which(is.na(resCol$valueCatch)==F & is.na(resCol$valueSurvey)==F)
cor(resCol$valueCatch[idx],resCol$valueSurvey[idx])
print(xyplot(value ~ age | as.factor(year),group=type,data=res,type="b",auto.key=T,pch=".",ylab="N-at-age (scaled)",main="Survey shifted 1 age-group"))
print(xyplot(value ~ year | as.factor(age),group=type,data=res,type="b",auto.key=T,pch=".",ylab="N-at-age (scaled)",main="Survey shifted (scaled by age)"))

#- Look at numbers in the cohort and do correlation (no scaling)
a             <- as.data.frame(catchCohort[,ac(1986:2013)])
b             <- as.data.frame(survCohort[,ac(1986:2013)])

b$age[which(b$cohort >= 1994 & b$cohort <= 2003)] <- b$age[which(b$cohort >= 1994 & b$cohort <= 2003)] + 1
d             <- merge(a[,c("age","cohort","data")],b[,c("age","cohort","data")],by=c("age","cohort"))
idx           <- which(is.na(d$data.x)==F & is.na(d$data.y)==F)
cor(d$data.x[idx],d$data.y[idx])

dev.off()