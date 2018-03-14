
##
#  script to compare the output of the assessment using the new Fprop and the 2017 assessment

### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA);library(ggplot2); library(grid); library(corrplot)

rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

# local path
path <- "D:/git/wg_HAWG/NSAS/benchmark/"
try(setwd(path),silent=TRUE)

data.source <- file.path(".","data")    #Data source, not code or package source!!!

### ======================================================================================================
### correlation all surveys
### ======================================================================================================

# load previous assessment in sequence
data_surveys <- readFLIndices(file.path(data.source, "fleet_3c_newIBTSQ3.txt"))

# catches
#data_catches <- read.csv(file.path(data.source, "catches.csv"))
#data_catches <- as.data.frame( data_catches[!is.na(data_catches$data),])
#data_catches$X <- NULL
#data_catches$unit <- NULL
#data_catches$season <- NULL
#data_catches$area <- NULL
#data_catches$iter <- NULL
#data_catches <- cbind(data_catches, rep("canum",dim(data_catches)[1]))
#colnames(data_catches)[4] <- 'cname'

# IBTSQ3
data_IBTSQ3 <- as.data.frame(data_surveys)
data_IBTSQ3 <- as.data.frame( data_IBTSQ3[data_IBTSQ3$cname == "IBTS-Q3" & 
                                            data_IBTSQ3$slot == "index" &
                                            !is.na(data_IBTSQ3$data),c(2,3,8)])

# IBTSQ1
data_IBTSQ1 <- as.data.frame(data_surveys)
data_IBTSQ1 <- as.data.frame( data_IBTSQ1[data_IBTSQ1$cname == "IBTS-Q1" & 
                                            data_IBTSQ1$slot == "index" &
                                            !is.na(data_IBTSQ1$data),c(2,3,8)])

# IBTS0
data_IBTS0 <- as.data.frame(data_surveys)
data_IBTS0 <- as.data.frame( data_IBTS0[data_IBTS0$cname == "IBTS0" & 
                                          data_IBTS0$slot == "index" &
                                          !is.na(data_IBTS0$data),c(2,3,8)])

# HERAS
data_HERAS <- as.data.frame(data_surveys)
data_HERAS <- as.data.frame( data_HERAS[data_HERAS$cname == "HERAS" & 
                                          data_HERAS$slot == "index" &
                                          !is.na(data_HERAS$data),c(2,3,8,9)])

data_all <- as.data.frame(data_surveys)
data_all <- as.data.frame(data_all[data_all$slot == "index" &
                                     !is.na(data_all$data),])

data_all$slot = NULL
data_all$unit = NULL
data_all$season = NULL
data_all$area = NULL
data_all$iter = NULL

windows()
par(mfrow=c(3,2))

for(age_current in 0:5){

data_ages <- data_all[data_all$age == age_current,]
survey_names <- unique(data_ages$cname)

res <- data.frame(matrix(ncol = length(survey_names), nrow = length(survey_names)))
rownames(res) <- gsub('-', '', survey_names)
colnames(res) <- gsub('-', '', survey_names)
for(idxSurvey_current in 1:length(survey_names)){
  # current data
  data_current <- data_ages[data_ages$cname == survey_names[idxSurvey_current],]
  for (idxSurvey_comp in 1:length(survey_names)){
    # comparison data
    data_comp <- data_ages[data_ages$cname == survey_names[idxSurvey_comp],]
    # select common years
    year_vec <- intersect(data_current$year,data_comp$year)
    
    idx_year_comp <- which(data_comp$year %in% year_vec)
    idx_year_current <- which(data_current$year %in% year_vec)
    y <- data_comp[idx_year_comp,]$data
    x <- data_current[idx_year_current,]$data
    
    res_pearson <- cor.test(x, y,method = "pearson")
    #res[idxSurvey_current,idxSurvey_comp] <- as.numeric(gsub('cor', '', res_pearson$estimate))
    res[idxSurvey_current,idxSurvey_comp] <- cor(x,y)
  }
}

corrplot.mixed(as.matrix(res),lower = "ellipse", upper = "number",title=paste("age", age_current),mar=c(0,0,1,0))#, method = "ellipse", title=paste("age", age_current))

#corrplot(as.matrix(res), method = "ellipse", title=paste("age", age_current))
}

savePlot(file.path(".","results","miscellaneous","correlation_surveys.png"),type="png")


### ======================================================================================================
### correlation IBTS-Q1 old and new
### ======================================================================================================
# load new IBTSQ1
data_surveys <- readFLIndices(file.path(data.source, "fleet_3c_newIBTSQ3.txt"))

# IBTSQ1
data_IBTSQ1 <- as.data.frame(data_surveys)
data_IBTSQ1 <- as.data.frame( data_IBTSQ1[data_IBTSQ1$cname == "IBTS-Q1" & 
                                            data_IBTSQ1$slot == "index" &
                                            !is.na(data_IBTSQ1$data),c(2,3,8)])

data_IBTSQ1$cname <- rep("IBTSQ1_new",dim(data_IBTSQ1)[1])

# load old IBTSQ1
data_surveys <- readFLIndices(file.path(data.source, "fleet.txt"))

data_IBTSQ1_old <- as.data.frame(data_surveys)
data_IBTSQ1_old <- as.data.frame(data_IBTSQ1_old[data_IBTSQ1_old$cname == "IBTS-Q1" & 
                                                 data_IBTSQ1_old$slot == "index" &
                                                 !is.na(data_IBTSQ1_old$data),c(2,3,8)])

data_IBTSQ1_old$cname <- rep("IBTSQ1_old",dim(data_IBTSQ1_old)[1])

data_all <- rbind(data_IBTSQ1_old,data_IBTSQ1)


windows()
par(mfrow=c(3,2))

for(age_current in 1:5){
  
  data_ages <- data_all[data_all$age == age_current,]
  survey_names <- unique(data_ages$cname)
  
  res <- data.frame(matrix(ncol = length(survey_names), nrow = length(survey_names)))
  rownames(res) <- gsub('-', '', survey_names)
  colnames(res) <- gsub('-', '', survey_names)
  for(idxSurvey_current in 1:length(survey_names)){
    # current data
    data_current <- data_ages[data_ages$cname == survey_names[idxSurvey_current],]
    for (idxSurvey_comp in 1:length(survey_names)){
      # comparison data
      data_comp <- data_ages[data_ages$cname == survey_names[idxSurvey_comp],]
      # select common years
      year_vec <- intersect(data_current$year,data_comp$year)
      
      idx_year_comp <- which(data_comp$year %in% year_vec)
      idx_year_current <- which(data_current$year %in% year_vec)
      y <- data_comp[idx_year_comp,]$data
      x <- data_current[idx_year_current,]$data
      
      res_pearson <- cor.test(x, y,method = "pearson")
      #res[idxSurvey_current,idxSurvey_comp] <- as.numeric(gsub('cor', '', res_pearson$estimate))
      res[idxSurvey_current,idxSurvey_comp] <- cor(x,y)
    }
  }
  
  corrplot.mixed(as.matrix(res),lower = "ellipse", upper = "number",title=paste("age", age_current),mar=c(0,0,1,0))#, method = "ellipse", title=paste("age", age_current))
}

savePlot(file.path(".","results","miscellaneous","correlation_IBTSQ1.png"),type="png")

#dev.off()
