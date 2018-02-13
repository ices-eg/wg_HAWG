
##
#  script to compare the output of the assessment using the new Fprop and the 2017 assessment

### ============================================================================
### imports
### ============================================================================
library(FLSAM); library(FLEDA);library(ggplot2); library(grid); library(FLEDA); library(FLBRP); library(ggpubr);library(reshape2)

rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

### ======================================================================================================
### setting up folders
### ======================================================================================================

# local path
path <- "D:/git/wg_HAWG/NSAS/benchmark/"
try(setwd(path),silent=TRUE)

# paths
output.dir          <-  file.path(".","results/miscellaneous/")                #figures directory
data.source <- file.path(".","data")    #Data source, not code or package source!!!

### ======================================================================================================
### loading data
### ======================================================================================================

# load previous assessment in sequence
data_surveys <- readFLIndices(file.path(data.source, "fleet_3c_newIBTSQ3.txt"))

# catches
data_catches <- read.csv(file.path(data.source, "catches.csv"))
data_catches <- as.data.frame( data_catches[!is.na(data_catches$data),])
data_catches$X <- NULL
data_catches$unit <- NULL
data_catches$season <- NULL
data_catches$area <- NULL
data_catches$iter <- NULL

data_catches <- cbind(data_catches, rep("canum",dim(data_catches)[1]))
colnames(data_catches)[4] <- 'cname'

# IBTSQ3
data_IBTSQ3 <- as.data.frame(data_surveys)
data_IBTSQ3 <- as.data.frame( data_IBTSQ3[data_IBTSQ3$cname == "IBTS-Q3" & 
                                          data_IBTSQ3$slot == "index" &
                                          !is.na(data_IBTSQ3$data),c(1,2,8)])

# IBTSQ1
data_IBTSQ1 <- as.data.frame(data_surveys)
data_IBTSQ1 <- as.data.frame( data_IBTSQ1[data_IBTSQ1$cname == "IBTS-Q1" & 
                                            data_IBTSQ1$slot == "index" &
                                        !is.na(data_IBTSQ1$data),c(1,2,8)])

# IBTS0
data_IBTS0 <- as.data.frame(data_surveys)
data_IBTS0 <- as.data.frame( data_IBTS0[data_IBTS0$cname == "IBTS0" & 
                                        data_IBTS0$slot == "index" &
                                        !is.na(data_IBTS0$data),c(1,2,8)])

# HERAS
data_HERAS <- as.data.frame(data_surveys)
data_HERAS <- as.data.frame( data_HERAS[data_HERAS$cname == "HERAS" & 
                                        data_HERAS$slot == "index" &
                                        !is.na(data_HERAS$data),c(1,2,8)])

data_all <- as.data.frame(data_surveys)
data_all <- as.data.frame(data_all[data_all$slot == "index" &
                                   !is.na(data_all$data),])

data_all$slot = NULL
data_all$unit = NULL
data_all$season = NULL
data_all$area = NULL
data_all$iter = NULL

data_all <- rbind(data_all, data_catches)

### ======================================================================================================
### png printing
### ======================================================================================================

png(file.path(output.dir,paste("correlation_time_series - %02d.png")),units = "px", height=800,width=672, bg = "white")

### ======================================================================================================
### loading data
### ======================================================================================================
survey_name_mat <- unique(data_all$cname)

res <- data.frame(matrix(ncol = 5, nrow = 1))
colnames(res) <- c('age_comp', 'survey_name', 'survey_name_comp', 'corr', 'pvalue')
count <- 1
for(survey_name in survey_name_mat){
  current_survey <- as.data.frame(data_all[data_all$cname == survey_name &
                                             data_all$data != -1,])
  ages_current_survey <- unique(current_survey$age)
  
  for (age_comp in ages_current_survey){
    current_survey_age <- as.data.frame(current_survey[current_survey$age == age_comp,])
    for (survey_name_comp in survey_name_mat[survey_name_mat != survey_name]){
      comp_survey <- as.data.frame(data_all[data_all$cname == survey_name_comp &
                                            data_all$data != -1 &
                                            data_all$age == age_comp,])
      
      if(dim(comp_survey)[1]!=0){
        length(comp_survey)
        year_vec <- intersect(current_survey_age$year,comp_survey$year)
        idx_year_comp <- which(comp_survey$year %in% year_vec)
        idx_year_current <- which(current_survey_age$year %in% year_vec)
        y <- comp_survey[idx_year_comp,]$data
        x <- current_survey_age[idx_year_current,]$data
        
        my_data <- data.frame(x, y)
        colnames(my_data) <- c(gsub('-', '', survey_name), gsub('-', '', survey_name_comp))
        
        g <- ggscatter(my_data, x = gsub('-', '', survey_name), y = gsub('-', '', survey_name_comp), 
                       add = "reg.line", conf.int = TRUE, 
                       cor.coef = TRUE, cor.method = "pearson",
                       xlab = paste(survey_name, ' index'), ylab = paste(survey_name_comp, ' index'),
                       title = paste('age ', age_comp))
        print(g)
        
        res_pearson <- cor.test(x, y,method = "pearson")
        corr <- as.numeric(gsub('cor', '', res_pearson$estimate))
        pvalue <- res_pearson$p.value
        res <- rbind(res, data.frame(age_comp, survey_name, survey_name_comp, corr, pvalue))
      }
    }
  }
}
res <- res[2:dim(res)[1],]

write.csv(res, file.path(output.dir, 'stat_res.csv'))

dev.off()