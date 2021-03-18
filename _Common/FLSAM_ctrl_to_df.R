# ============================================================================
# Convert FLSAM configuration to data frame
#
# Martin Pastoors mpastoors@pelagicfish.eu
#
# 17/03/2021 first coding
# ============================================================================

# read data stuff
# library(FLCore)
# try(packageVersion("FLSAM")=="2.1.1", silent=TRUE)
# rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
# options(stringsAsFactors=FALSE)
# log.msg     <-  function(string) {cat(string);}
# log.msg("\nNSH Final Assessment (single fleet)\n=====================\n")
# path <- "D:/git/wg_HAWG/NSAS/"
# try(setwd(path),silent=TRUE)
# dir.create("assessment",showWarnings = FALSE)
# output.dir          <-  file.path(".","assessment/")              # result directory\
# script.dir          <-  file.path(".","side_scripts/")            # result directory
# n.retro.years       <-  12                                        # Number of years for which to run the retrospective
# assessment_name     <- 'NSH_HAWG2021_sf'
# library(FLSAM); library(FLEDA)
# addM <- 0.11
# source(file.path(script.dir,"setupAssessmentObjects_sf.r"))
# source(file.path(script.dir,"setupControlObject_sf.r"))
# ctrl <- NSH.ctrl

FLSAM_ctrl2df <- function(ctrl) {
  
  try(require(tidyverse))
  
  # fleets <- data.frame(rownames(ctrl@states)) %>% setNames("fleet") %>% mutate(fleetid=row_number())
  fleets  <- rownames(ctrl@states)
  fleetid <- 1:length(fleets)
  ages    <- ac(ctrl@range["min"]:ctrl@range["max"])
  
  df <- bind_rows(
    data.frame(stringsAsFactors = FALSE),
    data.frame(parameter="minAge", parameter2 = "range[min]", 
               value = ac(ctrl@range["min"])),
    data.frame(parameter="maxAge", parameter2 = "range[max]",
               value = ac(ctrl@range["max"])),
    data.frame(parameter="maxAgePlusGroup", parameter2 = "plus.group",
               value = ac( ifelse(ctrl@plus.group, 1, 0))),
    data.frame(parameter  ="fbarRangeMin", parameter2 = "range[minfbar]",
               value = ac(ctrl@range[c("minfbar")])),
    data.frame(parameter  ="fbarRangeMax", parameter2 = "range[maxfbar]",
               value = ac(ctrl@range[c("maxfbar")])),
    data.frame(parameter  ="corFlag", parameter2 = "cor.F", 
               value = ac(ctrl@cor.F)),
    data.frame(parameter  ="stockRecruitmentModelCOde", parameter2 = "srr", 
               value = ac(ctrl@srr)),
    data.frame(parameter  ="obsLikelihoodFlag", parameter2 = "likFlag", 
               value = ac(ctrl@likFlag)),
    data.frame(parameter  ="conf$fixVarToWeight", 
               value = ac(0)),

    # by fleet and age
    melt(ctrl@states) %>% 
      mutate(
        parameter  = "keyLogFsta", parameter2 = "states",
        age        = ac(age),
        fleetid    = rep(fleetid, length(ages)),
        value      = ac(value)),
    melt(ctrl@catchabilities) %>% 
      mutate(
        parameter  = "keyLogFpar", parameter2 = "catchabilities",
        age        = ac(age),
        fleetid    = rep(fleetid, length(ages)),
        value      = ac(value)),
    melt(ctrl@power.law.exps) %>% 
      mutate(
        parameter  = "keyQpow", parameter2 = "power.law.exps",
        age        = ac(age),
        fleetid    = rep(fleetid, length(ages)),
        value      = ac(value)),
    melt(ctrl@f.vars) %>% 
      mutate(
        parameter  = "keyVarF", parameter2 = "f.vars",
        age        = ac(age),
        fleetid    = rep(fleetid, length(ages)),
        value      = ac(value)),
    melt(ctrl@obs.vars) %>% 
      mutate(
        parameter  = "keyVarObs", parameter2 = "obs.vars",
        age        = ac(age),
        fleetid    = rep(fleetid, length(ages)),
        value      = ac(value)),
    melt(ctrl@cor.obs) %>% 
      mutate(
        parameter  = "keyCorObs", parameter2 = "cor.obs",
        age        = ac(age),
        fleetid    = rep(fleetid, length(ages)-1),
        value      = ac(value)),
    
    # by fleet
    melt(ctrl@cor.obs.Flag) %>% 
      rownames_to_column(var="fleet") %>%  
      mutate(
        parameter  = "obsCorStruct", parameter2 = "cor.obs.Flag",
        fleet      = fleets,
        fleetid    = fleetid,
        value      = factor(value, levels = c("ID", "AR", "US"))),
    melt(ctrl@biomassTreat) %>% 
      rownames_to_column(var="fleet") %>%  
      mutate(
        parameter  = "keyBiomassTreat", parameter2 = "biomassTreat",
        fleet      = fleets,
        fleetid    = fleetid,
        value      = ac(value)),
    
    # by age
    melt(ctrl@logN.vars) %>% 
      rownames_to_column(var="age") %>%  
      mutate(
        parameter  = "keyVarLogN", parameter2 = "logN.vars",
        age        = ac(age),
        value      = ac(value)),
    
    # scaled years
    data.frame(parameter  ="noScaledYears", parameter2 = "scaleNoYear", 
               value = ac(ctrl@scaleNoYears)),
    
    {if (ctrl@scaleNoYears > 0) {
      melt(ctrl@scaleYears) %>% 
        rownames_to_column(var="year") %>% 
        mutate(
          parameter = "keyScaledYears", parameter2 = "scaleYears",
          value     = ac(value))
    } else {
      data.frame(parameter  ="keyScaledYears", parameter2 = "scaleYears", 
                 value = "numeric()")
    }},
    {if (ctrl@scaleNoYears > 0) {
      melt(ctrl@scalePars) %>% 
        rownames_to_column(var="check") %>% 
        mutate(
          parameter = "keyParScaledYA", parameter2 = "scalePars",
          value     = ac(value))
    } else {
      data.frame(
        parameter = "keyParScaledYA", parameter2 = "scalePars",
        value = "0 x 0 matrix")
    }},
    
    # logP.vars
    {if (is.na(ctrl@logP.vars[1])) {
      data.frame(
        parameter = "keyVarLogP", parameter2 = "logP.vars",
        value = "numeric(0)")
    } else {
      melt(ctrl@logP.vars) %>% 
        rownames_to_column(var="check") %>% 
        mutate(
          parameter = "keyVarLogP", parameter2 = "logP.vars",
          value     = ac(value))
    }},
    
  )

  df
}


df <- FLSAM_ctrl2df(ctrl)

write.csv(df, "FLSAM_ctrl_df.csv", row.names = FALSE)


