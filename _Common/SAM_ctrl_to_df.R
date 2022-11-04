# ============================================================================
# Convert SAM configuration to data frame
#
# Martin Pastoors mpastoors@pelagicfish.eu
#
# 17/03/2021 first coding
# ============================================================================

require(tidyverse)

load("D:/GIT/wk_WKREBUILD/EqSimWHM/RData/WHOM_SAM19_fit.RData")
cfg            <- fit$conf

# for (i in names(cfg)) {
#   print(paste(i, class(cfg[[i]]), paste(dim(cfg[[i]]), collapse=","), is.null(cfg[[i]])))
# }

ages           = cfg$minAge:cfg$maxAge
fleets         = paste0("F",1:fit$data$noFleets)
fleettypes     = fit$data$fleetTypes

df <- data.frame(stringsAsFactors = FALSE)

for (i in names(cfg)) {
  
  t <- cfg[[i]]

  if(class(t) == "factor") {
    df <- bind_rows(df,
                    data.frame(parameter=i,
                               value = as.character(t),
                               fleet = fleets,
                               type  = "factor"))
  }

  if(class(t) == "numeric") {
    if ( length(t) == 0) {
      df <- bind_rows(df,
                      data.frame(parameter=i, value = as.character(NA), type="numeric"))
    } else if (length(t) == 1) {
      df <- bind_rows(df,
                      data.frame(parameter=i, value = as.character(t), type="numeric"))
    } else if (i %in% c("fbarRange")) {
      df <- bind_rows(df,
                      data.frame(parameter=i, value = as.character(t), type="numeric"))
    } else if (i %in% c("maxAgePlusGroup","keyBiomassTreat","fracMixObs")) {
        df <- bind_rows(df,
                        data.frame(parameter=i, value = as.character(t), 
                                   fleet = fleets, fleettypes=fleettypes, type="numeric"))
    } else if (i %in% c("keyVarLogN")) {
      df <- bind_rows(df,
                      data.frame(parameter=i, value = as.character(t), 
                                 age = as.character(ages), type="numeric"))
    }
  }
  
  if(grepl("matrix", class(t))) {
    if (i == "keyCorObs") {
      df <- bind_rows(
        df,
        t %>%
          reshape2::melt() %>%
          setNames(c('fleet', 'age', 'value')) %>%
          mutate(fleet = rep(fleets, times=length(ages)-1)) %>%
          mutate(fleettypes = rep(fleettypes, times=length(ages)-1)) %>%
          arrange(fleet, age) %>%
          mutate(age = rep(ages[c(1:length(ages)-1)], times=length(fleets))) %>%
          mutate_at(c("fleet","age","value"), as.character) %>%
          mutate(parameter = i) %>%
          mutate(type = "matrix")
      )
      
    } else if (dim(cfg[["keyParScaledYA"]])[1]==0) {
      df <- bind_rows(
        df,
        data.frame(parameter=i, value = NA, type="matrix")
      )
      
    } else {
      df <- bind_rows(
        df,
        t %>%
          reshape2::melt() %>%
          setNames(c('fleet', 'age', 'value')) %>%
          mutate(fleet = rep(fleets, times=length(ages))) %>%
          mutate(fleettypes = rep(fleettypes, times=length(ages))) %>%
          arrange(fleet, age) %>%
          mutate(age = rep(ages, times=length(fleets))) %>%
          mutate_at(c("fleet","age","value"), as.character) %>%
          mutate(parameter = i) %>%
          mutate(type = "matrix")
      )
      
    } # end of if statement special matrix cases
  } # end of if matrix statement
} # end of for loop

df <- 
  df %>% 
  ungroup() %>% 
  mutate(age = as.integer(age)) %>% 
  arrange(type, parameter, fleet, age)

glimpse(df)


