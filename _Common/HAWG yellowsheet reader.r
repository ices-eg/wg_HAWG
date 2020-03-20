# ==================================================================
# HAWG yellowsheet reader
#
# 11/03/2020: first coding
# ==================================================================

# Reset lists
# rm(list=ls())

# general libraries
library(tidyverse)     # data manipulation and piping
library(readxl)        # excel reader from hadley; much quicker than the java based excel readers
library(lubridate)     # data and time functions
library(stringr)       # string manipulation
library(pander)        # for tables
library(reshape2)      # cast and melt dataframes
library(scales)        # pretty scales

# source utilities
source("../prf/r/my utils.R")

data_path <- "D:/iHAWG/HAWG_accessions/Data call 2020/HER"

onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF') 
load(file.path(onedrive, "rdata/length_raised.RData"))

# read_catch function -------------------------------------------------------------------

read_catch <- function(file, ws, member, species, year) {
  
  print(ws)
  
  t1 <- 
    read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "B12:M13") %>% 
    t() %>% 
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(c("quarter", "year")) %>% 
    mutate(quarter = as.numeric(stringr::str_match_all(quarter, "[0-9]+")) ) %>%
    zoo::na.locf(.)                       # fill the empty values with previous values
  
  t <- 
    read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "B15:M20") %>% 
    t() %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    setNames(c("variable","unit","fleet1","fleet2","fleet3","fleet4")) %>%
    bind_cols(t1) %>% 
    gather(key=fleet, value=value, 3:6) %>% 
    mutate(member=member, species=species, year=as.integer(year), file=basename(file))  %>% 
    filter(!is.na(value))
  
  # generate dataframe is not all empty
  if (nrow(t) > 0) return(t)
  
} # end of function

i <- 1
j <- 4
ws = j
file = file.list[i]

# read_sampling function -------------------------------------------------------------------

read_sampling <- function(file, ws, member, species, year) {
  
  print(ws)
  
  t1 <- 
    read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "B8:Q8") %>% 
    t() %>% 
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(c("quarter")) %>%
    mutate(quarter = as.numeric(stringr::str_match_all(quarter, "[0-9]+")),
           quarter = ifelse(quarter > 4, NA, quarter),
           year    = year) %>%
    zoo::na.locf(.)                        # fill the empty values with previous values

  t <- 
    read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "B9:Q14") %>% 
    t() %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    setNames(c("variable","unit","fleet1","fleet2","fleet3","fleet4")) %>%
    bind_cols(t1) %>% 
    gather(key=fleet, value=value, 3:6) %>% 
    mutate(member=member, species=species, year=year, file=basename(file))  %>% 
    filter(!is.na(value))
  
  # generate dataframe is not all empty
  if (nrow(t) > 0) return(t)
  
} # end of function

# read_length function -------------------------------------------------------------------

# file       <- file.list[8]
# worksheets <- excel_sheets(file)
# ws         <- worksheets[grepl("length", tolower(worksheets))][1]
# member     <- as.character(read_excel(file, sheet = 1, col_names = FALSE, col_types = "text", range = "F7"))
# species    <- as.character(read_excel(file, sheet = 1, col_names = FALSE, col_types = "text", range = "F6"))
# year       <- as.integer  (read_excel(file, sheet = 1, col_names = FALSE, col_types = "numeric", range = "K4"))

read_length <- function(file, ws, member, species, year) {
  
  print(ws)
  
  fleet      <- gsub("length ","", tolower(ws))
  lengthtype <- as.character(read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", range = "A8") )
  lengthunit <- as.character(read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", range = "A9"))
  # unit       <- as.character(read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", range = "F5") )
  # need to sort out how to deal with missing units !!
  
  t <- 
    read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "B10:F84") %>% 
    setNames(c("length","quarter1","quarter2","quarter3","quarter4")) %>%
    gather(key=quarter, value=value, quarter1:quarter4) %>% 
    mutate(member=member, species=species, year=year, file=basename(file), fleet=fleet,
           lengthtype=lengthtype, lengthunit = lengthunit)  %>% 
    mutate(quarter = as.numeric(stringr::str_match_all(quarter, "[0-9]+")) ) %>%
    mutate(value   = as.numeric(gsub("\\s+", "", value) )) %>% 
    mutate(length  = as.numeric(length)) %>% 
    filter(!is.na(value)) %>% 
    group_by(member, fleet, species, year, quarter, lengthtype) %>% 
    arrange(member, fleet, species, year, quarter, lengthtype, length) %>% 
    mutate(prop = value / sum(value)) %>% 
    mutate(cumvalue = cumsum(value),
           cumprop = cumsum(prop)) %>% 
    ungroup()
  
  
  # generate dataframe is not all empty
  if (nrow(t) > 0) print(head(t))
  
  if (nrow(t) > 0) return(t)
  
} # end of function


# read_alk function -------------------------------------------------------------------

# file       <- file.list[1]
# worksheets <- excel_sheets(file)
# ws         <- worksheets[grepl("ALK", worksheets)][1]
# member     <- as.character(read_excel(file, sheet = 1, col_names = FALSE, col_types = "text", range = "F7"))
# species    <- as.character(read_excel(file, sheet = 1, col_names = FALSE, col_types = "text", range = "F6"))
# year       <- as.integer  (read_excel(file, sheet = 1, col_names = FALSE, col_types = "numeric", range = "K4"))

read_alk <- function(file, ws, member, species, year) {
  
  print(ws)
  
  fleet      <- gsub("ALK ","", ws)
  lengthtype <- as.character(read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", range = "A10") )
  lengthunit <- as.character(read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", range = "A11") )

  # ages
  a <- 
    read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", range = "B11:N11") %>% 
    mutate_all( ~ paste0("age",.)) %>% 
    as.character() 
  
  t1 <- 
    read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "A12:N111") %>% 
    setNames(c("length",a)) %>%
    mutate(quarter=1) %>% 
    gather(key=age, value=value, a) %>% 
    filter(!is.na(value))
  
  t2 <- 
    read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "P12:AC111") %>% 
    setNames(c("length",a)) %>%
    mutate(quarter=2) %>% 
    gather(key=age, value=value, a) %>% 
    filter(!is.na(value))

  t3 <- 
    read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "AE12:AR111") %>% 
    setNames(c("length",a)) %>%
    mutate(quarter=3) %>% 
    gather(key=age, value=value, a) %>% 
    filter(!is.na(value))
  
  t4 <- 
    read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "AT12:BG111") %>% 
    setNames(c("length",a)) %>%
    mutate(quarter=4) %>% 
    gather(key=age, value=value, a) %>% 
    filter(!is.na(value))

  t <-
    bind_rows(t1, t2, t3, t4) %>% 
    mutate(member=member, species=species, year=year, file=basename(file), fleet=fleet,
           lengthtype=lengthtype, lengthunit = lengthunit)  %>% 
    mutate(value   = as.numeric(gsub("\\s+", "", value) )) %>% 
    mutate(length  = as.numeric(length)) %>% 
    mutate(age     = as.integer(gsub("age","",age))) %>% 
    group_by(member, fleet, species, year, quarter, length) %>% 
    mutate(prop = value / sum(value)) %>% 
    ungroup()
    

  # generate dataframe is not all empty
  if (nrow(t) > 0) return(t)
  
} # end of function



# Create file list for import 
file.list   <- list.files(
      path = data_path, 
      recursive  = FALSE,
      pattern    = "*\\.xls*",
      full.names = TRUE,
      ignore.case= TRUE)
file.list <- file.list[!grepl("\\~",file.list)]

# create empty dataframes
catch <- sampling <- length <- alk <- data.frame()

i <- 1
j <- 3
ws <- "catch data"

# start file reading loop
for (i in 1:length(file.list)){                                           

  
  # get name and number of worksheets in the file
  file        <- file.list[i]
  worksheets  <- excel_sheets(file)
  nworksheets <- length(worksheets)
  
  print(paste(i, basename(file), sep=" "))
  
  # generic information
  member <- as.character(read_excel(file, sheet = 1, col_names = FALSE, col_types = "text", range = "F7"))
  species<- as.character(read_excel(file, sheet = 1, col_names = FALSE, col_types = "text", range = "F6"))
  year   <- as.integer  (read_excel(file, sheet = 1, col_names = FALSE, col_types = "numeric", range = "K4"))
  
  # catch
  # for (ws in worksheets[grepl("catch", tolower(worksheets))]) {
  #   catch <- bind_rows(catch, read_catch(file=file, ws=ws, member=member, species=species, year=year)) }
  
  # sampling
  # for (ws in worksheets[grepl("sampling", tolower(worksheets))]) {
  #   sampling <- bind_rows(sampling, read_sampling(file=file, ws=ws, member=member, species=species, year=year)) }

  # length
  for (ws in worksheets[grepl("length", tolower(worksheets))]) {
    length <- bind_rows(length, read_length(file=file, ws=ws, member=member, species=species, year=year)) }

  # alk
  # for (ws in worksheets[grepl("alk", tolower(worksheets))]) {
  #   alk <- bind_rows(alk, read_alk(file=file, ws=ws, member=member, species=species, year=year)) }
  
} # end of i loop

# plot cumulative proportions at length
length %>%   
  ggplot(aes(x=length, y=cumprop, group=member)) + 
  theme_publication() +
  theme(panel.spacing     = unit(2, "mm"),
        legend.key.width  = unit(1, "cm"),
        legend.title      = element_blank(),
        panel.grid.minor  = element_blank(),
        # legend.position = "null",
        text              = element_text(size=12) ) +
  
  labs(x = "length", y = "prop") +
  
  geom_line(aes(colour=member, group=member), alpha=1) +
  facet_grid(fleet~quarter)
  # facet_grid(.~quarter)

t <-
  length_raised %>% 
  filter(year == 2019, species == "her") %>%
  filter(division %in% c("27.4.a","27.4.b","27.7.d")) %>% 
  mutate(length = floor(length)) %>% 
  mutate(fleet = case_when(
    division == "27.4.a" ~ "iva",
    division == "27.4.b" ~ "ivb",
    division == "27.7.d" ~ "viid",
    TRUE                 ~ division
  )) %>% 
  group_by(fleet, quarter, length) %>% 
  summarize(catchnumber = sum(catchnumber, na.rm=TRUE)) %>% 
  group_by(fleet, quarter) %>% 
  mutate(prop = catchnumber / sum(catchnumber, na.rm=TRUE)) %>% 
  mutate(member = "PFA")



length %>%   
  filter(!grepl("discard", fleet)) %>% 
  filter(length >= 20) %>% 
  
  mutate(fleet = ifelse(grepl("iva", fleet), "iva", fleet)) %>% 
  filter(fleet %in% c("iva", "ivb", "viid")) %>% 
  
  group_by(quarter, length, fleet, member) %>% 
  summarize(prop = sum(prop, na.rm=TRUE)) %>% 
  
  ggplot(aes(x=length, y=prop, group=member)) + 
  theme_publication() +
  theme(panel.spacing     = unit(2, "mm"),
        legend.key.width  = unit(1, "cm"),
        legend.title      = element_blank(),
        panel.grid.minor  = element_blank(),
        # legend.position = "null",
        text              = element_text(size=12) ) +
  
  labs(x = "length", y = "xxx") +
  
  geom_line(aes(colour=member, group=member), alpha=1) +
  geom_line(data=t, colour="darkgray", size=1, alpha=1) +
  facet_grid(fleet~quarter, scales = "free_y")


# plot age-length keys
alk %>%   
  filter(year == 2018) %>% 
  mutate(quarter = paste0("Q", quarter)) %>% 
  
  ggplot(aes(x=length, y=age, group=quarter)) + 
  theme_publication() +
  theme(panel.spacing     = unit(2, "mm"),
        legend.key.width  = unit(1, "cm"),
        legend.title      = element_blank(),
        panel.grid.minor  = element_blank(),
        # legend.position = "null",
        text              = element_text(size=12) ) +
  
  labs(x = "length", y = "age") +
  
  geom_point(aes(size=prop, colour=member, shape=factor(year)), alpha=0.5) +
  # facet_grid(fleet~quarter)
  facet_grid(.~quarter)






