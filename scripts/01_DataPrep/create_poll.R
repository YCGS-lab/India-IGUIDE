#==============================================================================#
# I-GUIDE Flood Worry ###
# Construct Poll ####

# INPUTS:
# Cleaned Survey Data

# OUTPUTS:
# Recoded Poll Data

# Last modified: June 23, 2026, EG
#==============================================================================#
# 0. Set R Environment ####
#==============================================================================#
rm(list=ls())

library(tidyverse)
library(readxl)
library(openxlsx)

# Set parameters
year <- "2026" 
wave <- "05" 
surveyname <- "cvoter" 
model_no <- "2"
remove_states <- c("Andaman & Nicobar Islands", "Kargil", "Leh")

# Set file paths
github <- paste0("~/GitHub/India-IGUIDE/")
survey <- paste0("~/YSE Dropbox/Emily Goddard/ypcccdb/_data/surveys/india/",surveyname,year,"/output/")
dropbox <- paste0("~/YSE Dropbox/Emily Goddard/India-IGUIDE/_data/")
xwalk_path <- paste0(dropbox,"xwalks/")
temp <- paste0(github,"temp/")
setwd(github)

#==============================================================================#
# 1.0 Load Data ####
#==============================================================================#
# Load Survey Data
ccim <- readRDS(paste0(survey,"combined_full_india_",surveyname,"_w01-w",wave,"_",year,".rds"))

# Crosswalk
xwalk <- read.xlsx(paste0(xwalk_path, "xwalk_full.xlsx"))

# Covariates
covars <- readRDS(paste0(dropbox, "covariates/covariates.rds"))
#==============================================================================#
# 2.0 Subset Data ####
#==============================================================================#
poll <- ccim %>%
  dplyr::select(caseid, weight, wave, state, district, age, gender, caste, 
                religion, urban_rural, starts_with("n7")) %>%
  dplyr::filter(wave=="tapp", 
                !is.na(weight)) %>%
  dplyr::mutate(state_district = paste0(state, ", ", district)) %>%
  dplyr::distinct()

# Replace "Not Asked"
poll[poll=="Not asked"] <- NA
poll <- poll[, colSums(is.na(poll)) < nrow(poll)]

# Remove states
poll <- poll[!poll$district %in% remove_states,]

rm(ccim)
#==============================================================================#
# 3.0 Merge Covariates ####
#==============================================================================#
# Limit Covariate Data to Survey
state_dist <- unique(poll$state_district)

# Summarize Covariates as needed by survey data
covars <- covars %>%
  dplyr::filter(state_district_survey %in% state_dist) %>%
  dplyr::select(-contains("_shape23"), -contains("_census11")) %>%
  dplyr::group_by(state_survey, district_survey, state_district_survey) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  dplyr::distinct()

# Merge datasets
poll_covars <- base::merge(covars, poll, 
                           by.x=c("state_survey", "district_survey", "state_district_survey"), 
                           by.y=c("state", "district", "state_district"), 
                           all=TRUE) %>%
  dplyr::filter(!is.na(weight)) %>%
  dplyr::distinct()

# Check for Missing Data
countna <- function(x){sum(is.na(x))}
nans <- lapply(poll_covars, countna)

#==============================================================================#
# 4.0 Write Data ####
#==============================================================================#
# Write data
write_rds(poll, paste0(dropbox, "poll/poll.rds"))
write_rds(poll_covars, paste0(dropbox, "poll/poll_covariates.rds"))

write.xlsx(poll, paste0(dropbox, "poll/poll.xlsx"))
write.xlsx(poll_covars, paste0(dropbox, "poll/poll_covariates.xlsx"))
#==============================================================================#
# END OF FILE ####
#==============================================================================#
