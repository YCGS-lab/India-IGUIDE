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

#==============================================================================#
# 2.0 Load Covariates ####
#==============================================================================#

#==============================================================================#
# 3.0 Subset Data ####
#==============================================================================#
ccim <- ccim %>%
  dplyr::select(caseid, weight, wave, state, district, age, gender, caste, 
                religion, urban_rural, starts_with("n7")) %>%
  dplyr::filter(wave=="tapp") %>%
  dplyr::distinct()

# Merge Crosswalk
poll <- base::merge(ccim, xwalk, 
                    by.x=c("state", "district"),
                    by.y=c("state_survey", "district_survey"))

rm(ccim)
#==============================================================================#
# 4.0 Merge Covariates ####
#==============================================================================#







#==============================================================================#
# 7.0 Write Data ####
#==============================================================================#
write_rds(poll, paste0(dropbox, "poll/poll.rds"))
# write_rds(poll_covars, paste0(dropbox, "poll/poll_covariates.rds"))

#==============================================================================#
# END OF FILE ####
#==============================================================================#
