#==============================================================================#
# I-GUIDE Flood Worry ###
# Clean Higher Education Data ####

# INPUTS:
# Higher Ed Covariates

# OUTPUTS:
# Cleaned Higher Ed Data

# Last modified: June 23, 2026, EG
#==============================================================================#
# 0. Set R Environment ####
#==============================================================================#
rm(list=ls())

library(plyr)
library(tidyverse)
library(ggplot2)
library(sf)

# Identify states to remove from data (not mapping these)
remove_states <- c("Ladakh","Lakshadweep","Andaman & Nicobar")
remove_codes <- c("01003","01004","31587","35640","35638","35639")

# Set file paths
external <- paste0("~/Dropbox (YSE)/ypcccdb/_data/external/india/")
dropbox <- paste0("~/YSE Dropbox/Emily Goddard/India-IGUIDE/_data/")
xwalk_path <- paste0(dropbox,"xwalks/")

#==============================================================================#
# 1.0 Load Data ###
#==============================================================================#
# Education Data
load(paste0(external,"literacy/census.district.Rda"))

# Crosswalk
xwalk <- read.xlsx(paste0(xwalk_path, "xwalk_full.xlsx"))

#==============================================================================#
# 2.0 Clean and Combine Data ###
#==============================================================================#
# Fix Telangana Districts
cd$state.code <- as.character(cd$state.code)
cd$district.code <- as.character(cd$district.code)
cd$state.code[cd$district %in% xwalk$district_survey[xwalk$state_survey=="Telangana"] & cd$state=="ANDHRA PRADESH"] <- "36"
cd$district.code[cd$state.code=="36"] <- gsub("^28","36",cd$district.code[cd$state.code=="36"])

# Literacy by District
higher_ed <- cd %>% 
  dplyr::group_by(district.code) %>% 
  dplyr::mutate(total_pop = sum(population)) %>% 
  dplyr::group_by(district.code, edu) %>% 
  dplyr::mutate(pop_by_edu = sum(population),
                pct_edu = pop_by_edu / total_pop) %>%
  dplyr::rename(state_dist_code = district.code) %>%
  dplyr::select(state_dist_code, edu, pct_edu) %>% 
  dplyr::filter(!state_dist_code %in% remove_codes) %>%
  dplyr::distinct() %>% 
  dplyr::filter(edu %in% c("HigherSecondaryAbove"))
higher_ed$edu <- paste0(higher_ed$edu,"_district")
higher_ed <- spread(higher_ed, key=edu, value = pct_edu)

#==============================================================================#
# 3.0 Write Data ####
#==============================================================================#
write_rds(higher_ed, paste0(dropbox, "covariates/higher_ed.rds"))

#==============================================================================#
# END OF FILE ####
#==============================================================================#
