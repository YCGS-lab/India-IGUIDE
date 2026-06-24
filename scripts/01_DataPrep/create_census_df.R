#==============================================================================#
# I-GUIDE Flood Worry ###
# Construct Census DF ####

# INPUTS:
# Cleaned Census Data

# OUTPUTS:
# Census Post-Strat Weights

# Last modified: June 23, 2026, EG
#==============================================================================#
# 0. Set R Environment ####
#==============================================================================#
rm(list=ls())

library(tidyverse)
library(readxl)

# Set parameters
model_no <- "2"
remove_states <- c("31", "35") #Andaman & Nicobar Islands, Lakshadweep
remove_codes <- c("01003", "01004") # Ladakh

# Set file paths
github <- paste0("~/GitHub/ypccc_india/india_w",model_no,"/")
dropbox <- paste0("~/YSE Dropbox/Emily Goddard/India-IGUIDE/_data/")
xwalk_path <- paste0(dropbox,"xwalks/")
temp <- paste0(github,"temp/")
setwd(github)

#==============================================================================#
# 1.0 Load Data ####
#==============================================================================#
# Census DF
df_district <- readRDS(paste0(temp,"df_district.rds"))

# Crosswalk
xwalk <- read.xlsx(paste0(xwalk_path, "xwalk_full.xlsx"))

# Covariates
covars <- readRDS(paste0(dropbox, "covariates/covariates.rds"))
#==============================================================================#
# 2.0 Clean Data ####
#==============================================================================#
# Merge with Crosswalk
df_district <- base::merge(xwalk, df_district, 
                           by.x=c("state_census11_code", "district_census11_code", "state_district_census11_code"),
                           by.y=c("state_code", "district_code", "state_dist_code"), 
                           all=TRUE)

# Remove disputed areas
df_district <- df_district %>%
  dplyr::filter(!state_census11_code %in% remove_states, 
                !state_district_census11_code %in% remove_codes)

rm(xwalk)
#==============================================================================#
# 3.0 Merge Covariates ####
#==============================================================================#
# Find overlapping columns
cols <- colnames(df_district)[colnames(df_district) %in% colnames(covars)]

# Merge datasets
df_district_covars <- base::merge(df_district, covars, by=cols, all=TRUE)

#==============================================================================#
# 4.0 Write Data ####
#==============================================================================#
# Check for Missing Data
countna <- function(x){sum(is.na(x))}
nans <- lapply(df_district_covars, countna)

# Write data
write_rds(df_district, paste0(dropbox, "census_df/df_district.rds"))
write_rds(df_district_covars, paste0(dropbox, "census_df/df_district_covariates.rds"))

write.xlsx(df_district, paste0(dropbox, "census_df/df_district.xlsx"))
write.xlsx(df_district_covars, paste0(dropbox, "census_df/df_district_covariates.xlsx"))
#==============================================================================#
# END OF FILE ####
#==============================================================================#
