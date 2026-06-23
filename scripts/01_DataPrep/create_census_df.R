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

# Model for list of covariates needed
model <- readRDS("~/YSE Dropbox/Emily Goddard/India-IGUIDE/_data/_archive/best_model_03052026.rds")
# model[["var.names"]]

#==============================================================================#
# 2.0 Load Covariates ####
#==============================================================================#

#==============================================================================#
# 3.0 Clean Data ####
#==============================================================================#
# Merge with Crosswalk
df_district <- base::merge(xwalk, df_district, 
                           by.x=c("state_census11_code", "district_census11_code", "state_district_census11_code"),
                           by.y=c("state_code", "district_code", "state_dist_code"), 
                           all=TRUE)

#==============================================================================#
# 4.0 Merge Covariates ####
#==============================================================================#


#==============================================================================#
# 7.0 Write Data ####
#==============================================================================#
write_rds(df_district, paste0(dropbox, "census_df/df_district.rds"))
# write_rds(df_district_covars, paste0(dropbox, "census_df/df_district_covariates.rds"))

#==============================================================================#
# END OF FILE ####
#==============================================================================#
