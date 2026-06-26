#==============================================================================#
# I-GUIDE Flood Worry ###
# Clean Covariates ####

# INPUTS:
# Model Covariate

# OUTPUTS:
# Cleaned and Merged Covariate Data

# Last modified: June 23, 2026, EG
#==============================================================================#
# 0. Set R Environment ####
#==============================================================================#
rm(list=ls())

library(plyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(openxlsx)

# Identify states to remove from data (not mapping these)
remove_states <- c("Ladakh","Lakshadweep","Andaman & Nicobar")
remove_codes <- c("01003","01004","31587","35640","35638","35639")

# Set file paths
external <- paste0("~/Dropbox (YSE)/ypcccdb/_data/external/india/")
dropbox <- paste0("~/YSE Dropbox/Emily Goddard/India-IGUIDE/_data/")
covar_path <- paste0(dropbox, "covariates/")
xwalk_path <- paste0(dropbox,"xwalks/")
setwd(covar_path)

#==============================================================================#
# 1.0 Load Data ###
#==============================================================================#
# Model for list of covariates needed
model <- readRDS("~/YSE Dropbox/Emily Goddard/India-IGUIDE/_data/_archive/best_model_03052026.rds")
model[["var.names"]]

# Education Data
load(paste0(external,"literacy/census.district.Rda"))

# Extreme Weather Vulnerability
ceew_d <- read.xlsx(paste0(external,"vulnerability_data/CEEW/CEEW - CVAT Main sheet 15Jun23.xlsx"), sheet = "District")

# Nightime lights
# lights <- read.csv("India_District_Medain_NTL_2024.csv")

# Precipitation / Temperature
precip_temp <- read.csv("Latest_India_District_Merged_NTL_Precp_Temp_2024.csv")

# News media
news <- read.csv("news_data_updated.csv")
news_key <- read.xlsx("news_key.xlsx")

# Flow accumulation
flow <- read.csv("India_FlowAccumulation.csv")

# Historical floods
hist_floods <- read.csv("India_HistoricalFloods.csv")

# Alpha Earth
embeds <- list.files(path = paste0(covar_path, "Embeddings_Stats_30m/"), pattern = paste0(".csv"), full.names=TRUE)
alpha_earth <- lapply(embeds, read.csv)
embeds <- Reduce(function(x, y) merge(x, y, by = c("di_code", "st_code", "state23", "dist23"), all = TRUE), alpha_earth)

# Spatial Covariates
spat <- read.csv(paste0(dropbox,"/_archive/district_level_spatial_features_covariates.csv"))

# Crosswalk
xwalk <- read.xlsx(paste0(xwalk_path, "xwalk_full.xlsx"))

# TEMP
df2 <- read.csv(paste0(dropbox,"/_archive/district_level_spatial_features_covariates.csv"))

rm(alpha_earth)
#==============================================================================#
# 2.0 Clean and Combine Data ###
#==============================================================================#
#------------------------------------------------------------------------------#
# 2.1 Higher Education ###
#------------------------------------------------------------------------------#
# Fix Telangana Districts
cd$state.code <- as.character(cd$state.code)
cd$district.code <- as.character(cd$district.code)
cd$state.code[cd$district %in% xwalk$district_survey[xwalk$state_survey=="Telangana"] & cd$state=="ANDHRA PRADESH"] <- "36"
cd$state.code[cd$district.code %in% c("28537","28538")] <- "36"
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
  dplyr::filter(edu %in% c("HigherSecondaryAbove")) %>%
  dplyr::mutate(edu = paste0(str_to_lower(edu),"_district"))
covars <- spread(higher_ed, key=edu, value = pct_edu)

# Merge Crosswalk
covars <- base::merge(xwalk, covars, 
                      by.x="state_district_census11_code", 
                      by.y="state_dist_code", 
                      all = TRUE)

rm(cd, higher_ed, xwalk)
#------------------------------------------------------------------------------#
# 2.2 Vulnerability
#------------------------------------------------------------------------------#
ceew_d <- ceew_d %>%
  dplyr::mutate(vulnerability_district = as.numeric(Vulnerability.Index),
                district_code = ifelse(nchar(Census.2011.Code)==1, paste0("00",Census.2011.Code),
                                 ifelse(nchar(Census.2011.Code)==2, paste0("0",Census.2011.Code), Census.2011.Code))) %>%
  dplyr::select(district_code, vulnerability_district) %>%
  dplyr::distinct()

# Replace Missing Data with 0's (no vulnerability because no event)
ceew_d$vulnerability_district[is.na(ceew_d$vulnerability_district)] <- 0

# Merge
covars <- base::merge(covars, ceew_d, 
                      by.x=c("district_census11_code"), 
                      by.y=c("district_code"),
                      all.x=TRUE)

rm(ceew_d)
#------------------------------------------------------------------------------#
# 2.3 Precip / Temp ###
#------------------------------------------------------------------------------#
precip_temp <- precip_temp %>%
  dplyr::select(-prep_year, -state, -districts) %>%
  dplyr::mutate(st_code = ifelse(nchar(st_code)==1, paste0("0",st_code), st_code),
                di_code = ifelse(nchar(di_code)==1, paste0("00",di_code),
                                 ifelse(nchar(di_code)==2, paste0("0",di_code), di_code)))
  
covars <- base::merge(covars, precip_temp, 
                      by.x=c("state_shape23_code", "district_shape23_code"),
                      by.y=c("st_code", "di_code"),
                      all.x=TRUE)

rm(precip_temp)
#------------------------------------------------------------------------------#
# 2.4 Alpha Earth Embeddings ###
#------------------------------------------------------------------------------#
embeds <- embeds %>%
  dplyr::select(-state23, -dist23) %>%
  dplyr::mutate(st_code = ifelse(nchar(st_code)==1, paste0("0",st_code), st_code),
                di_code = ifelse(nchar(di_code)==1, paste0("00",di_code),
                                 ifelse(nchar(di_code)==2, paste0("0",di_code), di_code)))

covars <- base::merge(covars, embeds, 
                      by.x=c("state_shape23_code", "district_shape23_code"),
                      by.y=c("st_code", "di_code"),
                      all.x=TRUE)

rm(embeds)
#------------------------------------------------------------------------------#
# 2.5 Flow Accumulation ###
#------------------------------------------------------------------------------#
flow <- flow %>%
  dplyr::select(-system.index, -Shape_Area, -Shape_Leng, -.geo, -GEOID, -GeoName) %>%
  dplyr::mutate(st_code = ifelse(nchar(st_code)==1, paste0("0",st_code), st_code),
                di_code = ifelse(nchar(di_code)==1, paste0("00",di_code),
                                 ifelse(nchar(di_code)==2, paste0("0",di_code), di_code)))

covars <- base::merge(covars, flow, 
                      by.x=c( "district_shape23_code", "district_shape23", "state_shape23_code","state_district_shape23", "state_shape23"),
                      by.y=c("di_code", "dist23", "st_code", "st_di23", "state23"),
                      all.x=TRUE)

rm(flow)
#------------------------------------------------------------------------------#
# 2.5 Historical Flood ###
#------------------------------------------------------------------------------#
hist_floods <- hist_floods %>%
  dplyr::select(-system.index, -Shape_Area, -Shape_Leng, -.geo, -GEOID, -GeoName) %>%
  dplyr::mutate(st_code = ifelse(nchar(st_code)==1, paste0("0",st_code), st_code),
                di_code = ifelse(nchar(di_code)==1, paste0("00",di_code),
                                 ifelse(nchar(di_code)==2, paste0("0",di_code), di_code)))

covars <- base::merge(covars, hist_floods, 
                      by.x=c( "district_shape23_code", "district_shape23", "state_shape23_code","state_district_shape23", "state_shape23"),
                      by.y=c("di_code", "dist23", "st_code", "st_di23", "state23"),
                      all.x=TRUE)

rm(hist_floods)
#------------------------------------------------------------------------------#
# 2.6 Spatial Covariates ###
#------------------------------------------------------------------------------#
spat <- spat %>%
  dplyr::mutate(di_code = ifelse(nchar(di_code)==1, paste0("00",di_code),
                                 ifelse(nchar(di_code)==2, paste0("0",di_code), di_code)))

covars <- base::merge(covars, spat, by.x=c("district_shape23_code"), by.y=c("di_code"),
                      all.x=TRUE)

rm(spat)
#------------------------------------------------------------------------------#
# 2.7 News Media ###
#------------------------------------------------------------------------------#
# Filter news data
news <- news %>%
  dplyr::select(EventType, Country, District_Clean, State_Clean, 
                starts_with("Tone")) %>%
  dplyr::filter(State_Clean != "",
                District_Clean != "")

# Merge with shapefile names
news <- base::merge(news, news_key, by=c("District_Clean", "State_Clean"), all.x=TRUE)

# Calculate Flood News Count
news_flood <- news %>%
  dplyr::mutate(flood_news_count = ifelse(EventType=="Flood", 1, 0)) %>%
  dplyr::filter(!is.na(District_Match)) %>%
  dplyr::group_by(District_Match) %>%
  dplyr::summarise(flood_news_count = sum(flood_news_count)) %>%
  dplyr::distinct()

# Take Average of tone by shapefile districts
news <- news %>%
  dplyr::filter(!is.na(District_Match)) %>%
  dplyr::group_by(District_Match) %>%
  dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  dplyr::distinct()

# Merge Flood news and tone
news <- base::merge(news, news_flood, by="District_Match", all=TRUE)

# Merge with covariates
covars <- base::merge(covars, news,
                      by.x=c("district_shape23"),
                      by.y=c("District_Match"),
                      all.x=TRUE)

rm(news, news_key, news_flood, df2)
#==============================================================================#
# 3.0 Write Data ####
#==============================================================================#
setdiff(model[["var.names"]], colnames(covars))

# Check for Missing Data
covars <- covars[!is.na(covars$zone),]
countna <- function(x){sum(is.na(x))}
nans <- lapply(covars, countna)

# Write Data
write_rds(covars, paste0(covar_path, "covariates.rds"))

#==============================================================================#
# END OF FILE ####
#==============================================================================#
