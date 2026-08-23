#==============================================================================#
# I-GUIDE Flood Worry ###
# Construct Shapefiles ####

# INPUTS:
# Raw Shapefile Data

# OUTPUTS:
# Cleaned Shapefile Data

# Last modified: June 23, 2026, EG
#==============================================================================#
# 0. Set R Environment ####
#==============================================================================#
rm(list=ls())

shpyear <- "2023"

library(sf)
library(tidyverse)


# Set file paths
dropbox <- paste0("~/Dropbox (YSE)/ypcccdb/_data/external/india/shapefiles/Survey_of_India/",shpyear,"/Administrative Boundary Database/")

#==============================================================================#
# 1. Load Dataset ####
#==============================================================================#
# Load Shapefile
shp <- read_sf(dsn=paste0(dropbox),
               layer = "DISTRICT_BOUNDARY")

# Load Population Counts
df_district <- readRDS(paste0("~/YSE Dropbox/Emily Goddard/India-IGUIDE/_data/census_df/df_district.rds"))

#==============================================================================#
# 2.0 Clean Data ####
#==============================================================================#
# Fix Text
shp$state_district <- paste0(shp$STATE, ", ", shp$District)
shp$DISTRICT_L <- ifelse(shp$DISTRICT_L=="NOT AVAILABLE", "xxx",shp$DISTRICT_L)

# Remove unnecessary columns for downscaling, reorder, and rename
district_shapefile <- shp %>%
  dplyr::select(STATE, State_LGD, District, DISTRICT_L, state_district, geometry) %>%
  dplyr::rename(state = STATE) %>%
  
  # Clean Text
  dplyr::mutate(dist = gsub("  "," ",District),
                st_cd = str_pad(State_LGD, 2, pad = "0"), 
                dist_cd = str_pad(DISTRICT_L, 3, pad = "0"),
                st_dist_cd = paste0(st_cd, dist_cd),
                GeoName = paste0(state,", ",dist),
                GeoID = paste0("x",st_dist_cd)) %>%
  dplyr::filter(!startsWith(state,"DISPUTED"),
                !startsWith(dist,"DISPUTED")) %>%
  dplyr::filter(!is.na(dist)) %>%
  dplyr::select(GeoName, GeoID, state, st_cd, dist, dist_cd) %>%
  dplyr::arrange(st_cd, dist_cd) %>%
  dplyr::distinct()

rm(shp)
#==============================================================================#
# 3.0 Merge Population Counts Data ####
#==============================================================================#
# Subset Census Data
population <-  df_district %>%
  dplyr::select(-GEOID, -age, -gender, -urban, -caste, -n, -n_pct_geo, -country,
                -ends_with("_survey"), -contains("census11")) %>%
  dplyr::mutate(state_district_shape23_code = paste0("x", state_district_shape23_code)) %>%
  dplyr::filter(!is.na(state_shape23)) %>%
  dplyr::distinct()

# Merge Data
district_shapefile <- base::merge(district_shapefile, population, 
                                  by.x=c("GeoName", "GeoID", "state", "st_cd", "dist", "dist_cd"),
                                  by.y=c("state_district_shape23", "state_district_shape23_code", "state_shape23", "state_shape23_code", "district_shape23", "district_shape23_code"),
                                  all.x=TRUE)

rm(df_district, population)
#==============================================================================#
# 3.0 Write Data ####
#==============================================================================#
write_sf(district_shapefile, paste0("~/YSE Dropbox/Emily Goddard/India-IGUIDE/_data/shapefiles/district_shapefile.shp"))

#==============================================================================#
# END OF FILE ####
#==============================================================================#
