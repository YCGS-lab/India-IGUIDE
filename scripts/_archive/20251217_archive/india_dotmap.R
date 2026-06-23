# IGUIDE data prep for India hazard worry modeling
# JRM, 08032025
# Crosswalk survey to geographic data to enable mapping

#==============================================================================#
# 0. Set R Environment ####
#==============================================================================#

rm(list=ls())

library(tidyverse)
library(readr)
library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(ggforce)  # for smoother jittering

#==============================================================================#
# 1. Create geographic key in the survey output ####
#==============================================================================#

df <- read_csv(file="~/Documents/GitHub/ypccc_india/india_w1/iguide/datafiles/poll_df/full_data.csv")
df$state_district_survey <- paste0(df$state, ", ", df$district)
# df$state_survey <- df$state
# df$district_survey <- df$district

#==============================================================================#
# 2. Xwalk for mapping ####
#==============================================================================#

xwalk <- read_excel("~/Documents/GitHub/ypccc_india/india_w1/iguide/xwalk_survey_shapefile.xlsx")
  
# Load India shapefiles
shape_country   <- read_sf(dsn="~/Documents/GitHub/ypccc_india/india_w1/iguide/shapefiles/national/", layer="nation")
shape_state <- read_sf(dsn="~/Documents/GitHub/ypccc_india/india_w1/iguide/shapefiles/state/", layer="state_shapefile_2023")
shape_district <- read_sf(dsn="~/Documents/GitHub/ypccc_india/india_w1/iguide/shapefiles/district/", layer="district_state_shapefile_2023")

# Harmonize naming convention
# xwalk <- xwalk %>%
#   mutate(state_district_survey = paste0(state, ", ", district))

# Join survey data with xwalk
df_joined <- df %>%
  left_join(xwalk, by = "state_district_survey")

# # Join with district shapefile
# shape_district <- shape_district %>%
#   mutate(district_id = as.character(district_id))  # adjust as needed
# 
# shape_district_df <- shape_district %>%
#   left_join(df_joined %>% count(district_id), by = "district_id")  # n = respondents per district
# 
# #==============================================================================#
# # 3. Generate one point per respondent, jittered in district ####
# #==============================================================================#
# 
# # Get centroids of each district
# district_centroids <- shape_district %>%
#   st_centroid(of_largest_polygon = TRUE) %>%
#   select(district_id, geometry)
# 
# # Merge district centroid with survey data
# df_points <- df_joined %>%
#   left_join(district_centroids, by = "district_id") %>%
#   st_as_sf(coords = c("geometry"), crs = st_crs(shape_district)) %>%
#   select(-geometry) %>%
#   bind_cols(st_coordinates(district_centroids[df_joined$district_id, ])) %>%
#   rename(x = X, y = Y)
# 
# # Apply jitter to simulate random location within the district area
# set.seed(123)  # for reproducibility
# df_points <- df_points %>%
#   mutate(x_jit = jitter(x, amount = 0.2),
#          y_jit = jitter(y, amount = 0.2))
# 
# #==============================================================================#
# # 4. Plot map ####
# #==============================================================================#
# 
# ggplot() +
#   geom_sf(data = shape_district, fill = "white", color = "gray60", size = 0.2) +
#   geom_point(data = df_points, aes(x = x_jit, y = y_jit),
#              color = "black", size = 0.3, alpha = 0.5) +
#   coord_sf(crs = st_crs(shape_district)) +
#   theme_minimal() +
#   labs(title = "Survey Respondents in India (Jittered by District)",
#        caption = "Each dot represents one survey respondent")
# 
# 
