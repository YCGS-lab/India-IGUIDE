# Prep for India survey data for mapping
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

df <- read_csv("~/Documents/GitHub/India-IGUIDE/datafiles/all_in.csv")

# Deduplicate based on caseid, keeping first occurrence
df_dedup <- df[!duplicated(df$caseid), ]
rm(df)

write_csv(df_dedup, "~/Documents/GitHub/India-IGUIDE/datafiles/all_in_dedup.csv")

# Calculate Weights

df <- df_dedup %>% select(caseid, weight, state, n7dy23_recode, n7fy23_recode)

df_subset <- df %>%
  group_by(state) %>%
  filter(n() >= 300) %>%
  ungroup()

df_drought_pct_wtd <- df_subset %>%
  group_by(state) %>%
  summarise(
    weighted_percent = 100 * sum(n7dy23_recode * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
  )

df_drought_pct_wtd
df_drought_pct_wtd$weighted_percent <- round(df_drought_pct_wtd$weighted_percent, 2)
write_csv(df_drought_pct_wtd, "~/Documents/GitHub/India-IGUIDE/datafiles/drought_survey_pct_wtd.csv")


df_flood_pct_wtd <- df_subset %>%
  group_by(state) %>%
  summarise(
    weighted_percent = 100 * sum(n7fy23_recode * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
  )

df_flood_pct_wtd
df_flood_pct_wtd$weighted_percent <- round(df_flood_pct_wtd$weighted_percent, 2)
write_csv(df_flood_pct_wtd, "~/Documents/GitHub/India-IGUIDE/datafiles/flood_survey_pct_wtd.csv")

