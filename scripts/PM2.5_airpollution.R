# Merge the India PM2.5 data and map it
# Air Pollution Data from India, 2019, aggregated to 2011 census districts
# - Minimum, Maximum, Annual total
# - Compiled from Siddhartha Mandal: siddhartha@ccdcindia.org
# Input file: dist_11_mean.csv

#------------------------------------------------------------------------------#
# 0. Set up R environment
#------------------------------------------------------------------------------#

# DO ONCE
install.packages("here")
library(here)
library(sf)
library(tidyverse)
library(readxl)
library(doBy)
library(corrplot)
library(ggplot2)
library(ggpubr)
library(ggtext)
library(scales)
library(viridis)


year <- "2023"

#dropbox <- "~/Dropbox (YSE)/ypcccdb/_data/external/india/"
gitdata <- "~/Documents/GitHub/India-IGUIDE/datafiles/new_covariates/airpollution/"
github <- "~/Documents/GitHub/India-IGUIDE/"
#temp <- paste0(github,"temp_vulnerability_education/")
#xwalks <- paste0(temp,"xwalks/")
#output <- "~/YSE Dropbox/Emily Goddard/ypcccdb/downscale/india/output"
#survey <- "~/Dropbox (YSE)/ypcccdb/_data/surveys/india/"

#------------------------------------------------------------------------------#
# 1. Load Files
#------------------------------------------------------------------------------#
# Load Air Pollution Data
air <- read.csv(paste0(gitdata,"/dist_11_mean.csv"))

# Census to Survey
xwalk_survey_apt <- read_excel(paste0(github,"xwalks/xwalk_survey_apt.xlsx"))
xwalk_state <- read_excel(paste0(github, "xwalks/xwalk_state.xlsx"))
xwalk_district <- read_excel(paste0(github, "xwalks/xwalk_district.xlsx"))
xwalk_survey <- read_excel(paste0(github, "xwalks/xwalk_survey.xlsx"))

# Load Shapefile
shape_state <- read_sf(dsn=paste0(github, "shapefiles/state"), layer=paste0("state_shapefile_2023"))
shape_district <- read_sf(dsn=paste0(github, "shapefiles/district"), layer=paste0("district_state_shapefile_2023"))

# Load Survey Data
#load(paste0(github,"datafiles/poll_df/full_data.rds"))
india_df <- read_csv(file=paste0(github, "datafiles/poll_df/full_extract.csv"))
poll <- india_df
rm(india_df)

#------------------------------------------------------------------------------#
# 2. Clean Air Pollution
#------------------------------------------------------------------------------#
# Fix states
air$STATE_UT[air$STATE_UT=="Delhi"] <- "NCT of Delhi"
air$STATE_UT[air$STATE_UT=="Tamilnadu"] <- "Tamil Nadu"

# Fix Telangana
air$STATE_UT[air$NAME %in% xwalk_survey_apt$district[xwalk_survey_apt$state=="Telangana"]] <- "Telangana"

# Fix Districts
air$NAME[air$NAME=="Barabanki"] <- "Bara Banki"
air$NAME[air$NAME=="Y.S.R"] <- "Y.S.R."
air$NAME[air$NAME=="East Nimar"] <- "Khandwa (East Nimar)"
air$NAME[air$NAME=="West Nimar"] <- "Khargone (West Nimar)"
air$NAME[air$NAME=="KolKata"] <- "Kolkata"
air$NAME[air$NAME=="Pakaur"] <- "Pakur"
air$NAME[air$NAME=="Marigaon"] <- "Morigaon"
air$NAME[air$NAME=="Sibsagar"] <- "Sivasagar"
air$NAME[air$NAME=="Ri Bhoi"] <- "Ribhoi"
air$NAME[air$NAME=="Balemu East Kameng"] <- "East Kameng"

# Fix column names
air_dist <- air %>%
  dplyr::rename(state = STATE_UT,
                district = NAME,
                pm2.5_district = PM2.5_mean_2019) %>%
  dplyr::select(state, district, pm2.5_district) %>%
  na.omit()

air_state <- air_dist %>%
  dplyr::group_by(state) %>%
  dplyr::summarise(pm2.5_state = mean(pm2.5_district)) %>%
  dplyr::select(state, pm2.5_state) %>%
  na.omit()

rm(air)

#------------------------------------------------------------------------------#
# 3.0 Reformat Shapefiles
#------------------------------------------------------------------------------#
# State
shape_state <- shape_state %>%
  dplyr::rename(ShapeName = GeoName,
                ShapeID = GEOID) %>%
  dplyr::select(ShapeName, ShapeID, geometry)

# District
shape_district <- shape_district %>%
  dplyr::rename(ShapeName = GeoName,
                ShapeID = GEOID) %>%
  dplyr::select(ShapeName, ShapeID, geometry)

#------------------------------------------------------------------------------#
# 4. Merge Poll with Crosswalk ####
#------------------------------------------------------------------------------#
intersect(names(xwalk_survey), names(poll))

# Merge with Xwalk
poll <- base::merge(xwalk_survey, poll, by.x=c("state_survey","district_survey"),
                    by.y=c("state","district"),all.y=TRUE)

# Remove missing data from poll (Rows where geo information is missing)
poll <- na.omit(poll) # Removes 1 observation

# Clean up
#View(poll)
# poll$state_dist <- poll$state_dist.x
# poll$state_dist.x <- NULL
# poll$state_dist.y <- NULL
# 
# poll$state_survey <- poll$state_survey.x
# poll$state_survey.x <- NULL
# poll$state_survey.y <- NULL

poll$state_district_survey <- poll$state_district_survey.x
poll$state_district_survey.x <- NULL
poll$state_district_survey.y <- NULL

# poll$district_survey <- poll$district_survey.x
# poll$district_survey.x <- NULL
# poll$district_survey.y <- NULL
# 


#------------------------------------------------------------------------------#
# 5. Merge with Crosswalk ####
#------------------------------------------------------------------------------#
# District Weights
# air_dist <- base::merge(xwalk_district, air_dist, by=c("state","district"),
#                            all.y=TRUE)
# air_dist <- base::merge(xwalk_survey, air_dist, by=c("zone", "country", "state","state_code", "district","district_code","state_dist","state_dist_code"),
#                         all.y=TRUE)
air_dist <- left_join(
  air_dist,
  xwalk_district %>% dplyr::select(state, district, state_code, district_code, state_dist, state_dist_code),
  by = c("state", "district")
)

air_dist <- air_dist %>%
  dplyr::select(state_dist, state_dist_code, state, state_code, district, 
                district_code, everything()) %>%
  dplyr::select(-ends_with("census11")) %>%
  dplyr::distinct()

# State Weights
air_state <- left_join(
  air_state, xwalk_state,
    by=c("state")
  )

intersect(names(xwalk_survey), names(air_state))
air_state <- left_join(
  xwalk_survey, air_state, by=c("country", "state","state_code"))

air_state <- air_state %>%
  dplyr::select(state, state_code, everything()) %>%
  dplyr::select(-ends_with("census11")) %>%
  dplyr::distinct()

#------------------------------------------------------------------------------#
# 6. Merge with Poll ####
#------------------------------------------------------------------------------#
# Merge with Poll
intersect(names(poll), names(air_dist))
intersect(names(poll), names(air_state))
poll <- left_join(poll, air_dist, by=c("state", "state_code", "district", "district_code", "state_dist", 
                                       "state_dist_code"))
poll <- left_join(poll, air_state, by=c("state_survey", "district_survey", "zone", "country", "state", 
                                        "state_code", "district", "district_code", "state_dist", "state_dist_code", 
                                        "state_district_survey"))


summaryBy(pm2.5_district ~ n7gy23, poll, FUN = mean)
summaryBy(pm2.5_state ~ n7gy23, poll, FUN = mean)

corpoll <- poll %>%
  dplyr::select(n7gy23, pm2.5_district, pm2.5_state)
corpoll <- na.omit(corpoll)

corpoll$n7gy23 <- case_when(
  corpoll$n7gy23 == "Not at all worried" ~ 1,
  corpoll$n7gy23 == "Not very worried" ~ 2,
  corpoll$n7gy23 == "Moderately worried" ~ 3,
  corpoll$n7gy23 == "Very worried" ~ 4,
  corpoll$n7gy23 == "Refused" ~ 5,
  corpoll$n7gy23 == "Don't know" ~ 6,
  corpoll$n7gy23 == "Not asked" ~ 7,
  .default = NA
)

corrplot(cor(corpoll))

#------------------------------------------------------------------------------#
# 7a. Write output #### 
#------------------------------------------------------------------------------#

poll_subset <- poll %>% dplyr::select(caseid,pm2.5_state,pm2.5_district) %>% arrange(caseid)
write.csv(poll_subset, file = "India-IGUIDE/datafiles/poll_df/full_airpollution.csv", row.names=F)

#------------------------------------------------------------------------------#
# 7b. Map #### --> NOT WORKING
#------------------------------------------------------------------------------#
# Load colors
#source("India-IGUIDE/scripts/util/plot_colors.R", local = TRUE)
# source(paste0(github,"scripts/util/mapping.R"))
# 
# map.ypccc(air_dist, shape_district, "pm2.5_district", c("state_district_shape23"), 
#           c("ShapeName"), year="2019", outputpaths=output, legendlab="Average PM2.5", 
#           ptitle="Average PM2.5 in India", psubtitle="District, 2019", 
#           source="Siddhartha Mandal")
# 
# map.ypccc(air_state, shape_state, "pm2.5_state", c("state_shape23"), 
#           c("ShapeName"), year="2019", outputpaths=output, legendlab="Average PM2.5", 
#           ptitle="Average PM2.5 in India", psubtitle="State, 2019", 
#           source="Siddhartha Mandal")

#------------------------------------------------------------------------------#
# END OF FILE ####
#------------------------------------------------------------------------------#
