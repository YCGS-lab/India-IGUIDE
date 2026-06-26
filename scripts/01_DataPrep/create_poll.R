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
# 2. Clean Demographic Variables to Match Census ####
#==============================================================================#
#------------------------------------------------------------------------------#
## 2.1. Gender ####
#------------------------------------------------------------------------------#
ccim$demo_gender <- ""
ccim$demo_gender[ccim$gender=="Male"] <- "Male"
ccim$demo_gender[ccim$gender=="Female"] <- "Female"
table(ccim$gender, ccim$demo_gender)

#------------------------------------------------------------------------------#
## 2.2. Age Group ####
#------------------------------------------------------------------------------#
ccim$demo_age <- ""
ccim$demo_age[ccim$age<=29] <- "18-29"
ccim$demo_age[ccim$age>=30 & ccim$age<=44] <- "30-44"
ccim$demo_age[ccim$age>=45 &  ccim$age<=59] <- "45-59"
ccim$demo_age[ccim$age>=60] <- "60+"
ccim$demo_age[ccim$age==100] <- "60+"
ccim$demo_age[ccim$age=="Not asked"] <- ""
table(ccim$age, ccim$demo_age)

#------------------------------------------------------------------------------#
## 2.3. Caste ####
#------------------------------------------------------------------------------#
ccim$incl_in_caste <- 0
ccim$incl_in_caste[ccim$religion %in% c("Hindu","Sikh","Buddhist/Neo Buddhist")] <- 1

ccim$demo_caste <- "Other Castes"
ccim$demo_caste[ccim$incl_in_caste==1 & ccim$caste=="Refused"] <- ""
ccim$demo_caste[ccim$incl_in_caste==1 & ccim$caste=="Scheduled Caste"] <- "Scheduled Castes/Tribes"
ccim$demo_caste[ccim$incl_in_caste==1 & ccim$caste=="Scheduled Tribe"] <- "Scheduled Castes/Tribes"
table(ccim$caste, ccim$demo_caste, useNA = 'always')

#------------------------------------------------------------------------------#
## 2.4. Urban-Rural ####
#------------------------------------------------------------------------------#
ccim$demo_urban <- ""
ccim$demo_urban[ccim$urban=="Urban"] <- "Urban"
ccim$demo_urban[ccim$urban=="Semi-urban"] <- "Urban"
ccim$demo_urban[ccim$urban=="Rural"] <- "Rural"
table(ccim$urban, ccim$demo_urban)

#------------------------------------------------------------------------------#
## 2.5. Reorder Variables in Dataset ####
#------------------------------------------------------------------------------#
ccim <- ccim %>% 
  dplyr::select(caseid, state, district, demo_urban, demo_gender, 
                demo_age, demo_caste, everything())

#------------------------------------------------------------------------------#
## 2.6. Limit Dataset to Only the Observations that Have Full Demographics ####
#------------------------------------------------------------------------------#
ccim <- ccim %>% 
  dplyr::filter(demo_urban!="",
                demo_gender!="",
                demo_age!="",
                demo_caste!="")

#==============================================================================#
# 3.0 Subset Data ####
#==============================================================================#
poll <- ccim %>%
  dplyr::select(caseid, weight, wave, state, district, demo_age, demo_gender, 
                demo_caste, demo_urban, starts_with("n7")) %>%
  dplyr::rename(age = demo_age,
                gender = demo_gender,
                caste = demo_caste,
                urban = demo_urban) %>%
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
# 4.0 Recode Data ####
#==============================================================================#
cols <- colnames(poll)[grepl("^n7", colnames(poll))]
for(i in 1:length(cols)){
  col <- cols[i]
  poll$recode <- case_when(poll[,col] %in% c("Moderately worried", "Very worried") ~ 1, 
                           poll[,col] %in% c("Not at all worried", "Not very worried", 
                                             "Refused", "Don't know") ~ 0, 
                           .default = NA)
  poll$recode_oppose <- case_when(poll[,col] %in% c("Not at all worried", "Not very worried") ~ 1, 
                                  poll[,col] %in% c("Moderately worried", "Very worried", 
                                                    "Refused", "Don't know") ~ 0, 
                                  .default = NA)
  
  colnames(poll)[colnames(poll)=="recode"] <- paste0(col,"_recode")
  colnames(poll)[colnames(poll)=="recode_oppose"] <- paste0(col,"_oppose_recode")
}

poll <- poll %>%
  dplyr::select(caseid, weight, wave, state, district, state_district,
                age, gender, caste, urban, ends_with("_recode"))
#==============================================================================#
# 4.0 Merge Covariates ####
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
# 5.0 Write Data ####
#==============================================================================#
# Write data
write_rds(poll, paste0(dropbox, "poll/poll.rds"))
write_rds(poll_covars, paste0(dropbox, "poll/poll_covariates.rds"))

write.xlsx(poll, paste0(dropbox, "poll/poll.xlsx"))
write.xlsx(poll_covars, paste0(dropbox, "poll/poll_covariates.xlsx"))
#==============================================================================#
# END OF FILE ####
#==============================================================================#
