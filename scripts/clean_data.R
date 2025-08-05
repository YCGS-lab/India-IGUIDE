#==============================================================================#
# iGuide Workshop ###

# INPUTS:
# Cleaned Survey Data

# OUTPUTS:
# Data for iGuide Workshop

# Last modified: June 26, 2025, EG
#==============================================================================#
# 0. Set R Environment ####
#==============================================================================#
rm(list=ls())

library(tidyverse)
library(readxl)
library(openxlsx)

# Set parameters
year <- "2025" #"2023"
wave <- "04" #"01"
surveyname <- "cvoter" #"tapp"
metas <- c("caseid","weight","wave","state","district","state_district") #,"language","segment"
demos <- c("gender","age","caste","urban")

# Set file paths
myname <- "Jennifer Marlon" #Emily Goddard
github <- "~/Documents/GitHub/ypccc_india/india_w1/" # add Documents
temp <- "~/GitHub/ypccc_india/india_w1/final/temp_vulnerability_education/"
dropbox <- "~/Dropbox (YSE)/ypcccdb/_data/external/india/"
survey <- paste0("~/Dropbox (YSE)/ypcccdb/_data/surveys/india/",surveyname,year,"/output/")
writepath <- paste0(github,"iguide/datafiles/")
setwd(github)

#==============================================================================#
# 1.0 Load Data ####
#==============================================================================#
# Load Survey Data
data <- readRDS(paste0(survey,"combined_full_india_",surveyname,"_w01-w",wave,"_",year,".rds"))

# Load Parameter File
param <- read_excel(paste0("input/parameters_india_",year,".xlsx"))
param2 <- read.xlsx(paste0("~/YSE Dropbox/",myname,"/ypcccdb/downscale/india/output/attribution/webtools/webtool_parameters_2025.xlsx"))

# Scaled Covariates
load(paste0(temp,"covariates_state.Rda"))
load(paste0(temp,"covariates_district.Rda"))
load(paste0(temp,"covariates_scaled_state.Rda"))
load(paste0(temp,"covariates_scaled_district.Rda"))

#==============================================================================#
# 2. Clean Demographic Variables to Match Census ####
#==============================================================================#
data[data == "Not asked"] <- NA
data <- data %>%
  dplyr::filter(wave == "2024")
data <- data[,colSums(is.na(data))<nrow(data)]

#------------------------------------------------------------------------------#
# 2..0 Clean Parameter Files ####
#------------------------------------------------------------------------------#
param <- param %>%
  dplyr::filter(iGuide=="yes") %>%
  dplyr::select(qid, varname, label, levels, recode_values, year, n_wave)
param2 <- param2 %>%
  dplyr::select(questionOrder, qid, shortTitle, diffTitle, longTitle, qtext, qCategory)
param <- base::merge(param, param2, by="qid",al.x=TRUE)
qlist <- unique(param$varname)
qlist2 <- unique(param$varname[grepl("^n7",param$varname)])

#------------------------------------------------------------------------------#
## 2.1. Gender ####
#------------------------------------------------------------------------------#
data$demo_gender <- ""
data$demo_gender[data$gender=="Male"] <- "Male"
data$demo_gender[data$gender=="Female"] <- "Female"
table(data$demo_gender, data$gender)

#------------------------------------------------------------------------------#
## 2.2. Age Group ####
#------------------------------------------------------------------------------#
data$demo_age <- ""
data$demo_age[data$age<=29] <- "18-29"
data$demo_age[data$age>=30 & data$age<=44] <- "30-44"
data$demo_age[data$age>=45] <- "45+"
data$demo_age[data$age==100] <- "45+"
table(data$age, data$demo_age)

#------------------------------------------------------------------------------#
## 2.3. Caste ####
#------------------------------------------------------------------------------#
data$incl_in_caste <- 0
data$incl_in_caste[data$religion %in% c("Hindu","Sikh","Buddhist/Neo Buddhist")] <- 1

data$demo_caste <- "Other Castes"
data$demo_caste[data$incl_in_caste==1 & data$caste=="Refused"] <- ""
data$demo_caste[data$incl_in_caste==1 & data$caste=="Scheduled Caste"] <- "Scheduled Castes/Tribes"
data$demo_caste[data$incl_in_caste==1 & data$caste=="Scheduled Tribe"] <- "Scheduled Castes/Tribes"
table(data$caste, data$demo_caste, useNA = 'always')

#------------------------------------------------------------------------------#
## 2.4. Urban-Rural ####
#------------------------------------------------------------------------------#
data$demo_urban <- ""
data$demo_urban[data$urban=="Urban"] <- "Urban"
data$demo_urban[data$urban=="Semi-urban"] <- "Urban"
data$demo_urban[data$urban=="Rural"] <- "Rural"
table(data$urban, data$demo_urban)

#------------------------------------------------------------------------------#
## 2.5. Reorder Variables in Dataset ####
#------------------------------------------------------------------------------#
data <- data %>% 
  dplyr::select(any_of(metas), all_of(qlist), 
                demo_urban, demo_gender, demo_age, demo_caste)

#------------------------------------------------------------------------------#
## 2.6. Limit Dataset to Only the Observations that Have Full Demographics ####
#------------------------------------------------------------------------------#
data <- data %>% 
  dplyr::filter(demo_urban!="",
                demo_gender!="",
                demo_age!="",
                demo_caste!="")

#==============================================================================#
# 3. Recode all DVs ####
#==============================================================================#
qa_list <- list()
index <- 0
# TEMP
param$recode_values <- gsub("Moderately worried, ", "", param$recode_values)

for(i in 1:length(qlist)){
  col <- qlist[i]
  
  # Identify Recode Values
  for(n in 1:length(param$varname[param$varname==col])){
    index <- index+1
    var <- param$label[param$varname==col][n]
    
    # Identify levels
    levels_param <- param$levels[param$label==var]
    levels_data <- levels(data[,col])
    stopifnot("Param levels and data levels don't match"=identical(paste0(levels_data, collapse = ", "),levels_param))
    
    # Identify the recode values
    recode_vals <- strsplit(param$recode_values[param$label==var],", ")[[1]]
    stopifnot("Missing recode values"=!is.na(recode_vals))
    
    # Identify the variables
    oldvar <- paste0(col)
    newvar <- paste0(var,"_recode")
    data[,newvar] <- 0
    
    # Recode
    data[,newvar][data[,oldvar] %in% recode_vals] <- 1
    data[,newvar][is.na(data[,oldvar])] <- NA
    
    # Manual QA
    # Create a list of tables of the new column by the old column to manually check later
    qa_list[[index]] <- table(data[,oldvar],data[,newvar], useNA='always')
    names(qa_list)[[index]] <- var
  }
}
rm(col, var, newvar, oldvar, i, n, index)
#==============================================================================#
# 4. Convert Character Variables to Factor Variables ####
#==============================================================================#
data$demo_urban <- as.factor(data$demo_urban)
data$demo_gender <- as.factor(data$demo_gender) 
data$demo_age <- as.factor(data$demo_age)
data$demo_caste <- as.factor(data$demo_caste)

# Remove "demo"
names(data) <- gsub("demo_","",names(data))

#==============================================================================#
# 5. Subset Data ####
#==============================================================================#
df_list <- list()
for(i in 1:length(qlist2)){
  var <- qlist2[i]
  
  df_list[[i]] <- data %>%
    dplyr::mutate(state_district = paste0(state,", ",district)) %>%
    dplyr::select(all_of(c(metas,demos,var,paste0(var,"_recode")))) %>%
    dplyr::filter(!is.na(!!sym(var))) 
}
names(df_list) <- unique(param$shortTitle[grepl("^n7",param$varname)])

#==============================================================================#
# 5. Write Data ####
#==============================================================================#
write.csv(data, file=paste0(writepath,"full_data.csv"), row.names = FALSE)
saveRDS(data, file=paste0(writepath,"full_data.rds"))
  
for(i in 1:length(df_list)){
  name <- gsub(" ","_",names(df_list)[i])
  assign(name, df_list[[i]])
  
  write.csv(df_list[[i]], file=paste0(writepath,name,".csv"), row.names = FALSE)
  saveRDS(df_list[[i]], file=paste0(writepath,name,".rds"))
}


#==============================================================================#
# 6. Reduce to Four Key Hazards and Write Four ####
#==============================================================================#
ig <- readRDS(paste0(writepath,"full_data.rds"))

# Four key dependent variables for hazard worry modeling 
# n7dy23 = droughts and water shortages
# n7ey23 = heat waves
# n7fy23 = floods
# n7gy23 = air pollution

ig <- ig %>% dplyr::select(caseid,weight,wave,state,district,urban,gender,age,caste,state,district,
                           n7dy23_recode,n7ey23_recode,n7fy23_recode,n7gy23_recode)

write.csv(poll, file = "iguide/datafiles/full_extract_recoded.csv", row.names=F)


