# IGUIDE data prep for India hazard worry modeling
# JRM, 08032025
# Join recoded hazards to geographic crosswalk to enable mapping

#==============================================================================#
# 0. Set R Environment ####
#==============================================================================#

rm(list=ls())

library(lme4)
library(tidyverse)
library(arm)
library(lattice)
library(merTools)
library(grid)
library(readxl)

# parameters
model <- "final/temp_vulnerability_education" #"null" #"vulnerability_education" #"attribution"
myname <- "Jennifer Marlon"

# paths
github <- "~/Documents/GitHub/ypccc_india/india_w1/"
dropbox <- paste0("~/Dropbox (YSE)/",myname,"ypcccdb/downscale/india/")
outputpath <- paste0(dropbox,"output/",model,"/maps/")
temp <- paste0(github,model,"/") #paste0(github,"temp_",model,"/")
xwalks <-paste0(temp,"xwalks/")
setwd(github)

#==============================================================================#
# 1. Load ####
#==============================================================================#

load(file=paste0(temp,"df_state_merge.rda"))
load(file=paste0(temp,"df_district_merge.rda"))

## DO ONCE: clean up N.x and N.y and convert to just "N"
# df_district$N <- df_district$N.y 
# df_district$N.x <- NULL
# df_district$N.y <- NULL
# df_district <- df_district %>% dplyr::select(state,district,state_dist,GEOID,                       
#                               state_dist_code,state_code,district_code,display_name,
#                              state_shape23,state_shape23_code,state_census11,district_census11,
#                                       state_district_census11,state_census11_code,district_census11_code,district_shape23,
#                                       district_shape23_code,state_district_shape23,zone,country,                   
#                                       urban,caste,gender,age,n,N,n_pct_geo,highersecondaryabove_district,vulnerability_district)
# save(df_district, file=paste0(temp,"df_district_merge.rda"))

#==============================================================================#
# 2. Xwalk for mapping ####
#==============================================================================#

