#==============================================================================#
# Explore India Health Survey Data from 2021 ###

# INPUTS:
# India Health Survey Data (SAV) contains 1274250 obs. of 1644 variables

# Full NFHS Metadata are available in the NFHS2019-2021-survey_report.PDF.
# NFHS-5 provides information for 707 districts, 28 states, and 8 union territories.
# Four survey schedules/questionnaires—Household, Woman, Man, and Biomarker—were 
# canvassed in 18 local languages using Computer Assisted Personal Interviewing (CAPI).
# Basic demographic information was collected on the characteristics of each person listed, 
# such as age, sex, marital status, schooling, ownership of an Aadhaar card, tobacco use, 
# alcohol consumption, disabilities, and relationship to the head of the household.

# OUTPUTS:
# Survey dataset (Rda)


# Last modified: Aug 15, 2025, JRM
#==============================================================================#
# 0. Set R Environment ####
#==============================================================================#
rm(list=ls())

# Libraries
library(foreign)
library(dplyr)
library(tidyverse)
library(readxl)
library(plyr)
library(haven)
#library(janitor)
library(stringr)
#library(crosswalkr)
#library(matrixStats)
#library(labelled)
library(openxlsx)
#library(sjlabelled)
#library(expss)


#Turning off scientific notation
options(scipen=999)


# Set Directories
myname <- "Jennifer Marlon" # for Dropbox users
directory <- paste0("~/YSE Dropbox/",myname,"/ypcccdb/_data/external/india/nfhs2021")
github  <- "~/GitHub/India-IGUIDE/"
setwd(directory)

#==============================================================================#
# 1. Load Raw Datasets ####
#==============================================================================#

infile <- paste0(directory, "/IABR7ESV/IABR7EFL.SAV")
df1 <- read_sav(infile, encoding = "utf-8")

infile <- paste0(directory, "/IACR7ESV/IACR7EFL.SAV")
df2 <- read_sav(infile, encoding = "utf-8")


#==============================================================================#
# 2. Explore IABR7ESV ####
#==============================================================================#

names(df1[1:100])
unique(df1$CASEID) # 494,019
table(df1$V013) # age in 5-year groups (7 groups total, 1-7)
table(df1$V024) # state1 (37 groups, 1-37)
table(df1$V101) # state2 - appears identixal to state1
table(df1$V025) # type of place of residence [rural/urban]  273,927; 1,000,323 
table(df1$V151) # Sex of Head of Household (HH) 1,074,529; 199,711 
table(df1$V045C) # Native language in 22 groups with most in last group 96

df1.demos <- df1 %>% select(CASEID, V013, V024, V025, V101, V151, V045C)

#==============================================================================#
# 3. Explore IACR7ESV --> demos appear identical to IABR7ESV ####
#==============================================================================#

names(df2[1:100])
table(df1$V013) # age in 5-year groups (7 groups total, 1-7)
table(df1$V024) # state1 (37 groups, 1-37)
table(df1$V101) # state2 - appears identixal to state1
table(df1$V025) # type of place of residence [rural/urban]  273,927; 1,000,323 
table(df1$V151) # Sex of Head of Household (HH) 1,074,529; 199,711 
table(df1$V045C) # Native language in 22 groups with most in last group 96

#==============================================================================#
# 4. Write Compressed Datasets ####
#==============================================================================#

saveRDS(df1.demos, file=paste0(directory, "/IABR7EFL.demos.rds"))
write_csv(df1.demos, file=paste0(directory, "/IABR7EFL.demos.csv"))



