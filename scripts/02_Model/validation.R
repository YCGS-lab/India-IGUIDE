#==============================================================================#
# validation.R ####
#==============================================================================#
### Validate MRP results against survey weighted averages

# INPUTS:
# Crosswalk files
# Poll file
# MRP Results

# OUTPUTS:
# Mean absolute errors
# Scatter plot

# Last updated 05/14/2026 EG
#==============================================================================#
# 0.0 Setup ####
#==============================================================================#
rm(list=ls())
library(tidyverse)
library(stringr)

# Load File Paths
dropbox <- "~/YSE Dropbox/Emily Goddard/India-IGUIDE/"
outdir <- paste0(dropbox, "output_flood/")
input <- paste0(dropbox, "_data/")
xwalk_path <- paste0(input,"xwalks/")

#==============================================================================#
# 1.0 Set parameters ####
#==============================================================================#
# Geographic column names
geo_cols <- c("state_district_survey", "state", "country")

# Geography namess
geo_names <- c("district", "state", "country")

# Set population cutoffs
pops <- c(50, 500, 500)

# Set plot colors
colors <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", 
            "gold1", "skyblue2", "#FB9A99", "palegreen2", "#CAB2D6", "#FDBF6F", 
            "gray70", "khaki2", "maroon", "orchid1", "deeppink1", "blue1", "steelblue4", 
            "darkturquoise", "green1", "yellow4", "yellow3", "darkorange4", "brown",
            "orange2", "purple4", "lightblue", "red1", "yellow")

#==============================================================================#
# 2.0 Load Files ####
#==============================================================================#
# Poll file
poll <- read.csv(paste0(input,"/poll_flood_updated_03052026.csv"))
poll2 <- read.csv(paste0(input,"/_archive/RAP_Flood_merged_all_updated_20260227.csv"))

# Load MRP results
areapred <- readRDS(paste0(outdir,"final/flood_district_mapping.rds"))

# Crosswalk file
xwalk <- read.csv(paste0(xwalk_path,"xwalk_full.csv"))

# Check Poll Data
ccim <- readRDS("~/YSE Dropbox/Emily Goddard/ypcccdb/_data/surveys/india/cvoter2026/output/combined_full_india_cvoter_w01-w05_2026.rds")
ccim$n7fy23_recode <- case_when(ccim$n7fy23_flood_worry %in% c("Moderately worried", "Very worried") ~ 1,
                                ccim$n7fy23_flood_worry %in% c("Not at all worried", "Not very worried",
                                                               "Refused", "Don't know") ~ 0,
                                .default = NA)

cols <- colnames(ccim)[colnames(ccim) %in% colnames(poll)]
ccim <- ccim %>%
  dplyr::select(all_of(cols), urban_rural) %>%
  dplyr::rename(urban = urban_rural) %>%
  dplyr::filter(wave=="tapp") %>%
  dplyr::filter(!is.na(n7fy23_recode))

# ccim$all <- paste0(ccim$state, ccim$district, ccim$age, ccim$gender, ccim$caste, ccim$urban, ccim$n7fy23_recode)
# poll$all <- paste0(poll$state, poll$district, poll$age, poll$gender, poll$caste, poll$urban, poll$n7fy23_recode)

poll <- poll[, colnames(poll) %in% colnames(ccim)]

# Check Another poll data
# poll_feb <- read.csv("~/GitHub/ypccc_india/india_w1/iguide/datafiles/Poll_Severe_floods.csv")
# poll_final <- read.csv("~/GitHub/ypccc_india/india_w1/iguide/datafiles/final_data_for_modelling.csv")
# length(poll_final$caseid[!duplicated(poll_final$caseid)])

#==============================================================================#
# 3.0 Calculate Margin of Error ####
#==============================================================================#
# Calculate Percent by level
areapred$country_per <- areapred$national_per
areapred$district_per <- areapred$pred_per
areapred_state <-  areapred %>%
  dplyr::group_by(state) %>%
  dplyr::summarise(state_n = sum(n, na.rm=TRUE), 
                   state_cellpred_n = sum(cellpred_n, na.rm=TRUE)) %>%
  dplyr::mutate(state_per = (state_cellpred_n / state_n)*100) %>%
  dplyr::select(state, state_per)
areapred <- base::merge(areapred, areapred_state, by="state", all.x=TRUE)
rm(areapred_state)

# Merge MRP results with crosswalk
areapred <- base::merge(areapred, xwalk, by=c("state_shape23", "state_shape23_code", "state_census11", "district_census11", 
                                              "state_district_census11", "state_census11_code", "district_census11_code", 
                                              "district_shape23", "district_shape23_code", "state_district_shape23", 
                                              "zone"), all.x=TRUE)

# Create Country Column
poll$country <- "India"

# Loop over each geography level
qa_list <- list()
for(i in 1:length(geo_cols)){
  geo_col <- geo_cols[i]
  geo_name <- geo_names[i]
  pct_name <- paste0(geo_name,"_per")

  # Calculate the share of each response for each question
  df1 <- poll %>%
    dplyr::select(caseid, !!sym(geo_col), weight, n7fy23_recode, wave) %>%
    dplyr::filter(!is.na(n7fy23_recode),
                  !is.na(!!sym(geo_col))) %>%
    dplyr::distinct()
  
  df1 <- df1 %>% 
    dplyr::mutate(count = 1) %>% 
    dplyr::group_by(!!sym(geo_col), n7fy23_recode) %>%
    dplyr::summarise(freq = sum(weight), n_obs = sum(count)) %>%
    dplyr::mutate(n_wgt = sum(freq),
                  prop  = freq/n_wgt,
                  pct   = prop*100,
                  n_obs = sum(n_obs)) %>% 
    dplyr::filter(n_obs>pops[i], 
                  n7fy23_recode==1) %>%
    dplyr::ungroup() %>% 
    dplyr::select(!!sym(geo_col), n_obs, freq, n_wgt, prop, pct)
  
  # Calculate averages by geography
  areapred_geo <- areapred %>%
    dplyr::select(!!sym(geo_col), !!sym(pct_name)) %>%
    dplyr::rename(pred_per = !!sym(pct_name)) %>%
    dplyr::distinct()
  
  # Merge MRP estimates and survey weighted averages
  qa_list[[i]] <- base::merge(areapred_geo, df1, by=geo_col, all.y=TRUE)
  qa_list[[i]] <- na.omit(qa_list[[i]])
  
  # Calculate the difference between survey weighted averages and MRP estimates
  qa_list[[i]] <- qa_list[[i]] %>%
    dplyr::mutate(difference = round(pct - pred_per, 2), 
                  survey_pct = round(pct,2),
                  estimate = round(pred_per,2),
                  survey_pop = n_obs,
                  abs_difference = round(abs(difference),2)) %>%
    dplyr::select(!!sym(geo_col), survey_pop, survey_pct, estimate, difference, abs_difference) %>%
    dplyr::distinct()
  
  # Calculate the Mean Absolute Error
  vect <- data.frame(matrix(nrow=1,ncol=ncol(qa_list[[i]])))
  colnames(vect) <- colnames(qa_list[[i]])
  vect[,colnames(vect)=="abs_difference"] <- round(mean(qa_list[[i]][,colnames(qa_list[[i]])=="abs_difference"],na.rm=TRUE),2)
  vect[,geo_col] <- "Mean Absolute Error"
  qa_list[[i]] <- rbind(qa_list[[i]], vect)
}
names(qa_list) <- geo_names

#==============================================================================#
# 4.0 Create Validation Scatter Plot ####
#==============================================================================#
# Set Plot parameters
geo <- "state_district_survey"
namecol <- "district"
df <- qa_list[[1]][qa_list[[1]]$state_district_survey!="Mean Absolute Error",]
title <- paste0("Flood Worry Model Validation")

# Create Plot
plot <- ggplot(df, aes(x=estimate, y=survey_pct)) +
  geom_abline(intercept = 0, slope = 1, color="#242424") +
  geom_point(aes(color = state_district_survey, size=0.3)) +
  scale_color_discrete(palette=colors) +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100,10), labels = seq(0,100,10))+#breaks = seq(0,max(data$pred_per_9, na.rm = TRUE),10)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10), labels = seq(0,100,10))+#breaks = seq(0,max(data$pred_per_survey, na.rm = TRUE),10)) +
  labs(title = title,
       x = paste0("Model Estimate"),
       y = "Weighted Survey Average") +
  scale_size(guide = 'none') +
  guides(color=guide_legend(title="District", ncol = 1)) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "left",
        legend.key.size = unit(0.3, 'cm'),
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.title = element_text(size=10),
        legend.text = element_text(size=6))  
#==============================================================================#
# 5.0 Write files ####
#==============================================================================#
# Create directory
if(dir.exists(paste0(outdir,"QA/"))==FALSE){
  dir.create(paste0(outdir,"QA/"), recursive=TRUE)
}

# Write QA files
write.csv(qa_list[[1]], file=paste0(outdir,"QA/qa_district.csv"), row.names = FALSE)
write.csv(qa_list[[2]], file=paste0(outdir,"QA/qa_state.csv"), row.names = FALSE)
write.csv(qa_list[[3]], file=paste0(outdir,"QA/qa_country.csv"), row.names = FALSE)

# Write scatterplot
ggsave(paste0(outdir,"QA/validation_plot.png"), plot, width = 6, height = 4)

#==============================================================================#
# END OF FILE
#==============================================================================#
