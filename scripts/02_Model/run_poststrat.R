#==============================================================================#
# run_poststrat.R ####
#==============================================================================#
### Run multilevel regression with post-stratification (MRP) 

# INPUTS:
# Crosswalk file
# Model file
# Covariate dataframe
# Spatial covariates dataframe
# Census counts

# OUTPUTS:
# MRP Results

# Last updated 05/14/2026 EG
#==============================================================================#
# 0.0 Setup ####
#==============================================================================#
rm(list=ls())
library(openxlsx)
library(tidyverse)
library(stringr)
library(merTools)
library(gbm)
library(xgboost)
library(boot)
library(dplyr)
library(caret)

# Load File Paths
dropbox <- "~/YSE Dropbox/Emily Goddard/India-IGUIDE/"
outdir <- paste0(dropbox, "output_flood/")
input <- paste0(dropbox, "_data/")
xwalk_path <- paste0(input,"xwalks/")

#==============================================================================#
# 1.0 Set parameters ####
#==============================================================================#
# Geographic column name
column_name <- "state_dist_code"

# Demographic columns
demo_vars <- c("gender", "age", "caste", "urban") 

# Calculate all demographic combinations
col_vars <- c()
if(length(demo_vars)>1){
  for(k in 2:length(demo_vars)){
    temp_vars <- paste0(utils::combn(demo_vars, k, paste0, collapse = "_"))
    col_vars <- c(col_vars, temp_vars)
  }
}
#==============================================================================#
# 2.0 Load Files ####
#==============================================================================#
# Crosswalk file
key <- read.xlsx(paste0(xwalk_path,"xwalk_district.xlsx"))

# Model file
model <- readRDS(paste0(input,"/best_model_03112026_withoutGSD.rds"))

# Census DF
df_census <- readRDS(paste0(dropbox, "_data/census_df/df_district_covariates.rds"))

# Poll Data
poll <- readRDS(paste0(dropbox, "_data/poll/poll_covariates.rds"))

# # Covariate dataframe
# df <- read.csv(paste0(input,"/_archive/district_wise_covars_flood.csv"))
# 
# # Spatial covariates dataframe
# df2 <- read.csv(paste0(input,"/_archive/district_level_spatial_features_covariates.csv"))
# 
# # Census counts
# load(paste0(input,"/df_district.rda"))
# 
# # Poll
# poll <- read.csv(paste0(input,"/poll_flood_updated_03052026.csv"))

#==============================================================================#
# 3.0 Prep and Clean Data ####
#==============================================================================#
# # Merge covariate dataframes
# df <- base::merge(df, df2, by="di_code",all=TRUE)
# 
# # Add leading zeros to .csv file
# df$di_code <- ifelse(
#   nchar(df$di_code) == 1,
#   paste0("00", df$di_code),
#   ifelse(
#     nchar(df$di_code) == 2,
#     paste0("0", df$di_code),
#     as.character(df$di_code)
#   )
# )
# 
# # Change column name of population counts
# colnames(df)[colnames(df)=="N"] <- "count"
# 
# # Merge data with crosswalk
# df <- base::merge(df, key, by.x="di_code",by.y="district_shape23_code", all.y=TRUE)
# 
# # Merge covariates with census counts
# df_census <- base::merge(df, df_district, by=c("state_dist_code","state_code","district_code"), all.x=TRUE)

# Subset dataframe to only covariates used in the model
df_covar <- df_census %>%
  dplyr::select(all_of(model$var.names))

#==============================================================================#
# 4.0 Run Post-Stratification ####
#==============================================================================#
#### 0. Set seed ####
set.seed(2496)

#### 1. List codes ####
geocode <- levels(as.factor(df_covar$GEOID))

#### 2. Create new prediction code using new predict() functionality for merMod objects ####
cellpred <- predict(model,df_covar,type="response",allow.new.levels=TRUE, params = list(predict_disable_shape_check = TRUE))

#### 3. Weight prediction by frequency of cell (not currently used) ####
cellpredweighted <- cellpred*df_census$n_pct_geo

#### 4. Calculate n for each cell ####
cellpred_n <- cellpred*df_census$n


#### 5. Calculate Confidence Intervals
bootfun <- function(poll) {
  #As before, just the defaults in this example. Palmitic is the first variable, hence data[,1]
  tr <- train(poll[,-1], poll[,1], method = "gbm", verbose=FALSE)
  
  predict(tr$Model, data = df_covar,type="response",allow.new.levels=TRUE, params = list(predict_disable_shape_check = TRUE))
}

#Perform the bootstrap, this can be very time consuming. Just 99 replicates here but we usually want to do more, e.g. 500. Consider using the parallel option
b <- boot(data = df_covar, statistic = bootfun, R = 99)


#Get the 95% intervals from the boot object as the 2.5th and 97.5th percentiles
lims <- t(apply(b$t, 2, FUN = function(x) quantile(x, c(0.025, 0.975))))




# Define a function to train the model and make predictions
xgboost_predict <- function(data, indices) {
  dtrain <- xgb.DMatrix(data = data[indices, -1], label = data[indices, 1])
  model <- xgboost(data = dtrain, params = params, nrounds = 99, verbose = 0)
  pred <- predict(model, as.matrix(mtcars[, -1]))
  return(pred)
}

# Combine the response and predictor variables into one dataframe
data_combined <- cbind(y, x)

# Apply bootstrapping
set.seed(123)  # For reproducibility
bootstrap_results <- boot(data_combined, statistic = xgboost_predict, R = 1000)

# Calculate the confidence intervals
confidence_intervals <- apply(bootstrap_results$t, 2, function(x) quantile(x, 
                                                                           probs = c(0.025, 0.975)))

# Print the confidence intervals for the first few predictions
print(confidence_intervals[, 1:5])






#### 5. Calculate national total n and proportion ####
pred.total <- data.frame(sum(df_census$n, na.rm=TRUE), 
                         sum(cellpred_n, na.rm=TRUE), 
                         sum(cellpred_n, na.rm=TRUE)/sum(df_census$n, na.rm=TRUE))
names(pred.total) <- c("national.n", "national.pred.n", "national.pred.prop")

#### 6. Combine to dataframe ####
df2 <- cbind(df_census, cellpredweighted, cellpred, cellpred_n) 

#### 7. Create columns for all demographic variables and combinations ####
if(length(col_vars)>1){
  temp_vars <- col_vars[grepl(".*_.*", col_vars)]
  for(c in 1:length(temp_vars)){
    # Identify the number of demographic variables
    if(lengths(regmatches(temp_vars[c], gregexpr("_", temp_vars[c])))==1){
      var1 <- gsub("_.*", "", temp_vars[c])
      var2 <- gsub(".*_", "", temp_vars[c])
      df2$newcol <- paste(df2[,colnames(df2)==var1],df2[,colnames(df2)==var2],sep="_")
      colnames(df2)[colnames(df2)=="newcol"] <- temp_vars[c]
    }else if(lengths(regmatches(temp_vars[c], gregexpr("_", temp_vars[c])))==2){
      var1 <- gsub("_.*", "", temp_vars[c])
      var3 <- gsub(".*_", "", temp_vars[c])
      var2 <- gsub(paste0(var1,"_"),"",temp_vars[c])
      var2 <- gsub(paste0("_",var3), "", var2)
      df2$newcol <- paste(df2[,colnames(df2)==var1], df2[,colnames(df2)==var2], df2[,colnames(df2)==var3], sep="_")
      colnames(df2)[colnames(df2)=="newcol"] <- temp_vars[c]
    }else if(lengths(regmatches(temp_vars[c], gregexpr("_", temp_vars[c])))==3){
      var1 <- gsub("_.*", "", temp_vars[c])
      var4 <- gsub(".*_", "", temp_vars[c])
      var2 <- gsub(paste0(var1,"_"),"",temp_vars[c])
      var2 <- gsub(paste0("_.*"), "", var2)
      var3 <- gsub(paste0("_",var4),"",temp_vars[c])
      var3 <- gsub(paste0(".*_"), "", var3)
      df2$newcol <- paste(df2[,colnames(df2)==var1], df2[,colnames(df2)==var2], df2[,colnames(df2)==var3], df2[,colnames(df2)==var4], sep="_")
      colnames(df2)[colnames(df2)=="newcol"] <- temp_vars[c]
    }
  }
}

#### 8. Loop over columns to calculate demographic columns by area ####
cols2 <- c()
for(j in 1:length(col_vars)){
  name <- paste0("area_",col_vars[j])
  df2$area <- paste(df2$GEOID, df2[,col_vars[j]], sep="_")
  colnames(df2)[colnames(df2)=="area"] <- name
  cols2 <- c(cols2, name)
}

#### 9. Aggregate predictions at geography level ####
pred_area                 <- aggregate(df2[,c("n", "cellpred_n")], list(df2$GEOID), FUN=sum, na.rm=TRUE) #"cellpred_n_lower", "cellpred_n_upper"
pred_area$pred_prop       <- pred_area$cellpred_n / pred_area$n
pred_area$pred_per        <- pred_area$pred_prop*100

#### 10. Merge names and FIPS codes ####
pred_area <- base::merge(key, pred_area, by.x=column_name, by.y="Group.1", all=TRUE)

#### 11. Calculate area and total preditions ####
if(!column_name %in% colnames(df2)){
  colnames(df2)[colnames(df2)=="GEOID"] <- column_name
}

pred_area_list <- list()
pred_total_list <- list()
for(j in 1: length(cols2)){
  # 12.1 Calculate area-specific predictions
  pred_area_list[[j]]                         <- aggregate(df2[,c("n", "cellpred_n")], list(df2[,colnames(df2)==column_name], df2[,cols2[j]]), FUN=sum, na.rm=TRUE) 
  pred_area_list[[j]]$pred_prop               <- pred_area_list[[j]]$cellpred_n / pred_area_list[[j]]$n
  pred_area_list[[j]]$pred_per                <- pred_area_list[[j]]$pred_prop*100

  # 12.2 Merge with key
  names(pred_area_list[[j]])[1] <- column_name
  pred_area_list[[j]] <- base::merge(key, pred_area_list[[j]], by=column_name, all=TRUE)
  
  # 12.3 Calculate total (national) predictions
  pred_total_list[[j]]                         <- aggregate(df2[,c("n", "cellpred_n")], list(df2[,col_vars[j]]), FUN=sum, na_rm=TRUE) 
  pred_total_list[[j]]$pred_prop               <- pred_total_list[[j]]$cellpred_n / pred_total_list[[j]]$n
  pred_total_list[[j]]$pred_per                <- pred_total_list[[j]]$pred_prop*100
}
names(pred_area_list) <- cols2
names(pred_total_list) <- col_vars

#### 13. Return output ####
pred <- list(df2, pred_area)
data_list <- list(pred, pred_total_list, pred_area_list)

areapred <- data_list[[1]][[2]]
pred_total_demo <- data_list[[2]]
pred_area_demo <- data_list[[3]]

#==============================================================================#
# 5.0 Output Post-Stratification Results ####
#==============================================================================#
#### 1. Sum at national level for difference plots ####
national_n                                <- sum(areapred$n, na.rm=TRUE)
national_cellpred_n                       <- sum(areapred$cellpred_n, na.rm=TRUE)
national_prop                             <- national_cellpred_n / national_n
national_per                              <- national_prop*100
areapred$national_per                     <- national_per
areapred$pred_prop[areapred$pred_prop==0] <- national_prop

#### 2. Create national dataframe ####
natpred <- cbind("us",national_n,national_cellpred_n,national_prop,national_per) 
colnames(natpred) <- c("country","n","cellpred_n","pred_prop","pred_per") 

#### 3. Calculate difference from national average for each area ####
areapred$pred_per_diff <- areapred$pred_per - national_per

for(i in 1:length(pred_area_demo)){
  pred_area_demo[[i]]$temp <- gsub("[0-9]._", "",pred_area_demo[[i]]$Group.2)
  for(j in 1:length(unique(pred_total_demo[[i]]$Group.2))){
    predvar <- pred_total_demo[[i]]$Group.2[j]
    pred_area_demo[[i]]$pred_per_diff[pred_area_demo[[i]]$temp == predvar] <-
      pred_area_demo[[i]]$pred_per[pred_area_demo[[i]]$temp == predvar] - pred_total_demo[[i]]$pred_per[pred_total_demo[[i]]$Group.2 == predvar]
  }
  pred_area_demo[[i]]$temp <- NULL
}

# Label names and codes for all geographies
areapred_write <- areapred %>%
  dplyr::mutate(GeoName = str_to_title(state_district_shape23),
                GeoID = paste0(state_shape23_code, district_shape23_code),
                state = str_to_title(state_shape23)) %>%
  dplyr::select(GeoName, GeoID, state, zone, n, cellpred_n, pred_prop, pred_per, national_per, pred_per_diff)

#### 4. Save geography files ####
if(dir.exists(paste0(outdir,"final/"))==FALSE){
  dir.create(paste0(outdir,"final/"), recursive=TRUE)
}
# Save files for mapping
write.csv(areapred, file=paste0(outdir,"final/flood_district_mapping.csv"), row.names = FALSE)
saveRDS(areapred, file=paste0(outdir,"final/flood_district_mapping.rds"))

# Save results
write.csv(areapred_write, file=paste0(outdir,"final/flood_district_table.csv"), row.names = FALSE)
saveRDS(areapred_write, file=paste0(outdir,"final/flood_district_table.rds"))

### 5. Save country files ####
write.csv(natpred, file=paste0(outdir,"final/flood_country_table.csv"), row.names = FALSE)
saveRDS(natpred, file=paste0(outdir,"final/flood_country_table.rds"))

### 6. Save demographic breakdowns ####
if(dir.exists(paste0(outdir,"demo_tables/"))==FALSE){
  dir.create(paste0(outdir,"demo_tables/"), recursive=TRUE)
}
for(i in 1:length(pred_area_demo)){
  demo <- names(pred_total_demo)[i]
  write.csv(pred_area_demo[[i]], file=paste0(outdir,"demo_tables/",demo,"_flood_table.csv"), row.names = FALSE)
}
for(i in 1:length(pred_total_demo)){
  demo <- names(pred_total_demo)[i]
  write.csv(pred_total_demo[[i]], file=paste0(outdir,"demo_tables/",demo,"flood_country_table.csv"), row.names = FALSE)
}
#==============================================================================#
# END OF FILE
#==============================================================================#
