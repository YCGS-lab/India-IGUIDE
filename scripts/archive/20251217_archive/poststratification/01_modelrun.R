#==============================================================================#
# India Downscaling ###

# INPUTS:
# Poll dataframe
# Crosswalk files
# Census dfs

# Last modified: November 4, 2025, EG
#==============================================================================#
# Set R Environment ####
#==============================================================================#
rm(list=ls())

library(lme4)
library(tidyverse)
library(arm)
library(lattice)
library(merTools)
library(grid)
library(readxl)

# Set file paths
github <- paste0("~/GitHub/ypccc_india/india_w1/iguide/")
outputpath <- paste0(github, "output/")
data <- paste0(github,"datafiles/")

# Load Poll Data
poll <- read.csv(paste0(data,"full_extract.csv"))

# Load Census df (Post-Stratification Weights)
load(paste0(data,"census_df/df_district.Rda"))
load(paste0(temp,"census_df/df_state.Rda"))

# Load Crosswalks
xwalk_state <- read_excel(paste0(data,"xwalk/xwalk_state.xlsx"))
xwalk_district <- read_excel(paste0(data,"xwalk/xwalk_district.xlsx"))
#==============================================================================#
## MODEL RUN FILE ####
#==============================================================================#
#### 1. Set inputs ####
demo_vars <- c("gender", "age", "caste", "urban") 
year <- "2025"
level <- "state" #"district"
myvars <- "drought" #"flood" # Vector of variable(s) to run through the post-strat function

model_list <- list()
variance_list <- list()
for (i in seq_along(myvars)){
  print(myvars[i]) 
  currvar   <- as.character(myvars[i])
  currtime  <- year
  currdir   <- outputpath
  if (level=="district") {
    df <- df_district
    key <- xwalk_district
    column_name <- "state_dist_code"
  }
  if (level=="state")    {
    df <- df_state
    key <- xwalk_state
    column_name <- "state_code"
  }
  
  #### 2. Select model ####
  
  #### PUT YOUR MODEL HERE ####
  
  #### 3. Extract Model coefficients ####
  model_list[[i]] <- as.data.frame(coef(summary(model)))
  model_list[[i]]$AIC <- AIC(model)
  model_list[[i]]$BIC <- BIC(model)
  model_list[[i]]$logLik <- logLik(model)
  model_list[[i]]$residual <- df.residual(model)
  model_list[[i]]$covariate <- rownames(model_list[[i]])
  model_list[[i]]$n_obs <- nobs(model)
  model_list[[i]]$question <- currvar
  
  #### 3.1 Extract Model random effect variance ####
  variance_list[[i]] <- as.data.frame(VarCorr(model))
  variance_list[[i]]$var2 <- currvar
  
  #### 4. Create list of demographic variable combinations to loop over ####
  col_vars <- demo_vars
  if(length(demo_vars)>1){
    for(k in 2:length(demo_vars)){
      temp_vars <- paste0(utils::combn(demo_vars, k, paste0, collapse = "_"))
      col_vars <- c(col_vars, temp_vars)
    }
  }
  
  #### 5. poststratify ####
  data_list <- poststrat(df, poll=poll, key=key, col_vars=col_vars, 
                         column_name=column_name, model=model, 
                         target_time=currtime, xsim=TRUE, n_sims=99)
  pred <- data_list[[1]]
  pred_total_demo <- data_list[[2]]
  pred_area_demo <- data_list[[3]]
  
  #### 6. Save Post-Stratification Predictions ####
  map_pred <- poststrat_save(projections=pred, pred_total_demo=pred_total_demo, 
                             pred_area_demo=pred_area_demo, level=level, 
                             column_name=column_name, var=currvar, time=currtime, 
                             outdir=currdir)
  pred.range <- setNames(data.frame(max(pred[[2]]$pred_prop),min(pred[[2]]$pred_prop),
                                    (max(pred[[2]]$pred_prop)-min(pred[[2]]$pred_prop))),
                         c("max","min","range"))
  
  if(dir.exists(paste0(currdir,level,"/model_output/"))==FALSE){
    dir.create(paste0(currdir,level,"/model_output/"), recursive=TRUE)
  }
  capture.output(pred.range, summary(model),file = paste0(currdir,level,"/model_output/",currvar,".txt"))
}

#### 7. Save Model Fixed Effects Estimates ####
model_estimates <- do.call(rbind, model_list)
write.csv(model_estimates, file=paste0(currdir,level,"/",level,"_model_estimates.csv"), row.names = FALSE)

#### 8. Save Model Variances ####
random_effect_var <- do.call(rbind, variance_list)
write.csv(random_effect_var, file=paste0(currdir,level,"/",level,"_random_effect_variance.csv"), row.names = FALSE)

#==============================================================================#
# END OF FUNCTION ####
#==============================================================================#
