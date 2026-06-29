#==============================================================================#
# calculate_cis.R ####
#==============================================================================#
### Calculcate Confidence Intervals

# INPUTS:
# survey_data   : data.frame with covariates, geographic level var, and DV
# weight_df   : data.frame with covariates, geographic level var, and weights
# best_model      : your fitted gbm model object

# OUTPUTS:
# MRP Confidence Intervals

# Last updated 06/29/2026 EG
#==============================================================================#
# 0.0 Setup ####
#==============================================================================#
rm(list=ls())

library(gbm)
library(dplyr)
library(openxlsx)

# Load File Paths
dropbox <- "~/YSE Dropbox/Emily Goddard/India-IGUIDE/"
outdir <- paste0(dropbox, "output_flood/")
input <- paste0(dropbox, "_data/")
xwalk_path <- paste0(input,"xwalk/")

#==============================================================================#
# 1.0 Set parameters ####
#==============================================================================#
dv           <- "n7fy23_flood_worry_recode"   # column name of DV in survey_data
geo_var      <- "state_district_census11_code" # column name of geographic unit
weight_var   <- "n_pct_geo"                    # column name of weights in weight_df
n_boot       <- 500                         # number of bootstrap iterations
ci_level     <- 0.95
#==============================================================================#
# 2.0 Load Files ####
#==============================================================================#
# Crosswalk file
key <- read.xlsx(paste0(xwalk_path,"xwalk_full.xlsx"))

# Model file
best_model <- readRDS(paste0(outdir,"model/best_model.rds"))

# Census DF
weight_df <- readRDS(paste0(dropbox, "_data/census_df/df_district_covariates.rds"))

# Poll Data
survey_data <- readRDS(paste0(dropbox, "_data/poll/poll_model.rds"))

# Fix Data types
for(i in 1:15){
  survey_data[, i] <- as.character(survey_data[, i])
}
survey_data$state_district_census11_code <- as.numeric(survey_data$state_district_census11_code)
weight_df$state_district_census11_code <- as.numeric(weight_df$state_district_census11_code)
#==============================================================================#
# 3.0 Create function ####
#==============================================================================#
# ── Helper: one post-stratified estimate per geo unit ─────────────────────────
poststratify <- function(ranger_model, weight_df, geo_var, weight_var) {
  preds <- predict(ranger_model, data = weight_df)$predictions  # ranger syntax
  
  weight_df %>%
    dplyr::mutate(.pred = preds) %>%
    dplyr::group_by(across(all_of(geo_var))) %>%
    dplyr::summarise(
      estimate = sum(.pred * get(weight_var)) / sum(get(weight_var)),
      .groups  = "drop"
    )
}

# ── Point estimates from the original best model ──────────────────────────────
point_estimates <- poststratify(best_model, weight_df, geo_var, weight_var)

# ── Bootstrap loop ────────────────────────────────────────────────────────────
set.seed(42)

boot_results <- vector("list", n_boot)

# ── Bootstrap loop ─────────────────────────────────────────────────────────────
for (i in seq_len(n_boot)) {
  
  if (i %% 50 == 0) message("Bootstrap iteration: ", i, " / ", n_boot)
  
  boot_sample <- survey_data[sample(nrow(survey_data), replace = TRUE), ]
  
  for (col in names(boot_sample)) {
    if (is.factor(boot_sample[[col]])) {
      boot_sample[[col]] <- factor(boot_sample[[col]], levels = levels(survey_data[[col]]))
    }
  }
  
  boot_model <- ranger::ranger(
    formula      = as.formula(paste(dv, "~ .")),
    data         = boot_sample %>% dplyr::select(-all_of(geo_var)), 
    probability  = TRUE,          # gives probabilities for binary DV, equivalent to bernoulli in gbm
    num.trees    = best_model$num.trees,
    mtry         = best_model$mtry,
    min.node.size = best_model$min.node.size
  )
  
  boot_results[[i]] <- poststratify(boot_model, weight_df, geo_var, weight_var) %>%
    dplyr::mutate(boot_iter = i)
}

# ── Summarise bootstrap distribution into CIs ─────────────────────────────────
alpha <- 1 - ci_level

boot_df <- bind_rows(boot_results)

ci_estimates <- boot_df %>%
  dplyr::group_by(across(all_of(geo_var))) %>%
  dplyr::summarise(
    ci_lower = quantile(estimate, alpha / 2),
    ci_upper = quantile(estimate, 1 - alpha / 2),
    boot_se  = sd(estimate),
    .groups  = "drop"
  ) %>%
  left_join(point_estimates, by = geo_var) %>%
  dplyr::select(all_of(geo_var), estimate, ci_lower, ci_upper, boot_se)

print(ci_estimates)