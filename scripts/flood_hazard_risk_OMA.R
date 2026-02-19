
###############################################################
# 4 Feature Scenarios × 4 Classifiers (R version)
# Compares different feature combinations with multiple classifiers
# using 250km spatial stratification.
# 
# Scenarios:
#   M1: Demographics only
# M2: M1 + Climate + Flood indicators
# M3: M2 + Spatial features (lag + LISA)
# M4: M3 + Alpha Earth embeddings (everything)
# 
# Classifiers:
# - RandomForest
# - GradientBoosting
# - LogisticRegression
# - LightGBM
##############################################################
install.packages("RhpcBLASctl")
library(RhpcBLASctl)
library(terra)
library(data.table)
library(dplyr)
library(sf)
library(spdep)
library(caret)
library(pROC)
library(ranger)
library(xgboost)
library(lightgbm)
library(gbm)      


# ============================================================
# PATH SETUP
# ============================================================

#SCRIPT_DIR <- getwd()
PROJECT_ROOT <- ("C:/Users/alegb/Documents/iguide/modeling")
RAW_DATA_PATH <- file.path(PROJECT_ROOT, "data/RAP_Flood_merged_all_updated_20260217.csv")
SHAPEFILE_PATH <- file.path(PROJECT_ROOT,
                            "shapefile/district_state_shapefile_2023.shp")
RESULTS_DIR <- file.path(PROJECT_ROOT, "results")


# ============================================================
# LOAD DATA
# ============================================================

#data <- read.csv(RAW_DATA_PATH)
data <- fread(RAW_DATA_PATH)
head(data, 1)

#Shapefile
shp <- st_read(SHAPEFILE_PATH, quiet = TRUE)
# plot(shp)

TARGET <- "n7fy23_recode"

data <- data[!is.na(get(TARGET))]
data[[TARGET]] <- as.integer(data[[TARGET]])
data$di_code <- as.integer(data$di_code)

#cat(nrow(data), "individuals loaded\n")

# ============================================================
# SPATIAL SPLIT (250 km grid)
# ============================================================

set.seed(42)

#transform the crs
shp_utm <- st_transform(shp, 32644)
centroids <- st_centroid(shp_utm)

#attach the coordinates
coords <- st_coordinates(centroids)
shp_utm$centroid_x <- coords[,1]
shp_utm$centroid_y <- coords[,2]

#splitting the data based on their spatial location
grid_meters <- 250 * 1000
x_min <- min(shp_utm$centroid_x)
y_min <- min(shp_utm$centroid_y)

shp_utm$grid_cell <- paste0(
  floor((shp_utm$centroid_x - x_min)/grid_meters), "_",
  floor((shp_utm$centroid_y - y_min)/grid_meters)
)

#Unique number of codes
cells <- unique(shp_utm$grid_cell)
test_cells <- sample(cells, max(1, floor(length(cells)*0.18)))

#splitting the train and test data 
shp_utm$split <- ifelse(shp_utm$grid_cell %in% test_cells,
                        "test", "train")

##### find district code column
shp_code_col <- NULL

# Columns to skip
skip_cols <- c("geometry", "centroid_x", "centroid_y", "grid_cell", "split")

# Loop through shapefile columns
for (col in names(shp_utm)) {
  
  if (col %in% skip_cols) next
  
  tryCatch({
    # Convert to numeric integers and remove NA
    vals <- unique(na.omit(as.integer(as.numeric(shp_utm[[col]]))))
    # Compare with dataset IDs
    if (length(intersect(vals, unique(data$di_code))) > 100) {
      shp_code_col <- col
      break
    }
    
  }, error = function(e) {
    
  })
}

# Remove rows without IDs
shp_utm <- shp_utm[!is.na(shp_utm[[shp_code_col]]), ]

# Convert ID column to integer
shp_utm[[shp_code_col]] <- as.integer(shp_utm[[shp_code_col]])


split_map <- shp_utm %>%
  st_drop_geometry() %>%
  select(all_of(shp_code_col), split)

data <- left_join(data, split_map,
                  by=setNames(shp_code_col,"di_code"))

data$split[is.na(data$split)] <- "train"


################################################################ 
# Checking for accuracy 
# data2 <- data
# split_map2 <- setNames(shp_utm$split,
#                       shp_utm[[shp_code_col]])
# data2$split <- split_map2[as.character(data$di_code)]
################################################################ 


train_n <- sum(data$split=="train")
test_n <- sum(data$split=="test")

cat("Total data:", length(data$caseid),  "\nTraining data:", train_n, "\nTesting data:", test_n)


# ============================================================
# SPATIAL FEATURES
# ============================================================

district_data <- data %>%
  group_by(di_code) %>%
  summarise(worry_rate=mean(.data[[TARGET]], na.rm=TRUE))

env_vars <- c("precip_sum_mm","mean_tmax","vulnerability_district",
              "FlowAcc_sum","GFD_sum","flood_news_count",
              "Tone_Avg","mean_monthly_avg_rd_MD")

env_vars <- env_vars[env_vars %in% names(data)]

env_agg <- data %>%
  group_by(di_code) %>%
  summarise(across(all_of(env_vars), first))

district_data <- left_join(district_data, env_agg)

#Matching with shapefiles
shp_subset <- shp %>%
  select(all_of(shp_code_col), geometry)

shp_subset <- shp_subset %>%
  filter(!is.na(.data[[shp_code_col]]))

shp_subset[[shp_code_col]] <- as.integer(shp_subset[[shp_code_col]])

# Merge with district data
geo_districts <- shp_subset %>%
  left_join(district_data, by = setNames("di_code", shp_code_col))

# Ensure it is an sf spatial object
geo_districts <- st_as_sf(geo_districts)

# neighbors
nb <- poly2nb(geo_districts, queen = TRUE)

# Identify islands (no neighbors)
islands <- which(card(nb) == 0)

# Use k-nearest neighbors if island exist
if (length(islands) > 0) {
  
  coords <- st_coordinates(st_centroid(geo_districts))
  
  knn <- knearneigh(coords, k = 5)
  knn_nb <- knn2nb(knn)
  
  # Replace island neighbors with KNN neighbors
  for (i in islands) {
    nb[[i]] <- knn_nb[[i]]
  }
}

# Convert to row-standardized spatial weights
weights <- nb2listw(nb, style = "W")
# spatial lag
lag_vars <- intersect(c("worry_rate","precip_sum_mm","mean_tmax",
                        "vulnerability_district","FlowAcc_sum",
                        "GFD_sum","flood_news_count","Tone_Avg"),
                      names(geo_districts))

SPATIAL_LAG_COLS <- c()
for(v in lag_vars){
  col <- paste0("lag_",v)
  values <- geo_districts[[v]]
  values[is.na(values)] <- 0
  geo_districts[[col]] <- lag.listw(weights,
                                    values,
                                    zero.policy=TRUE)
  SPATIAL_LAG_COLS <- c(SPATIAL_LAG_COLS, col)
}

# LISA clusters
lisa_variables <- c("worry_rate", "precip_sum_mm", "mean_tmax",
                    "vulnerability_district", "GFD_sum")

lisa_variables <- lisa_variables[lisa_variables %in% names(geo_districts)]
#Classification
cluster_names <- c(
  "1" = "high_high",
  "2" = "low_high",
  "3" = "low_low",
  "4" = "high_low"
)

LISA_COLS <- c()
for (v in lisa_variables) {
  
  values <- geo_districts[[v]]
  values[is.na(values)] <- 0
  
  lisa <- localmoran(values, weights, zero.policy = TRUE)
  
  short_name <- gsub("_district|_sum", "", v)
  
  # Local Moran's I
  col_I <- paste0("lisa_I_", short_name)
  geo_districts[[col_I]] <- lisa[, "Ii"]
  LISA_COLS <- c(LISA_COLS, col_I)
  
  # Significance
  p_col <- grep("^Pr", colnames(lisa), value = TRUE)
  significant <- lisa[, p_col] <= 0.05
  #significant <- lisa[, "Pr(z > 0)"] <= 0.05
  
  # Cluster classification
  lag_vals <- lag.listw(weights, values, zero.policy = TRUE)
  
  q <- ifelse(values > mean(values) & lag_vals > mean(lag_vals), 1,
              ifelse(values < mean(values) & lag_vals > mean(lag_vals), 2,
                     ifelse(values < mean(values) & lag_vals < mean(lag_vals), 3, 4)))
  
  for (code in names(cluster_names)) {
    name <- cluster_names[[code]]
    col <- paste0("lisa_", short_name, "_", name)
    
    geo_districts[[col]] <- as.integer(q == as.numeric(code) & significant)
    LISA_COLS <- c(LISA_COLS, col)
  }
  
  # Non-significant cluster flag
  col_ns <- paste0("lisa_", short_name, "_not_sig")
  geo_districts[[col_ns]] <- as.integer(!significant)
  LISA_COLS <- c(LISA_COLS, col_ns)
}

cat("Created", length(LISA_COLS), "LISA cluster features")


# Merge back the spatial features to individual level data
# Select spatial columns
spatial_cols <- c("di_code", SPATIAL_LAG_COLS, LISA_COLS)

spatial_df <- geo_districts %>%
  st_drop_geometry() %>%  
  select(all_of(spatial_cols))

# Merge with main data
data <- data %>%
  left_join(spatial_df, by = "di_code")

# Replace NA with 0
cols_to_fill <- c(SPATIAL_LAG_COLS, LISA_COLS)
for (col in cols_to_fill) {
  data[[col]][is.na(data[[col]])] <- 0
}


# ============================================================
#  FEATURE SCENARIOS
# ============================================================

# M1: Demographics only
M1 <- c("age","gender","urban","caste","di_code")

# M2 <- intersect(c(
#   "mean_monthly_avg_rd_MD","precip_sum_mm","precip_mean_mm",
#   "mean_tmax","district_flood_sentiment","state_flood_sentiment",
#   "vulnerability_district","FlowAcc_sum","GFD_sum",
#   "flood_news_count","Tone_Avg","Tone_Positive",
#   "Tone_Negative","Tone_Polarity","Tone_ActivityRef",
#   "Tone_SelfGroupRef","highersecondaryabove_district"),
#   names(data))

# M2: M1 + Climate + Flood indicators
M2 <- intersect(c(
  #climate data
  "mean_monthly_avg_rd_MD","precip_sum_mm","precip_mean_mm",
  "mean_tmax",
  #sentiment
  "district_flood_sentiment","state_flood_sentiment",
  # Flood/vulnerability indicators
  "vulnerability_district","FlowAcc_sum","GFD_sum",
  #News/Tome
  "flood_news_count","Tone_Avg","Tone_Positive",
  "Tone_Negative","Tone_Polarity","Tone_ActivityRef",
  "Tone_SelfGroupRef",
  #District education
  "highersecondaryabove_district"), names(data))

# M3: M2 + Spatial features
M3 <- c(SPATIAL_LAG_COLS, LISA_COLS)

#M4: All predictors with AlphaEarth Embeddings
ALPHA_EARTH <- grep("A[0-9]+_", names(data), value=TRUE)

#Scenario
SCENARIOS <- list(
  M1_Demographics = M1,
  M2_Climate_Flood = unique(c(M1,M2)),
  M3_With_Spatial = unique(c(M1,M2,M3)),
  M4_Everything = unique(c(M1,M2,M3,ALPHA_EARTH))
)


# Remove duplicates and keep only existing columns
SCENARIOS <- lapply(SCENARIOS, function(features) {
  unique_features <- unique(features)          # remove duplicates
  existing_features <- unique_features[unique_features %in% names(data)]
  return(existing_features)
})

# Print feature scenario summary
cat("\nFeature scenarios:\n")
for (name in names(SCENARIOS)) {
  cat(sprintf("  %s: %d features\n", name, length(SCENARIOS[[name]])))
}


# ============================================================
# ENCODE CATEGORICALS
# ============================================================

categorical_cols <- c("age", "gender", "urban", "caste")

data <- data %>%
  mutate(across(all_of(categorical_cols), ~as.integer(as.factor(.x))))


# ============================================================
#  TRAIN MODELS BY SCENARIOS
# ============================================================

SEED <- 42

CLASSIFIERS <- list(
  #Random Forest
  RandomForest = list(func = ranger, params = list(num.trees = 100, max.depth = 10, 
                                                   importance = 'impurity')),
  
  #Gradient Boosting
  GradientBoosting = list(func = gbm, 
                          params = list(n.trees = 100, interaction.depth = 5, shrinkage = 0.1, 
                                        distribution = "bernoulli", verbose = FALSE)),
  #Logistic Regression
  LogisticRegression = list(func = glm, params = list(family = binomial())),
  
  #Light GBM
  LightGBM = list(func = lightgbm) 
  #params = list(num_leaves = 31, 
  #max_depth = 3, learning_rate = 0.1, objective = "binary"))
)


# Initialize storage
results <- list()
feature_importances <- list()
models <- list()
current <- 0
total_combinations <- length(SCENARIOS) * length(CLASSIFIERS)


# Main loop over scenarios and classifiers
for (scenario_name in names(SCENARIOS)) {
  
  features <- SCENARIOS[[scenario_name]]
  cat("\n--------------------------------------------------\n")
  cat("SCENARIO:", scenario_name, "(", length(features), "features )\n")
  cat("--------------------------------------------------\n")
  
  # Prepare train/test
  train_data <- data %>% filter(as.character(split) == "train")
  test_data  <- data %>% filter(as.character(split) == "test")
  
  X_train <- train_data %>%
    select(all_of(features)) %>% mutate_all(~ifelse(is.na(.), 0, .))
  X_test  <- test_data %>%
    select(all_of(features)) %>% mutate_all(~ifelse(is.na(.), 0, .))
  y_train <- train_data %>% select(all_of(TARGET))
  y_test  <- test_data %>% select(all_of(TARGET))
  
  # Scale features for Logistic Regression
  preproc <- preProcess(X_train, method = c("center", "scale"))
  X_train_scaled <- predict(preproc, X_train)
  X_test_scaled  <- predict(preproc, X_test)
  
  # Looop through classifiers
  for (clf_name in names(CLASSIFIERS)) {
    current <- current + 1
    cat(sprintf("  [%d/%d] %s ... ", current, total_combinations, clf_name))
    
    clf <- CLASSIFIERS[[clf_name]]$func
    params <- CLASSIFIERS[[clf_name]]$params
    
    start_time <- Sys.time()
    
    # Train Models
    if (clf_name == "RandomForest") {
      formula <- as.formula(paste(TARGET, "~ ."))
      model <- do.call(clf, c(list(formula = formula, data = cbind(X_train, y_train)), params))
      
      y_prob <- predict(model, data = X_test)$predictions
      y_pred <- ifelse(y_prob > 0.5, 1, 0)
      
      # Feature importance
      imp <- data.frame(
        Feature = features,
        Importance = model$variable.importance
      ) %>% arrange(desc(Importance))
      
    } else if (clf_name == "GradientBoosting") {
      formula <- as.formula(paste(TARGET, "~ ."))
      model <- do.call(clf, c(list(formula = formula, data = cbind(X_train, y_train)), params))
      
      y_prob <- predict(model, newdata = X_test, n.trees = params$n.trees, type = "response")
      y_pred <- ifelse(y_prob > 0.5, 1, 0)
      
      # Feature importance
      imp <- summary(model, n.trees = params$n.trees, plotit = FALSE)
      colnames(imp) <- c("Feature", "Importance")
      imp <- imp %>% arrange(desc(Importance))
      
    } else if (clf_name == "LogisticRegression") {
      formula <- as.formula(paste(TARGET, "~ ."))
      model <- do.call(clf, c(list(formula = formula, data = cbind(X_train_scaled, y_train)), params))
      
      y_prob <- predict(model, newdata = X_test_scaled, type = "response")
      y_pred <- ifelse(y_prob > 0.5, 1, 0)
      
      # Feature importance
      coefs <- coef(model)[-1]  # drop intercept
      imp <- data.frame(
        Feature = names(coefs),
        Importance = abs(coefs)
      ) %>% arrange(desc(Importance))
      
    } else if (clf_name == "LightGBM") {
      y_train_num <- as.numeric(unlist(y_train))
      dtrain <- lgb.Dataset(data = as.matrix(X_train), label = y_train_num)
      model <- do.call(clf, c(list(data = dtrain)))
      
      y_prob <- predict(model, as.matrix(X_test))
      y_pred <- ifelse(y_prob > 0.5, 1, 0)
      
      # Feature importance
      imp <- lgb.importance(model, percentage = TRUE)
      imp <- imp[, c("Feature", "Gain")]
      colnames(imp) <- c("Feature", "Importance")
      imp <- imp %>% arrange(desc(Importance))
    }
    
    # Store the model
    key <- paste(scenario_name, clf_name, sep="__")
    models[[key]] <- model
    
    
    # Metrics
    #change Y_test list to numerics for AUC
    y_test_num <- as.numeric(unlist(y_test))
    #Accuracy
    acc <- mean(y_test == y_pred)
    #Precision
    precision <- ifelse(sum(y_pred == 1) == 0, NA, 
                        sum(y_pred == 1 & y_test == 1)/sum(y_pred == 1))
    #Recall
    recall <- ifelse(sum(y_test == 1) == 0, NA, 
                     sum(y_pred == 1 & y_test == 1)/sum(y_test == 1))
    #F1 Score
    f1 <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0, NA, 
                 2*precision*recall/(precision+recall))
    #AUC
    auc <- tryCatch({auc(roc(y_test_num, y_prob))}, error = function(e) NA)
    
    elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
    
    #Results
    results[[length(results)+1]] <- data.frame(
      Scenario = scenario_name,
      Classifier = clf_name,
      N_Features = length(features),
      Accuracy = acc,
      Precision = precision,
      Recall = recall,
      F1 = f1,
      AUC = auc,
      Time_sec = elapsed
    )
    
    feature_importances[[paste0(scenario_name, "__", clf_name)]] <- imp
    
    cat(
      "AUC = ", format(auc, digits=4), 
      ", Acc = ", format(acc, digits=4), 
      ", Recall = ", format(precision, digits=4),
      ", Precision = ", format(precision, digits=4),
      ", F1 = ", format(f1, digits=4),
      " (", elapsed, "s)\n", sep=""
    )
    
  } 
}

# Convert results list to data frame
results_df <- as.data.frame(do.call(rbind, results))

View(results_df)

# Round numeric values for display
results_df <- results_df %>%
  mutate(
    Accuracy = round(Accuracy, 4),
    AUC = round(AUC, 4),
    F1 = round(F1, 4),
    Precision = round(Precision, 4),
    Recall = round(Recall, 4)
  )

print(results_df)
View(results_df)

# ============================================================================
# VISUALIZE EVALUATION METRICS
# ============================================================================

# Plot by each metric - Accuracy
ggplot(results_df, aes(x = Classifier, y = Accuracy, fill = Scenario)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = sprintf("%.2f", Accuracy)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4
  ) + 
  scale_fill_manual(values = c("cornsilk", "coral", "burlywood", "darkgoldenrod"))+
  labs(title = "Accuracy", y = "Accuracy", x = NULL) +
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# AUC
ggplot(results_df, aes(x = Classifier, y = AUC, fill = Scenario)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = sprintf("%.2f", AUC)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4
  ) + 
  scale_fill_manual(values = c("cornsilk", "coral", "burlywood", "darkgoldenrod"))+
  labs(title = "AUC", y = "AUC", x = NULL) +
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# F1 Score
ggplot(results_df, aes(x = Classifier, y = F1, fill = Scenario)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = sprintf("%.2f", F1)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4
  ) + 
  scale_fill_manual(values = c("cornsilk", "coral", "burlywood", "darkgoldenrod"))+
  labs(title = "F1 Score", y = "F1 Score", x = NULL) +
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Precision
ggplot(results_df, aes(x = Classifier, y = Precision, fill = Scenario)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = sprintf("%.2f", Precision)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4
  ) + 
  scale_fill_manual(values = c("cornsilk", "coral", "burlywood", "darkgoldenrod"))+
  labs(title = "Precision", y = "Precision", x = NULL) +
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Recall
ggplot(results_df, aes(x = Classifier, y = Recall, fill = Scenario)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = sprintf("%.2f", Recall)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4
  ) + 
  scale_fill_manual(values = c("cornsilk", "coral", "burlywood", "darkgoldenrod"))+
  labs(title = "Recall", y = "Recall", x = NULL) +
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )



# Visualization - One Chart for All Metrics 
results_long <- results_df %>%
  pivot_longer(
    cols = c(Accuracy, Precision, Recall, F1, AUC),
    names_to = "Metric",
    values_to = "Value"
  )


ggplot(results_long, aes(x = Classifier, y = Value, fill = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = sprintf("%.2f", Value)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4
  ) + scale_fill_manual(values = c("cornsilk", "coral", "burlywood", "darkgoldenrod"))+
  facet_grid("Metric")



# ============================================================================
# SELECT THE BEST MODEL
# ============================================================================

# Best model by AUC
best_auc_row <- results_df %>% slice_max(AUC, n = 1)
print(best_auc_row)
cat("\nBEST MODEL BY AUC (Scenario) :", best_auc_row$Scenario, "\n")
cat("\nBEST MODEL BY AUC (AUC Value):", best_auc_row$AUC, "\n")
cat("\nBEST MODEL BY AUC (Accuracy):", best_auc_row$Accuracy, "\n")
cat("\nBEST MODEL BY AUC (F1 Score):", best_auc_row$F1, "\n")
cat("\nBEST MODEL BY AUC (Precision):", best_auc_row$Precision, "\n")
cat("\nBEST MODEL BY AUC (Recall):", best_auc_row$Recall, "\n")
cat("\nBEST MODEL BY AUC (Features):", best_auc_row$N_Features, "\n")



# Best classifier per scenario
best_per_scenario <- results_df %>%
  group_by(Scenario) %>%
  slice_max(AUC, n = 1) %>%
  select(Scenario, Classifier, AUC)

cat("\nBEST CLASSIFIER PER SCENARIO:\n")
print(best_per_scenario)


# Feature Importance for the Best Model
best_key <- paste(best_auc_row$Scenario, best_auc_row$Classifier, sep="__")

if (best_key %in% names(feature_importances)) {
  
  best_imp <- feature_importances[[best_key]] %>%
    arrange(desc(Importance)) %>%
    slice_head(n = 20)
  
  ggplot(best_imp,
         aes(x = reorder(Feature, Importance),
             y = Importance)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = paste(best_auc_row$Scenario, "+", best_auc_row$Classifier,
                    "Feature Importance"),
      x = "Feature",
      y = "Importance"
    ) +
    theme_minimal(base_size = 12)
}



# ============================================================
# SAVE RESULTS
# ============================================================

###################################################################################
# If the best model is a Random Forest or Gradient Boosting, use the code below
###################################################################################

best_model <- models[[best_key]]
model_name <- "best_model.rds"
model_full_path <- file.path(RESULTS_DIR, model_name)
saveRDS(best_model, model_full_path)

###################################################################################
# If the model is LightGBM, use this:
###################################################################################
best_model <- models[[best_key]]
model_name <- "best_model.txt"
model_full_path <- file.path(RESULTS_DIR, model_name)
lightgbm::lgb.save(model, model_full_path)


# Save the model comparison results
write.csv(results_df,
          file.path(RESULTS_DIR,"model_comparison_results.csv"),
          row.names=FALSE)

cat("Completed.\n")
