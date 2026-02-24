rm(list=ls())
library(openxlsx)

# Set parameters
col_vars <- c("gender", "age", "caste", "urban") 
column_name <- "state_dist_code"
# target_time <- 2024
xsim=FALSE

# Load File Paths
github <- paste0("~/GitHub/ypccc_india/india_w1.5/")
dropbox <- "~/Dropbox (YSE)/ypcccdb/downscale/india/"
temp <- paste0(github,"temp/")
xwalks <- paste0(temp,"xwalks/")

# Load Files
# xwalk_state <- read_excel(paste0(xwalks,"xwalk_state.xlsx"))
key <- read.xlsx(paste0(xwalks,"xwalk_district.xlsx"))
model <- readRDS("/Users/eag82/GitHub/India-IGUIDE/scripts/best_model.rds")
df <- readRDS("/Users/eag82/GitHub/ypccc_india/india_w1/iguide/datafiles/02162025/Severe_floods.rds")

# df$di_code <- df$state_district_survey

#### 0. Set seed ####
set.seed(2496)

#### 1. Add time and mode to dataframe ####
# df$year <- target_time

#### 2. List codes ####
geocode <- levels(as.factor(df$GEOID))

#### 3. Create new prediction code using new predict() functionality for merMod objects ####
cellpred <- predict(model,df,type="response",allow.new.levels=TRUE)

#### 4. Weight prediction by frequency of cell (not currently used) ####
cellpredweighted <- cellpred*df$n_pct_geo

#### 5. Calculate n for each cell ####
cellpred_n <- cellpred*df$n

#### 6. Create blank confidence intervals ####
cellpred_n_lower <- 0
cellpred_n_upper <- 0

#### 7. Use simulation to estimate confidence intervals ####
if(xsim==TRUE){
  # split data frame into chunks of 999 rows--this is needed for predictInterval to work properly in its current version 
  n.splits <- ceiling(length(df$n)/999)
  df.split <- suppressWarnings(split(df, rep(1:n.splits,each=999)))
  for(s in 1:n.splits){
    if(s==1){
      cellpred.conf <- suppressWarnings(predictInterval(model, newdata = df.split[[s]], n.sims = n_sims, stat='mean', type='probability', include.resid.var = FALSE, which=c("full")))
    } else {
      cellpred.conf.temp <- suppressWarnings(predictInterval(model, newdata = df.split[[s]], n.sims = n_sims, stat='mean', type='probability', include.resid.var = FALSE, which=c("full")))
      cellpred.conf <- rbind(cellpred.conf, cellpred.conf.temp)
    }
  }
  
  cellpred_n_lower <- cellpred.conf$lwr*df$n
  cellpred_n_upper <- cellpred.conf$upr*df$n
}

#### 8. Calculate national total n and proportion ####
pred.total <- data.frame(sum(df$n, na.rm=TRUE), 
                         sum(cellpred_n, na.rm=TRUE), 
                         sum(cellpred_n, na.rm=TRUE)/sum(df$n, na.rm=TRUE), 
                         sum(cellpred_n_lower, na.rm=TRUE)/sum(df$n, na.rm=TRUE), 
                         sum(cellpred_n_upper, na.rm=TRUE)/sum(df$n, na.rm=TRUE))
names(pred.total) <- c("national.n", "national.pred.n", "national.pred.prop", "national.pred.lower", "national.pred.upper")

#### 9. Combine to dataframe ####
df2 <- cbind(df, cellpredweighted, cellpred, cellpred_n, cellpred_n_lower, cellpred_n_upper)

#### 10. Create columns for all demographic variables and combinations ####
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

#### 11. Loop over columns to calculate demographic columns by area ####
cols2 <- c()
for(j in 1:length(col_vars)){
  name <- paste0("area_",col_vars[j])
  df2$area <- paste(df2$GEOID, df2[,col_vars[j]], sep="_")
  colnames(df2)[colnames(df2)=="area"] <- name
  cols2 <- c(cols2, name)
}

#### 12. Aggregate predictions at geography level ####
pred_area                 <- aggregate(df2[,c("n", "cellpred_n", "cellpred_n_lower", "cellpred_n_upper")], list(df2$GEOID), FUN=sum, na.rm=TRUE)
pred_area$pred_prop       <- pred_area$cellpred_n / pred_area$n
pred_area$pred_prop_lower <- pred_area$cellpred_n_lower / pred_area$n
pred_area$pred_prop_upper <- pred_area$cellpred_n_upper / pred_area$n
pred_area$pred_per        <- pred_area$pred_prop*100
pred_area$moe             <- (pred_area$pred_prop_upper - pred_area$pred_prop_lower)/2

#### 13. Merge names and FIPS codes ####
pred_area <- base::merge(key, pred_area, by.x=column_name, by.y="Group.1", all=TRUE)

#### 14. Calculate area and total preditions ####
if(!column_name %in% colnames(df2)){
  colnames(df2)[colnames(df2)=="GEOID"] <- column_name
}
pred_area_list <- list()
pred_total_list <- list()
for(j in 1: length(cols2)){
  # 14.1 Calculate area-specific predictions
  pred_area_list[[j]]                         <- aggregate(df2[,c("n", "cellpred_n", "cellpred_n_lower", "cellpred_n_upper")], list(df2[,colnames(df2)==column_name], df2[,cols2[j]]), FUN=sum, na.rm=TRUE)
  pred_area_list[[j]]$pred_prop               <- pred_area_list[[j]]$cellpred_n / pred_area_list[[j]]$n
  pred_area_list[[j]]$pred_prop_lower         <- pred_area_list[[j]]$cellpred_n_lower / pred_area_list[[j]]$n
  pred_area_list[[j]]$pred_prop_upper         <- pred_area_list[[j]]$cellpred_n_upper / pred_area_list[[j]]$n
  pred_area_list[[j]]$pred_per                <- pred_area_list[[j]]$pred_prop*100
  pred_area_list[[j]]$moe                     <- (pred_area_list[[j]]$pred_prop_upper - pred_area_list[[j]]$pred_prop_lower)/2
  
  # 14.2 Merge with key
  names(pred_area_list[[j]])[1] <- column_name
  pred_area_list[[j]] <- base::merge(key, pred_area_list[[j]], by=column_name, all=TRUE)
  
  # 14.3 Calculate total (national) predictions
  pred_total_list[[j]]                         <- aggregate(df2[,c("n", "cellpred_n", "cellpred_n_lower", "cellpred_n_upper")], list(df2[,col_vars[j]]), FUN=sum, na_rm=TRUE)
  pred_total_list[[j]]$pred_prop               <- pred_total_list[[j]]$cellpred_n / pred_total_list[[j]]$n
  pred_total_list[[j]]$pred_prop_lower         <- pred_total_list[[j]]$cellpred_n_lower / pred_total_list[[j]]$n
  pred_total_list[[j]]$pred_prop_upper         <- pred_total_list[[j]]$cellpred_n_upper / pred_total_list[[j]]$n
  pred_total_list[[j]]$pred_per                <- pred_total_list[[j]]$pred_prop*100
  pred_total_list[[j]]$moe                     <- (pred_total_list[[j]]$pred_prop_upper - pred_total_list[[j]]$pred_prop_lower)/2
}
names(pred_area_list) <- cols2
names(pred_total_list) <- col_vars

#### 15. Return output ####
pred <- list(df2, pred_area)
data_list <- list(pred, pred_total_list, pred_area_list)
