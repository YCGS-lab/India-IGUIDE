rm(list=ls())
library(openxlsx)
library(tidyverse)
library(sf)
library(ggtext)
library(stringr)
library(ggplot2)
library(merTools)

# Function to calculate the mean of a data vector given indices
boot_mean <- function(data, indices) {
  d <- data[indices] # Select the data subset using the current bootstrap indices
  return(mean(d, na.rm = TRUE)) # Calculate the statistic (e.g., mean)
}

# Set parameters
demo_vars <- c("gender", "age", "caste", "urban") 
col_vars <- demo_vars
if(length(demo_vars)>1){
  for(k in 2:length(demo_vars)){
    temp_vars <- paste0(utils::combn(demo_vars, k, paste0, collapse = "_"))
    col_vars <- c(col_vars, temp_vars)
  }
}

column_name <- "state_dist_code"
# target_time <- 2024
xsim=FALSE

# Load File Paths
github <- paste0("~/GitHub/ypccc_india/india_w1.5/")
dropbox <- "~/Dropbox (YSE)/ypcccdb/downscale/india/"
outdir <- "~/YSE Dropbox/Emily Goddard/India-IGUIDE/output_flood/"
temp <- paste0(github,"temp/")
xwalks <- paste0(temp,"xwalks/")

# Load Files
# xwalk_state <- read_excel(paste0(xwalks,"xwalk_state.xlsx"))
load("~/GitHub/ypccc_us/_data/xwalks/india/output/xwalk_full.rda")
key <- read.xlsx(paste0(xwalks,"xwalk_district.xlsx"))
model <- readRDS("~/Downloads/best_model_03112026_withoutGSD.rds")
# model <- readRDS("~/GitHub/India-IGUIDE/scripts/best_model_03052026.rds")
# poll <- read.csv("~/GitHub/ypccc_india/india_w1/iguide/datafiles/Poll_Severe_floods.csv")
poll <- read.csv("~/Downloads/RAP_Flood_merged_all_updated_20260227.csv")
poll2 <- read.csv("~/GitHub/ypccc_india/india_w1/iguide/datafiles/03052026/Severe_floods.csv")
df <- read.csv("~/Downloads/district_wise_covars_flood.csv")
load("~/GitHub/ypccc_india/india_w1.5/temp/df_district.rda")
# df1 <- read.csv("~/GitHub/ypccc_india/india_w1/iguide/datafiles/Severe_floods.csv")
# df_model <- read.csv("~/GitHub/ypccc_india/india_w1/iguide/datafiles/final_data_for_modelling.csv")
# covars2 <- read.csv("~/Downloads/covariates.csv")
# covars <- read.csv("~/Downloads/best_covariates_without_GSD.csv")
df2 <- read.csv("~/Downloads/district_level_spatial_features_covariates.csv")

# Merge polls
samecols <- colnames(poll)[colnames(poll) %in% colnames(poll2)]
samecols <- samecols[samecols!="caseid"]
poll <- poll[, !colnames(poll) %in% samecols]
poll2 <- poll2[!duplicated(poll2$caseid),]
poll <- base::merge(poll, poll2, by="caseid", all.x = TRUE)
# write.csv(poll, file="~/Downloads/poll_flood_updated_03052026.csv")

df <- base::merge(df, df2, by="di_code",all=TRUE)

df$di_code <- ifelse(nchar(df$di_code)==1, paste0("00",df$di_code),
                     ifelse(nchar(df$di_code)==2, paste0("0",df$di_code), as.character(df$di_code)))

setdiff(key$district_shape23_code, df$di_code)
setdiff(df$di_code, key$district_shape23_code)

setdiff(df_district$state_dist_code, key$state_dist_code)
setdiff(key$state_dist_code, df_district$state_dist_code)

colnames(df)[colnames(df)=="N"] <- "count"

# Merge Data
df <- base::merge(df, key, by.x="di_code",by.y="district_shape23_code", all.y=TRUE)
df <- base::merge(df, df_district, by=c("state_dist_code","state_code","district_code"), all.x=TRUE)

# covars <- trimws(covars$features)
# covars <- trimws(covars$Feature)

df_orig <- df
df <- df %>%
  dplyr::select(all_of(model$var.names))

# GFD_sum, lag_GFD_sum, lisa_I_GFD, lisa_GFD_high_high, lisa_GFD_low_high, 
# lisa_GFD_low_low, lisa_GFD_high_low, lisa_GFD_not_sig, 

# rm(df_district, df2)

#### 0. Set seed ####
set.seed(2496)

#### 1. Add time and mode to dataframe ####
# df$year <- target_time

#### 2. List codes ####
geocode <- levels(as.factor(df$GEOID))
# df <- df %>%
#   dplyr::select(-state_shape23, -state_shape23_code, -state_census11, 
#                 -district_census11, -state_district_census11, -state_census11_code, 
#                 -district_census11_code, -district_shape23, -state_district_shape23, 
#                 -zone, -state_dist, -district, -state, -country)
# df_model <- as.matrix(df)

#### 3. Create new prediction code using new predict() functionality for merMod objects ####
cellpred <- predict(model,df,type="response",allow.new.levels=TRUE, params = list(predict_disable_shape_check = TRUE))



#### 4. Weight prediction by frequency of cell (not currently used) ####
cellpredweighted <- cellpred*df_orig$n_pct_geo

#### 5. Calculate n for each cell ####
cellpred_n <- cellpred*df_orig$n

#### 6. Create blank confidence intervals ####
cellpred_n_lower <- 0
cellpred_n_upper <- 0

#### 7. Use simulation to estimate confidence intervals ####
if(xsim==TRUE){
  # split data frame into chunks of 999 rows--this is needed for predictInterval to work properly in its current version 
  n.splits <- ceiling(length(df_orig$n)/999)
  df.split <- suppressWarnings(split(df_orig, rep(1:n.splits,each=999)))
  for(s in 1:n.splits){
    if(s==1){
      cellpred.conf <- suppressWarnings(predictInterval(model, newdata = df.split[[s]], n.sims = n_sims, stat='mean', type='probability', include.resid.var = FALSE, which=c("full")))
    } else {
      cellpred.conf.temp <- suppressWarnings(predictInterval(model, newdata = df.split[[s]], n.sims = n_sims, stat='mean', type='probability', include.resid.var = FALSE, which=c("full")))
      cellpred.conf <- rbind(cellpred.conf, cellpred.conf.temp)
    }
  }
  
  cellpred_n_lower <- cellpred.conf$lwr*df_orig$n
  cellpred_n_upper <- cellpred.conf$upr*df_orig$n
}

#### 8. Calculate national total n and proportion ####
pred.total <- data.frame(sum(df_orig$n, na.rm=TRUE), 
                         sum(cellpred_n, na.rm=TRUE), 
                         sum(cellpred_n, na.rm=TRUE)/sum(df_orig$n, na.rm=TRUE), 
                         sum(cellpred_n_lower, na.rm=TRUE)/sum(df_orig$n, na.rm=TRUE), 
                         sum(cellpred_n_upper, na.rm=TRUE)/sum(df_orig$n, na.rm=TRUE))
names(pred.total) <- c("national.n", "national.pred.n", "national.pred.prop", "national.pred.lower", "national.pred.upper")

#### 9. Combine to dataframe ####
df2 <- cbind(df_orig, cellpredweighted, cellpred, cellpred_n) #, cellpred_n_lower, cellpred_n_upper

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
pred_area                 <- aggregate(df2[,c("n", "cellpred_n")], list(df2$GEOID), FUN=sum, na.rm=TRUE) #"cellpred_n_lower", "cellpred_n_upper"
pred_area$pred_prop       <- pred_area$cellpred_n / pred_area$n
# pred_area$pred_prop_lower <- pred_area$cellpred_n_lower / pred_area$n
# pred_area$pred_prop_upper <- pred_area$cellpred_n_upper / pred_area$n
pred_area$pred_per        <- pred_area$pred_prop*100
# pred_area$moe             <- (pred_area$pred_prop_upper - pred_area$pred_prop_lower)/2

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
  pred_area_list[[j]]                         <- aggregate(df2[,c("n", "cellpred_n")], list(df2[,colnames(df2)==column_name], df2[,cols2[j]]), FUN=sum, na.rm=TRUE) #, "cellpred_n_lower", "cellpred_n_upper"
  pred_area_list[[j]]$pred_prop               <- pred_area_list[[j]]$cellpred_n / pred_area_list[[j]]$n
  # pred_area_list[[j]]$pred_prop_lower         <- pred_area_list[[j]]$cellpred_n_lower / pred_area_list[[j]]$n
  # pred_area_list[[j]]$pred_prop_upper         <- pred_area_list[[j]]$cellpred_n_upper / pred_area_list[[j]]$n
  pred_area_list[[j]]$pred_per                <- pred_area_list[[j]]$pred_prop*100
  # pred_area_list[[j]]$moe                     <- (pred_area_list[[j]]$pred_prop_upper - pred_area_list[[j]]$pred_prop_lower)/2
  
  # 14.2 Merge with key
  names(pred_area_list[[j]])[1] <- column_name
  pred_area_list[[j]] <- base::merge(key, pred_area_list[[j]], by=column_name, all=TRUE)
  
  # 14.3 Calculate total (national) predictions
  pred_total_list[[j]]                         <- aggregate(df2[,c("n", "cellpred_n")], list(df2[,col_vars[j]]), FUN=sum, na_rm=TRUE) #, "cellpred_n_lower", "cellpred_n_upper"
  pred_total_list[[j]]$pred_prop               <- pred_total_list[[j]]$cellpred_n / pred_total_list[[j]]$n
  # pred_total_list[[j]]$pred_prop_lower         <- pred_total_list[[j]]$cellpred_n_lower / pred_total_list[[j]]$n
  # pred_total_list[[j]]$pred_prop_upper         <- pred_total_list[[j]]$cellpred_n_upper / pred_total_list[[j]]$n
  pred_total_list[[j]]$pred_per                <- pred_total_list[[j]]$pred_prop*100
  # pred_total_list[[j]]$moe                     <- (pred_total_list[[j]]$pred_prop_upper - pred_total_list[[j]]$pred_prop_lower)/2
}
names(pred_area_list) <- cols2
names(pred_total_list) <- col_vars

#### 15. Return output ####
pred <- list(df2, pred_area)
data_list <- list(pred, pred_total_list, pred_area_list)

areapred <- data_list[[1]][[2]]
pred_total_demo <- data_list[[2]]
pred_area_demo <- data_list[[3]]

#### 1. Sum at national level for difference plots ####
national_n                                <- sum(areapred$n, na.rm=TRUE)
national_cellpred_n                       <- sum(areapred$cellpred_n, na.rm=TRUE)
national_prop                             <- national_cellpred_n / national_n
national_per                              <- national_prop*100
# national_pred_prop_upper                  <- sum(areapred$pred_prop_upper, na.rm=TRUE)
# national_pred_prop_lower                  <- sum(areapred$pred_prop_lower, na.rm=TRUE)
# national_moe                              <- (national_pred_prop_upper - national_pred_prop_lower)/2
areapred$national_per                     <- national_per
areapred$pred_prop[areapred$pred_prop==0] <- national_prop

#### 2. Create national dataframe ####
natpred <- cbind("us",national_n,national_cellpred_n,national_prop,national_per) #,national_pred_prop_upper,national_pred_prop_lower,national_moe
colnames(natpred) <- c("country","n","cellpred_n","pred_prop","pred_per") #,"pred_prop_upper","pred_prop_lower","moe"

#### 3. Calculate difference from national average for each area ####
areapred$pred_per_diff <- areapred$pred_per - national_per

for(i in 1:length(pred_area_demo)){
  pred_area_demo[[i]]$temp <- gsub("[0-9]._", "",pred_area_demo[[i]]$Group.2)
  for(j in 1:length(unique(pred_total_demo[[i]]$Group.2))){
    predvar <- pred_total_demo[[i]]$Group.2[j]
    pred_area_demo[[i]]$pred_per_diff[pred_area_demo[[i]]$temp==predvar] <- pred_area_demo[[i]]$pred_per[pred_area_demo[[i]]$temp==predvar] - pred_total_demo[[i]]$pred_per[pred_total_demo[[i]]$Group.2==predvar]
  }
  pred_area_demo[[i]]$temp <- NULL
}

areapred_write <- areapred %>%
  dplyr::mutate(GeoName = display_name,
                GeoID = state_dist_code,
                state = str_to_title(state_shape23)) %>%
  dplyr::select(GeoName, GeoID, state, zone, n, cellpred_n, pred_prop, pred_per, national_per, pred_per_diff)

#### 4. Save geography files ####
if(dir.exists(paste0(outdir))==FALSE){
  dir.create(paste0(outdir), recursive=TRUE)
}
# write.csv(areapred_write, file=paste0(outdir,"flood_table.csv"), row.names = FALSE)
# save(areapred_write, file=paste0(outdir,"flood_table.Rda"))

#### 5. Save country files ####
# if(level=="state"){
#   if(dir.exists(paste0(outdir,"country/", var, "/"))==FALSE){
#     dir.create(paste0(outdir,"country/", var, "/"), recursive=TRUE)
#   }
#   write.csv(natpred, file=paste0(outdir,"country/", var, "/", var, "_country_", time, "_table.csv"), row.names = FALSE)
#   save(natpred, file=paste0(outdir,"country/", var, "/",var,"_country_", time, ".Rda"))
# }

#### 6. Save demographic breakdowns ####
# for(i in 1:length(pred_area_demo)){
#   demo <- names(pred_total_demo)[i]
#   write.csv(pred_area_demo[[i]], file=paste0(outdir,"tables/",demo,"_flood_table.csv"), row.names = FALSE)
# }
# for(i in 1:length(pred_total_demo)){
#   demo <- names(pred_total_demo)[i]
#   write.csv(pred_total_demo[[i]], file=paste0(outdir,level,"/",var,"/",var,"_country_",demo,"_",time,"_table.csv"), row.names = FALSE)
# }

#==============================================================================#
# 1.0 Set Map Parameters
#==============================================================================#
# Lines between sub-country geographies
linecol   <- "white"
linewidth <- 0.1

# Legend breaks and colors
breaks <- seq(0,100,5)
legendlab <- paste0("Estimated % of population, 2011")
map.palette <- rev(c("(95,100]"="#450847",
                     "(90,95]"="#6e123d",
                     "(85,90]"="#921c33",
                     "(80,85]"="#bf272a",
                     "(75,80]"="#cb612e",
                     "(70,75]"="#ed914c",
                     "(65,70]"="#f1ab62",
                     "(60,65]"="#f4c579",
                     "(55,60]"="#f7d990",
                     "(50,55]"="#fdeca7",
                     "(45,50]"="#e1ebf6",
                     "(40,45]"="#becee3",
                     "(35,40]"="#9eb1d0",
                     "(30,35]"="#8098bd",
                     "(25,30]"="#667eac",
                     "(20,25]"="#4e669a",
                     "(15,20]"="#395188",
                     "(10,15]"="#283e75",
                     "(5,10]"="#1a2b65",
                     "(0,5]"="#0c1b54"))

#==============================================================================#
# 2.0 Load the Data
#==============================================================================#
# Stacked Data
district <- areapred %>%
  dplyr::rename(ShapeName = state_district_shape23) %>%
  # dplyr::mutate(ShapeID = paste0("x",state_dist_code)) %>%
  dplyr::select(ShapeName, n, cellpred_n, pred_prop, pred_per, national_per, pred_per_diff)

# Long Data
# df_downscale <- read.csv(paste0(tables,"india_downscale",wave,"_",year,"_fullstack_long_",nvars,".csv"))
# district2  <- df_downscale[df_downscale$GeoType=="district",]

# Load the shapefiles that Martial is using:
shape_district <- read_sf(dsn=paste0("~/GitHub/ypccc_us/_data/external/india/shapefiles/2023/output"), layer=paste0("district_state_shapefile_2023"))



df_pop <- df_district %>%
  dplyr::ungroup() %>%
  dplyr::select(state_code, district_code, N) %>%
  dplyr::distinct()

xwalk_pop <- xwalk %>% 
  dplyr::mutate(state_code = ifelse(state_shape23=="TELANGANA", "36", state_census11_code), 
                district_code = district_census11_code) %>%
  dplyr::select(state_census11, district_census11, state_district_census11, 
                state_code, district_code, state_shape23, state_shape23_code, 
                district_shape23, district_shape23_code, state_district_shape23) %>%
  dplyr::distinct()

df_pop <- base::merge(df_pop,xwalk_pop, by=c("state_code", "district_code"), all=TRUE)
df_pop <- df_pop %>%
  dplyr::group_by(state_district_shape23) %>%
  dplyr::summarise(population = sum(N)) %>%
  dplyr::rename(GeoName = state_district_shape23) %>%
  dplyr::select(GeoName, population) %>%
  dplyr::distinct()

shape_di <- base::merge(shape_district, df_pop, by="GeoName", all=TRUE) %>%
  dplyr::select(GeoName, GEOID, population, state23, st_code, dist23, di_code, geometry)

write_sf(shape_di, "~/YSE Dropbox/Emily Goddard/India-IGUIDE/output_flood/shapefile/district_shapefile.shp")
#==============================================================================#
# 3.0 Reformat Shapefiles
#==============================================================================#
# District
shape_district <- shape_district %>%
  dplyr::rename(ShapeName = GeoName,
                ShapeID = GEOID) %>%
  dplyr::select(ShapeName, ShapeID, geometry)

#==============================================================================#
# 4.0 Merge Data onto Shapefiles
#==============================================================================#
# Merge Shapefiles and Data
shape_merge    <- base::merge(shape_district, district, by=c("ShapeName"), all=TRUE)

#==============================================================================#
# 4.0 Loop Over Each Question to Create Maps ###
#==============================================================================#
# Loop over geographies (country, state, district)
data <- shape_merge
geog <- "district"

# Format & Subset Data
df <- data %>%
  dplyr::mutate(pred_per = as.numeric(pred_per))

df <- df %>%
  dplyr::mutate(pred_per = as.numeric(pred_per)) %>%
  dplyr::mutate(pred_bin = cut(df$pred_per, breaks=breaks)) %>%
  dplyr::select(ShapeID, ShapeName, pred_per, pred_bin, geometry) %>% #GeoID, GeoName, Population, 
  dplyr::distinct()

nat_avg <- round(mean(df$pred_per, na.rm=TRUE),0)

# Create secondary dataframe to view all legend options
df2 <- df[1,c("ShapeID","ShapeName","pred_bin","geometry")]
df2 <- df2[rep(seq_len(nrow(df2)), each = 20),]
df2$pred_bin[1:20] <- c("(95,100]","(90,95]","(85,90]","(80,85]","(75,80]",
                        "(70,75]","(65,70]","(60,65]","(55,60]","(50,55]",
                        "(45,50]","(40,45]","(35,40]","(30,35]","(25,30]",
                        "(20,25]","(15,20]","(10,15]","(5,10]","(0,5]")

# Find centroids for placing text (rounded percents)
df3 <- sf::st_centroid(df)
df3 <- data.frame(sf::st_coordinates(df3))
df3 <- df3 %>% 
  dplyr::select(X, Y)
df3$pred_per <- df$pred_per
df3$ShapeName <- df$ShapeName
df3$label <- round(df3$pred_per,0)

# Identify title & text
plot_title <- paste0("Worry about severe floods (National Average: ",nat_avg,"%)")
plot_text <- paste0(lapply(strwrap("How worried are you that severe floods might harm your local area? are you very worried, moderately worried, not very worried or not at all worried?",width=100,
                                   simplify=FALSE), paste, collapse="<br>"),
                    collapse = "<br>")

# Set output file path
outpath  <- paste0("~/YSE Dropbox/Emily Goddard/India-IGUIDE/output_flood/district_flood_worry.png")

# Create Map
p <- ggplot(data)+
  geom_sf(data=df2, aes(fill=pred_bin), color=linecol, size=linewidth)+
  geom_sf(data=df, aes(fill=pred_bin), color=linecol, size=linewidth)+
  scale_fill_manual(values=map.palette, name=legendlab, na.value="darkgray", drop=FALSE)+
  # geom_text(data=df3, aes(X,Y,label=label), size=1, color=ifelse((as.numeric(df3$pred_per)<25|as.numeric(df3$pred_per)>85)&!grepl("Islands|Lakshadweep",df3$GeoName), "white", "black"))+
  guides(fill=guide_legend(title.position="top", keyheight=.75, label.hjust=(0)))+
  coord_sf()+
  theme_void()+
  labs(x="",y="", title=plot_title, caption=plot_text)+
  theme(plot.title = element_text(size=20, face = "bold", hjust = 0), 
        legend.position="right", 
        legend.title = element_text(size=16),
        legend.text = element_text(size=12),
        plot.caption= element_markdown(size=14, hjust = 0,lineheight = 1.2),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

# Print Map
# png(file=outpath, res=300, width=12.5, height=10, units="in")
# print(p)
# dev.off()  

#==============================================================================#
# 5.0 Loop Over Each Question to Create Difference Maps ###
#==============================================================================#
# # Format & Subset Data
# df <- data %>%
#   dplyr::mutate(pred_per_diff = as.numeric(pred_per_diff))
# 
# df <- df %>%
#   dplyr::mutate(pred_per_diff = as.numeric(pred_per_diff)) %>%
#   dplyr::mutate(pred_bin = cut(df$pred_per_diff, breaks=breaks)) %>%
#   dplyr::select(ShapeID, ShapeName, pred_per_diff, pred_bin, geometry) %>% #GeoID, GeoName, Population, 
#   dplyr::distinct()
# 
# # Create secondary dataframe to view all legend options
# df2 <- df[1,c("ShapeID","ShapeName","pred_bin","geometry")]
# df2 <- df2[rep(seq_len(nrow(df2)), each = 20),]
# df2$pred_bin[1:20] <- c("(95,100]","(90,95]","(85,90]","(80,85]","(75,80]",
#                         "(70,75]","(65,70]","(60,65]","(55,60]","(50,55]",
#                         "(45,50]","(40,45]","(35,40]","(30,35]","(25,30]",
#                         "(20,25]","(15,20]","(10,15]","(5,10]","(0,5]")
# 
# # Find centroids for placing text (rounded percents)
# df3 <- sf::st_centroid(df)
# df3 <- data.frame(sf::st_coordinates(df3))
# df3 <- df3 %>% 
#   dplyr::select(X, Y)
# df3$pred_per_diff <- df$pred_per_diff
# df3$ShapeName <- df$ShapeName
# df3$label <- round(df3$pred_per_diff,0)
# 
# # Identify title & text
# plot_title <- paste0("Worry about severe floods - difference from national average (National Average: ",nat_avg,"%)")
# plot_text <- paste0(lapply(strwrap("How worried are you that severe floods might harm your local area? are you very worried, moderately worried, not very worried or not at all worried?",width=100,
#                                    simplify=FALSE), paste, collapse="<br>"),
#                     collapse = "<br>")
# 
# # Set output file path
# outpath  <- paste0("~/YSE Dropbox/Emily Goddard/India-IGUIDE/output_flood/maps/district_flood_worry.png")
# 
# # Create Map
# p <- ggplot(data)+
#   geom_sf(data=df2, aes(fill=pred_bin), color=linecol, size=linewidth)+
#   geom_sf(data=df, aes(fill=pred_bin), color=linecol, size=linewidth)+
#   scale_fill_manual(values=map.palette, name=legendlab, na.value="darkgray", drop=FALSE)+
#   guides(fill=guide_legend(title.position="top", keyheight=.75, label.hjust=(0)))+
#   coord_sf()+
#   theme_void()+
#   labs(x="",y="", title=plot_title, caption=plot_text)+
#   theme(plot.title = element_text(size=20, face = "bold", hjust = 0), 
#         legend.position="right", 
#         legend.title = element_text(size=16),
#         legend.text = element_text(size=12),
#         plot.caption= element_markdown(size=14, hjust = 0,lineheight = 1.2),
#         plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
# 
# # Print Map
# png(file=outpath, res=300, width=12.5, height=10, units="in")
# print(p)
# dev.off()  

#==============================================================================#
# END OF FILE
#==============================================================================#
# Loop over each geography level
# poll$year <- as.numeric(poll$year)
geo <- "state_district_survey"
geo2 <- "district"

# Calculate the share of each response for each question
df1 <- poll %>%
  dplyr::select(!!sym(geo), weight, n7fy23_recode, wave) %>%
  dplyr::filter(!is.na(n7fy23_recode))

df1 <- df1 %>% 
  dplyr::mutate(count = 1) %>% 
  dplyr::group_by(!!sym(geo), n7fy23_recode) %>%
  dplyr::summarise(freq = sum(weight), n_obs = sum(count)) %>%
  dplyr::mutate(n_wgt = sum(freq),
                prop  = freq/n_wgt,
                pct   = prop*100,
                n_obs = sum(n_obs)) %>% 
  dplyr::filter(n_obs>50) %>%
  dplyr::ungroup()

df1 <- df1 %>% 
  dplyr::filter(n7fy23_recode==1) %>% 
  dplyr::select(state_district_survey, n_obs, freq, n_wgt, prop, pct)

length(unique(df1$state_district_survey))
for(i in 1:ncol(key)){
  print(colnames(key)[i])
  print(length(unique(key[,i])))
}

areapred <- base::merge(areapred, xwalk, by=c("state_shape23", "state_shape23_code", "state_census11", "district_census11", 
                                              "state_district_census11", "state_census11_code", "district_census11_code", 
                                              "district_shape23", "district_shape23_code", "state_district_shape23", 
                                              "zone"))

# areapred$state_district_survey <- str_to_lower(areapred$state_district_survey)

qa_df_di <- base::merge(areapred, df1, by="state_district_survey", all.y=TRUE)
qa_df_di <- na.omit(qa_df_di)
qa_df_di$difference <- round(qa_df_di$pct - qa_df_di$pred_per,2)

qa_df_di <- qa_df_di %>%
  dplyr::mutate(survey_pct = round(pct,2),
                estimate = round(pred_per,2),
                survey_pop = n_obs,
                abs_difference = round(abs(difference),2)) %>%
  dplyr::select(state_district_survey, survey_pop, survey_pct, estimate, difference, abs_difference) %>%
  dplyr::distinct()

vect <- data.frame(matrix(nrow=1,ncol=ncol(qa_df_di)))
colnames(vect) <- colnames(qa_df_di)
vect[,colnames(vect)=="abs_difference"] <- round(mean(qa_df_di[,colnames(qa_df_di)=="abs_difference"],na.rm=TRUE),2)
vect$state_district_survey <- "Mean Absolute Error"
qa_df_di <- rbind(qa_df_di, vect)


# STATE QA
# Loop over each geography level
# poll$year <- as.numeric(poll$year)
geo <- "state"
geo2 <- "state"

# Calculate the share of each response for each question
df1 <- poll %>%
  dplyr::select(!!sym(geo), weight, n7fy23_recode, wave) %>%
  dplyr::filter(!is.na(n7fy23_recode))

df1 <- df1 %>% 
  dplyr::mutate(count = 1) %>% 
  dplyr::group_by(!!sym(geo), n7fy23_recode) %>%
  dplyr::summarise(freq = sum(weight), n_obs = sum(count)) %>%
  dplyr::mutate(n_wgt = sum(freq),
                prop  = freq/n_wgt,
                pct   = prop*100,
                n_obs = sum(n_obs)) %>% 
  dplyr::ungroup()

df1 <- df1 %>% 
  dplyr::filter(n7fy23_recode==1) %>% 
  dplyr::select(state, n_obs, freq, n_wgt, prop, pct) %>%
  dplyr::filter(n_obs>500)

length(unique(df1$state))
for(i in 1:ncol(key)){
  print(colnames(key)[i])
  print(length(unique(key[,i])))
}


areapred_st <- areapred %>%
  dplyr::group_by(state) %>%
  dplyr::summarise(pred_per = mean(pred_per))

qa_df_st <- base::merge(areapred_st, df1, by="state", all.y=TRUE)
qa_df_st$difference <- round(qa_df_st$pct - qa_df_st$pred_per,2)

qa_df_st <- qa_df_st %>%
  dplyr::mutate(survey_pct = round(pct,2),
                estimate = round(pred_per,2),
                survey_pop = n_obs,
                abs_difference = abs(difference)) %>%
  dplyr::select(state, survey_pop, survey_pct, estimate, difference, abs_difference)

vect <- data.frame(matrix(nrow=1,ncol=ncol(qa_df_st)))
colnames(vect) <- colnames(qa_df_st)
vect[,colnames(vect)=="abs_difference"] <- round(mean(qa_df_st[,colnames(qa_df_st)=="abs_difference"],na.rm=TRUE),2)
vect$state <- "Mean Absolute Error"
qa_df_st <- rbind(qa_df_st, vect)


# Calculate the share of each response for each question
df_ct <- poll %>%
  dplyr::select(weight, n7fy23_recode, wave) %>%
  dplyr::filter(!is.na(n7fy23_recode))

df_ct <- df_ct %>% 
  dplyr::mutate(count = 1) %>% 
  dplyr::group_by(n7fy23_recode) %>%
  dplyr::summarise(freq = sum(weight), n_obs = sum(count)) %>%
  dplyr::mutate(n_wgt = sum(freq),
                prop  = freq/n_wgt,
                pct   = prop*100,
                n_obs = sum(n_obs)) %>% 
  dplyr::ungroup()

df_ct <- df_ct %>% 
  dplyr::filter(n7fy23_recode==1) %>% 
  dplyr::select(n_obs, freq, n_wgt, prop, pct)

areapred_ct <- areapred %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(pred_per = mean(pred_per))

qa_df_ct <- cbind(areapred_ct, df_ct)

qa_df_ct$difference <- round(qa_df_ct$pct - qa_df_ct$pred_per,2)

qa_df_ct <- qa_df_ct %>%
  dplyr::mutate(survey_pct = round(pct,2),
                estimate = round(pred_per,2),
                survey_pop = n_obs,
                abs_difference = abs(difference)) %>%
  dplyr::select(country, survey_pop, survey_pct, estimate, difference, abs_difference)

# write.csv(qa_df_di, file=paste0(outdir,"qa_district.csv"))
# write.csv(qa_df_st, file=paste0(outdir,"qa_state.csv"))
# write.csv(qa_df_ct, file=paste0(outdir,"qa_country.csv"))



# 
# india <- readRDS("~/YSE Dropbox/Emily Goddard/ypcccdb/_data/surveys/india/cvoter2025/output/combined_full_india_cvoter_w01-w04_2025.rds")
# 
# india_2024 <- india[india$wave=="2024",]
# india_2023 <- india[india$wave=="2023",]
# 
# prop.table(table(india_2024$n7fy23))*100
# prop.table(table(india_2023$n7fy23))*100
# 
# 
# india <- base::merge(india, poll, by=c("caseid","wave"), all.y=TRUE)


