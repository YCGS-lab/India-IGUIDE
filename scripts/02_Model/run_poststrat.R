#==============================================================================#
# run_poststrat.R ####
#==============================================================================#
### Run multilevel regression with post-stratification (MRP) 

# INPUTS:
# Crosswalk files
# Model file
# Poll file
# Covariate dataframe
# Spatial covariates dataframe
# Census counts

# OUTPUTS:
# 

#==============================================================================#
# 0.0 Setup ####
#==============================================================================#
rm(list=ls())
library(openxlsx)
library(tidyverse)
library(sf)
library(ggtext)
library(stringr)
library(ggplot2)
library(merTools)
library(viridis)

# Function to calculate the mean of a data vector given indices
boot_mean <- function(data, indices) {
  d <- data[indices] # Select the data subset using the current bootstrap indices
  return(mean(d, na.rm = TRUE)) # Calculate the statistic (e.g., mean)
}

# Load File Paths
github <- paste0("~/GitHub/India-IGUIDE/scripts/")
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
col_vars <- demo_vars
if(length(demo_vars)>1){
  for(k in 2:length(demo_vars)){
    temp_vars <- paste0(utils::combn(demo_vars, k, paste0, collapse = "_"))
    col_vars <- c(col_vars, temp_vars)
  }
}
#==============================================================================#
# 2.0 Load Files ####
#==============================================================================#
# Crosswalk files
xwalk <- read.csv(paste0(xwalk_path, "xwalk_full.csv"))
key <- read.xlsx(paste0(xwalk_path,"xwalk_district.xlsx"))

# Model file
model <- readRDS(paste0(input,"/best_model_03112026_withoutGSD.rds"))

# Poll file
poll <- read.csv(paste0(input,"/poll_flood_updated_03052026.csv"))

# Covariate dataframe
df <- read.csv(paste0(input,"/district_wise_covars_flood.csv"))

# Spatial covariates dataframe
df2 <- read.csv(paste0(input,"/district_level_spatial_features_covariates.csv"))

# Census counts
load(paste0(input,"/df_district.rda"))
     
#==============================================================================#
# 3.0 Prep and Clean Data ####
#==============================================================================#
# Merge covariate dataframes
df <- base::merge(df, df2, by="di_code",all=TRUE)

# Add leading zeros to .csv file
df$di_code <- ifelse(
  nchar(df$di_code) == 1,
  paste0("00", df$di_code),
  ifelse(
    nchar(df$di_code) == 2,
    paste0("0", df$di_code),
    as.character(df$di_code)
  )
)

# Change column name of population counts
colnames(df)[colnames(df)=="N"] <- "count"

# Merge data with crosswalk
df <- base::merge(df, key, by.x="di_code",by.y="district_shape23_code", all.y=TRUE)

# Merge covariates with census counts
df_census <- base::merge(df, df_district, by=c("state_dist_code","state_code","district_code"), all.x=TRUE)

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
if(dir.exists(paste0(outdir))==FALSE){
  dir.create(paste0(outdir), recursive=TRUE)
}
write.csv(areapred_write, file=paste0(outdir,"flood_district_table.csv"), row.names = FALSE)
save(areapred_write, file=paste0(outdir,"flood_district_table.Rda"))

### 5. Save country files ####
write.csv(natpred, file=paste0(outdir,"flood_country_table.csv"), row.names = FALSE)
save(natpred, file=paste0(outdir,"flood_country_table.Rda"))

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
png(file=outpath, res=300, width=12.5, height=10, units="in")
print(p)
dev.off()

#==============================================================================#
# 5.0 Loop Over Each Question to Create Difference Maps ###
#==============================================================================#
# Format & Subset Data
df <- data %>%
  dplyr::mutate(pred_per_diff = as.numeric(pred_per_diff))

df <- df %>%
  dplyr::mutate(pred_per_diff = as.numeric(pred_per_diff)) %>%
  dplyr::mutate(pred_bin = cut(df$pred_per_diff, breaks=breaks)) %>%
  dplyr::select(ShapeID, ShapeName, pred_per_diff, pred_bin, geometry) %>% #GeoID, GeoName, Population,
  dplyr::distinct()

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
df3$pred_per_diff <- df$pred_per_diff
df3$ShapeName <- df$ShapeName
df3$label <- round(df3$pred_per_diff,0)

# Identify title & text
plot_title <- paste0("Worry about severe floods - difference from national average (National Average: ",nat_avg,"%)")
plot_text <- paste0(lapply(strwrap("How worried are you that severe floods might harm your local area? are you very worried, moderately worried, not very worried or not at all worried?",width=100,
                                   simplify=FALSE), paste, collapse="<br>"),
                    collapse = "<br>")

# Set output file path
outpath  <- paste0("~/YSE Dropbox/Emily Goddard/India-IGUIDE/output_flood/maps/district_flood_worry.png")

# Create Map
p <- ggplot(data)+
  geom_sf(data=df2, aes(fill=pred_bin), color=linecol, size=linewidth)+
  geom_sf(data=df, aes(fill=pred_bin), color=linecol, size=linewidth)+
  scale_fill_manual(values=map.palette, name=legendlab, na.value="darkgray", drop=FALSE)+
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
png(file=outpath, res=300, width=12.5, height=10, units="in")
print(p)
dev.off()

#==============================================================================#
# END OF FILE
#==============================================================================#
# Loop over each geography level
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

write.csv(qa_df_di, file=paste0(outdir,"qa_district.csv"))
write.csv(qa_df_st, file=paste0(outdir,"qa_state.csv"))
write.csv(qa_df_ct, file=paste0(outdir,"qa_country.csv"))



colors <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "gold1", "skyblue2", "#FB9A99", "palegreen2", "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon", "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise", "green1", "yellow4", "yellow3", "darkorange4", "brown")

# Create Plots
geo <- "state_district_survey"
namecol <- "district"
df <- qa_df_di[qa_df_di$state_district_survey!="Mean Absolute Error",]
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

print(plot)

if(dir.exists(paste0(outdir,"/scatter/"))==FALSE){
  dir.create(paste0(outdir,"/scatter/"), recursive=TRUE)
}
ggsave(paste0(outdir,"/scatter/validation_plot.png"), plot, width = 6, height = 4)


