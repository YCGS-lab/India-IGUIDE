#==============================================================================#
# mapping.R ####
#==============================================================================#
### Create descriptive map of MRP results

# INPUTS:
# MRP Results
# District shapefile

# OUTPUTS:
# Map of flood worry across all districts in India

# Last updated 05/14/2026 EG
#==============================================================================#
# 0.0 Setup ####
#==============================================================================#
rm(list=ls())
library(tidyverse)
library(sf)
library(ggtext)
library(ggplot2)

# Load File Paths
dropbox <- "~/YSE Dropbox/Emily Goddard/India-IGUIDE/"
outdir <- paste0(dropbox, "output_flood/")
input <- paste0(dropbox, "_data/")

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
# Load MRP results
areapred <- readRDS(paste0(outdir,"final/flood_district_mapping.rds"))

# Load the shapefile
shape_district <- read_sf(dsn=paste0(input, "shapefiles/district"), layer=paste0("district_state_shapefile_2023"))

#==============================================================================#
# 3.0 Prep and Format Data
#==============================================================================#
# MRP Results
areapred <- areapred %>%
  dplyr::rename(ShapeName = state_district_shape23) %>%
  dplyr::select(ShapeName, n, cellpred_n, pred_prop, pred_per, national_per, pred_per_diff)

# Shapefile
shape_district <- shape_district %>%
  dplyr::rename(ShapeName = GeoName,
                ShapeID = GEOID) %>%
  dplyr::select(ShapeName, ShapeID, geometry)

# Merge Shapefiles and Data
shape_merge    <- base::merge(shape_district, areapred, by=c("ShapeName"), all=TRUE)

#==============================================================================#
# 4.0 Create Map ###
#==============================================================================#
data <- shape_merge
geog <- "district"

# Format & Subset Data
df <- data %>%
  dplyr::mutate(pred_per = as.numeric(pred_per))

df <- df %>%
  dplyr::mutate(pred_bin = cut(df$pred_per, breaks=breaks)) %>%
  dplyr::select(ShapeID, ShapeName, pred_per, pred_bin, geometry) %>% 
  dplyr::distinct()

# Identify national average
nat_avg <- round(mean(df$pred_per, na.rm=TRUE),0)

# Create secondary dataframe to view all legend options
df2 <- df[1,c("ShapeID","ShapeName","pred_bin","geometry")]
df2 <- df2[rep(seq_len(nrow(df2)), each = 20),]
df2$pred_bin[1:20] <- c("(95,100]","(90,95]","(85,90]","(80,85]","(75,80]",
                        "(70,75]","(65,70]","(60,65]","(55,60]","(50,55]",
                        "(45,50]","(40,45]","(35,40]","(30,35]","(25,30]",
                        "(20,25]","(15,20]","(10,15]","(5,10]","(0,5]")

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
png(file=paste0(outdir,"final/district_flood_worry.png"), res=300, width=12.5, height=10, units="in")
print(p)
dev.off()
#==============================================================================#
# END OF FILE
#==============================================================================#
