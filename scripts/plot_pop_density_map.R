# India estimated population
# Source: Facebook; Contributor: AI and Data for Good at Meta
# https://data.humdata.org/dataset/pakistan-india_all-files-high-resolution-population-density-maps
# Dataset date: March 2021
# Citation: "Facebook Connectivity Lab and Center for International Earth Science Information Network - CIESIN - Columbia University. 2016. High Resolution Settlement Layer (HRSL). Source imagery for HRSL © 2016 DigitalGlobe. Accessed 3 Jan 2026."
# R script: open + map 10x10° population GeoTIFF tiles (or the provided VRT)
# Packages: terra for rasters; ggplot2 for plotting; sf for borders (optional)
# Data are in YCGS_projects/DataTools/MetaYPCCC/India/population_ind_pak_women'

# install.packages(c("terra","ggplot2","sf","rnaturalearth","rnaturalearthdata"))
library(terra)
library(ggplot2)
library(sf)
library(rnaturalearth)

# ---- 1) Point this to your folder ----
root <- "~/YSE Dropbox/Jennifer Marlon/YCGS_projects/DataTools/MetaYPCCC/"
data_dir <- paste0(root, "India/population_ind_pak_women")  # <-- CHANGE ME

# If you have a VRT, use it (best option: no manual mosaicking)
vrt_path <- file.path(data_dir, "your_population_tiles.vrt")  # <-- CHANGE NAME

# ---- 2) Load raster (prefer VRT; else mosaic all TIFF tiles) ----
if (file.exists(vrt_path)) {
  pop <- rast(vrt_path)
} else {
  tif_files <- list.files(data_dir, pattern = "\\.tif(f)?$", full.names = TRUE)
  stopifnot(length(tif_files) > 0)
  
  # Read as a SpatRaster collection, then merge into a mosaic
  tiles <- lapply(tif_files, rast)
  
  # If tiles overlap, you may want fun="mean" or "max"; most tiles won't overlap.
  pop <- do.call(merge, tiles)
}

# If need actual mosaicked file and not VRT can do this in bash:
# gdal_merge.py -o pop_mosaic.tif *.tif

# Optional: inspect
print(pop)
# plot(pop) # quick base plot

# ---- 3) Crop to Pakistan + India bounding box (optional, speeds plotting) ----
# Rough bbox covering Pakistan + India (adjust if your tiles cover a slightly different extent)
roi <- ext(60, 100, 5, 40)  # xmin, xmax, ymin, ymax (lon/lat)
pop_roi <- crop(pop, roi)
# OR reproject...
# pop_ll <- project(pop, "EPSG:4326")


# ---- 4) Prepare a fast-to-plot version (optional but recommended) ----
# (a) If values are huge / skewed, log-transform for visualization
# Use log1p to handle zeros safely.
pop_plot <- log1p(pop_roi)

# (b) Downsample for plotting if very large
# Keep ~1500 columns (tweak); this does NOT change original data.
pop_plot <- aggregate(pop_plot, fact = max(1, round(ncol(pop_plot) / 1500)), fun = mean, na.rm = TRUE)

# ---- 5) Convert to data.frame for ggplot ----
df <- as.data.frame(pop_plot, xy = TRUE, na.rm = TRUE)
names(df) <- c("lon", "lat", "value")

# ---- 6) Add country outlines (optional) ----
countries <- ne_countries(scale = "medium", returnclass = "sf")
countries_sub <- countries[countries$admin %in% c("Pakistan", "India"), ]

# ---- 7) Map ----
pA <- ggplot() +
  geom_raster(data = df, aes(x = lon, y = lat, fill = value)) +
  geom_sf(data = countries_sub, fill = NA, linewidth = 0.5) +
  coord_sf(xlim = c(60, 100), ylim = c(5, 40), expand = FALSE) +
  scale_fill_viridis_c(
    option = "C",
    name = "log(1 + pop)",
    na.value = NA
  ) +
  labs(
    title = "Estimated Population", # (GeoTIFF tiles / VRT)",
    subtitle = "Visualization uses log1p transform for readability"
  ) +
  theme_minimal()

png("~/Documents/GitHub/India-IGUIDE/output/high-res-pop-basemap.png", width = 1800, height = 1200, res = 300)
print(pA)
dev.off()


# ---- 8) If you want the original (non-log) map, swap df creation:
# df2 <- as.data.frame(pop_roi, xy = TRUE, na.rm = TRUE)
# names(df2) <- c("lon","lat","value")
# then use df2 in geom_raster and set scale_fill_viridis_c(name="Population")
