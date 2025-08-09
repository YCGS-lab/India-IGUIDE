
import os, glob, subprocess, json
import geopandas as gpd
from exactextract import exact_extract

EMBEDDINGS_PATH = r'/anvil/projects/x-cis250634/team1/GoogleEmbeddings2024/'
TEMP_PATH = r'/anvil/scratch/x-ptripathy/temp/'
os.makedirs(TEMP_PATH, exist_ok=True)

# Load the district boundaries file
districts_path = "/anvil/projects/x-cis250634/team1/data/India_District_2023_3857.gpkg"
dist_gdf = gpd.read_file(districts_path)

# clean the district code column
dist_gdf['di_code'] = dist_gdf['di_code'].fillna(-1).astype(int)


# Load the satellite embeddings footprints
footprint_path = "/anvil/projects/x-cis250634/team1/data/SatelliteEmbeddings_footprints_3857.gpkg"
footprint_gdf = gpd.read_file(footprint_path)

# Check CRS compatibility
print("=== CRS Check ===")
if dist_gdf.crs == footprint_gdf.crs:
    print(f"Districts CRS: {dist_gdf.crs}")
    print(f"Footprints CRS: {footprint_gdf.crs}")
    print(f"CRS match: {dist_gdf.crs == footprint_gdf.crs}")

    # loop through the districts
    for dist_code in [532]:  # dist_gdf['di_code'].unique():
        current_dist = dist_gdf.loc[dist_gdf['di_code'] == dist_code].dissolve()

    # find the footprints that intersect with this district
    intersecting_footprints = footprint_gdf[footprint_gdf.intersects(current_dist.geometry.iloc[0])]

    # find the tif files corresponding to the footprints
    intersecting_tif_files_list = [
        f"{EMBEDDINGS_PATH}{os.path.split(file)[-1].replace('_footprint.gpkg', '.tif')}"
        for file in list(intersecting_footprints['source'].values)
    ]

    # use GDAL to mosaic the tif files that intersect with this district
    # Output paths
    vrt_path = f'{TEMP_PATH}district_{dist_code}_merged.vrt'
    merged_tif_path = f'{TEMP_PATH}district_{dist_code}_mosaiced.tif'

    # Build VRT
    subprocess.run(['gdalbuildvrt', vrt_path] + intersecting_tif_files_list, check=True)

    # Translate to compressed GeoTIFF
    subprocess.run([
        'gdal_translate',
        '-of', 'GTiff',
        '-co', 'COMPRESS=LZW',
        '-co', 'NUM_THREADS=ALL_CPUS',
        '-co', 'TILED=YES',
        vrt_path,
        merged_tif_path
    ], check=True)

    # run zonal statistics
    zs_result = exact_extract(merged_tif_path, current_dist, ['sum', 'median'])

    # export zonal stats results
    json_path = f'{TEMP_PATH}district_{dist_code}_ZonalStats.json'
    with open(json_path, 'w') as f:
        json.dump(json_path[0], f, indent=4)

else:
    print(f"CRS match failed!")
