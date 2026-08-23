#!/usr/bin/env python3
"""
High-resolution VIIRS DNB Nighttime Lights map of India
Data: NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG (463.8m resolution)
Outputs a publication-quality raster map clipped to India boundaries.
"""

import ee
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib.patches import Patch
import geopandas as gpd
from pathlib import Path
import requests
import io
import json

# ---- Configuration ----
YEAR = 2024
SCALE = 1500  # Export resolution in meters
OUTPUT_DIR = Path(__file__).parent.parent / "figures"
OUTPUT_DIR.mkdir(exist_ok=True)

# India bounding box
INDIA_BBOX = [68.0, 6.5, 97.5, 37.5]  # [west, south, east, north]


def initialize_gee():
    """Initialize Google Earth Engine."""
    ee.Initialize(project='EnterGEEProject') # enter  GEE project name here
    print("GEE initialized successfully.")


def get_viirs_composite():
    """Get annual median VIIRS DNB composite for India."""
    # Load VIIRS DNB monthly composites
    viirs = (ee.ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG')
             .filter(ee.Filter.calendarRange(YEAR, YEAR, 'year'))
             .select('avg_rad'))  # Average radiance band

    # Annual median composite (reduces cloud/noise artifacts)
    composite = viirs.median()

    # Clip to India region
    india_geom = ee.Geometry.Rectangle(INDIA_BBOX)

    return composite.clip(india_geom), india_geom


def download_as_numpy(image, region, scale=1000):
    """Download GEE image as numpy array via getDownloadURL."""
    # Get download URL
    url = image.getDownloadURL({
        'scale': scale,
        'region': region.getInfo()['coordinates'],
        'format': 'NPY',
        'bands': ['avg_rad'],
    })

    print(f"Downloading VIIRS data at {scale}m resolution...")
    response = requests.get(url)
    data = np.load(io.BytesIO(response.content), allow_pickle=True)

    # Extract the array
    if isinstance(data, np.ndarray) and data.dtype.names:
        arr = data['avg_rad']
    else:
        arr = data

    print(f"Downloaded array shape: {arr.shape}")
    return arr


def create_ntl_colormap():
    """Create a custom colormap mimicking NASA's nighttime lights style."""
    colors = [
        (0.0, '#000000'),    # Black (no light)
        (0.01, '#0a0a2e'),   # Very dark blue
        (0.05, '#1a1a4e'),   # Dark blue
        (0.10, '#2d1b69'),   # Purple-blue
        (0.15, '#4a1a7a'),   # Purple
        (0.25, '#8b3a3a'),   # Dark red
        (0.35, '#c46210'),   # Orange
        (0.50, '#e8a735'),   # Amber
        (0.65, '#f0d060'),   # Yellow
        (0.80, '#f5e898'),   # Light yellow
        (1.0, '#ffffff'),    # White (brightest)
    ]
    positions = [c[0] for c in colors]
    hex_colors = [c[1] for c in colors]
    rgb_colors = [mcolors.hex2color(h) for h in hex_colors]
    return mcolors.LinearSegmentedColormap.from_list('ntl', list(zip(positions, rgb_colors)))


def plot_ntl_map(arr, shapefile_path=None):
    """Create publication-quality nighttime lights map."""
    from shapely.ops import unary_union
    from shapely import vectorized

    fig, ax = plt.subplots(1, 1, figsize=(12, 10), facecolor='white')
    ax.set_facecolor('white')

    # Clean the data
    arr = np.where(arr < 0, 0, arr)  # Remove negative values

    # --- Build India mask from shapefile ---
    india_mask = None
    gdf = None
    if shapefile_path and Path(shapefile_path).exists():
        gdf = gpd.read_file(shapefile_path).to_crs(epsg=4326)
        india_geom = unary_union(gdf.geometry)

        rows, cols = arr.shape
        lon = np.linspace(INDIA_BBOX[0], INDIA_BBOX[2], cols)
        lat = np.linspace(INDIA_BBOX[3], INDIA_BBOX[1], rows)
        lon_grid, lat_grid = np.meshgrid(lon, lat)

        india_mask = vectorized.contains(india_geom, lon_grid.ravel(), lat_grid.ravel())
        india_mask = india_mask.reshape(rows, cols)
        print(f"India mask: {india_mask.sum()} of {india_mask.size} pixels inside ({100*india_mask.mean():.1f}%)")

    # Log transform for better visualization (NTL data is highly skewed)
    arr_log = np.log1p(arr)

    # Clip to 99.5th percentile to handle extreme outliers
    vmax = np.nanpercentile(arr_log[arr_log > 0], 99.5)

    # Mask outside India to NaN (renders as white)
    if india_mask is not None:
        arr_log = np.where(india_mask, arr_log, np.nan)

    # Custom colormap
    cmap = create_ntl_colormap()
    cmap.set_bad(color='white')

    # Plot the raster
    extent = [INDIA_BBOX[0], INDIA_BBOX[2], INDIA_BBOX[1], INDIA_BBOX[3]]
    im = ax.imshow(arr_log, extent=extent, cmap=cmap, vmin=0, vmax=vmax,
                   interpolation='bilinear', aspect='auto')

    # Overlay boundaries
    if gdf is not None:
        country = gdf.dissolve()
        country.boundary.plot(ax=ax, linewidth=1.2, color='#888888', alpha=0.9)

        if 'STATE' in gdf.columns or 'state' in gdf.columns:
            state_col = 'STATE' if 'STATE' in gdf.columns else 'state'
            states = gdf.dissolve(by=state_col)
            states.boundary.plot(ax=ax, linewidth=0.3, color='#AAAAAA', alpha=0.5)

    # Labels and formatting
    ax.set_xlim(INDIA_BBOX[0], INDIA_BBOX[2])
    ax.set_ylim(INDIA_BBOX[1], INDIA_BBOX[3])
    ax.set_xlabel('Longitude', color='black', fontsize=10)
    ax.set_ylabel('Latitude', color='black', fontsize=10)
    ax.set_title(f'Nighttime Light Radiance Across India ({YEAR})\n'
                 f'VIIRS DNB Monthly Composites (463.8 m native resolution)',
                 color='black', fontsize=13, fontweight='bold', pad=15)

    # Style tick labels
    ax.tick_params(colors='black', labelsize=9)
    for spine in ax.spines.values():
        spine.set_color('#333333')

    # Colorbar
    cbar = fig.colorbar(im, ax=ax, fraction=0.03, pad=0.02, aspect=30)
    cbar.set_label('Log(Radiance + 1)  [nW/cm²/sr]', color='black', fontsize=10)
    cbar.ax.tick_params(colors='black', labelsize=8)
    cbar.outline.set_edgecolor('#333333')

    # Source annotation
    ax.text(0.01, 0.01,
            'Data: NOAA/VIIRS DNB Monthly V1 (VCMCFG)\n'
            'Source: Earth Observation Group, Colorado School of Mines',
            transform=ax.transAxes, fontsize=7, color='#555555',
            verticalalignment='bottom')

    plt.tight_layout()

    # Save
    out_path = OUTPUT_DIR / f'viirs_ntl_india_{YEAR}.png'
    fig.savefig(out_path, dpi=300, bbox_inches='tight',
                facecolor='white', edgecolor='none')
    print(f"Saved: {out_path}")

    out_path_pdf = OUTPUT_DIR / f'viirs_ntl_india_{YEAR}.pdf'
    fig.savefig(out_path_pdf, dpi=300, bbox_inches='tight',
                facecolor='white', edgecolor='none')
    print(f"Saved: {out_path_pdf}")

    plt.close()
    return out_path


def main():
    # Initialize GEE
    initialize_gee()

    # Get VIIRS composite
    print(f"Building {YEAR} annual VIIRS DNB composite...")
    composite, region = get_viirs_composite()

    # Download as numpy array
    arr = download_as_numpy(composite, region, scale=SCALE)

    # Find shapefile
    shapefile_candidates = [
        Path(__file__).parent.parent / "data" / "shapefiles" / "district_state_shapefile_2023.shp",
        Path(__file__).parent.parent / "data" / "raw" / "district_state_shapefile_2023.shp",
    ]
    shapefile_path = None
    for sp in shapefile_candidates:
        if sp.exists():
            shapefile_path = str(sp)
            print(f"Found shapefile: {shapefile_path}")
            break

    # Plot
    out = plot_ntl_map(arr, shapefile_path)
    print(f"\nDone! Open the map: {out}")


if __name__ == '__main__':
    main()
