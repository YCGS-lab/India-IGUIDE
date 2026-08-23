// This script selects and calculates metrics (e.g., mean, percentiles) of NTL for India // at the district level


// India districts
var districts = ee.FeatureCollection('projects/ee-alegbeleyeokiki-fireapp/assets/India_District_2023_simple10');


Map.centerObject(india_districts);

Map.addLayer(india_districts, {color: 'red'},  "Districts", false);

// Composite VIIRS and clip to India
var viirs = ee.ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG')
              .filterDate('2024-01-01','2024-12-31')
              .select('avg_rad');
var annualMean = viirs.mean();
var clippedMean = annualMean.clip(india_districts.geometry().bounds());

var dnbVis = {
  min: 0,
  max: 10,
  palette: ['black', 'purple', 'cyan', 'green', 'yellow', 'red', 'white'],
};
Map.addLayer(annualMean, dnbVis, " MeanNTL");

print(annualMean);

// Combined reducer
var statsReducer = ee.Reducer.mean()
  .combine(ee.Reducer.minMax(), '', true)
  .combine(ee.Reducer.sum(), '', true)
  .combine(ee.Reducer.stdDev(), '', true);

// Zonal stats
var zonalStats = clippedMean.reduceRegions({
  collection: india_districts,
  reducer: statsReducer,
  scale: 500,
  crs: 'EPSG:4326'
});

print(zonalStats);
// // Export 
// Export.table.toDrive({
//   collection: zonalStats,
//   description: 'India_District_Medain_NTL_2024',
//   fileFormat: 'CSV',
//   selectors: ['dist23','state23', 'sum', 'mean','min','max','stdDev']
// });