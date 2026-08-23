// This script selects and calculates metrics (e.g., mean, percentiles) of AEEs for India // at the district level

// India districts
var districts = ee.FeatureCollection('projects/ee-alegbeleyeokiki-fireapp/assets/India_District_2023_simple10');
var year = 2024;
var percentileList = [2,90,95,98];
var scale = 30;
var outputFolder = 'Embeddings_Stats_30m';
var bounds = districts.geometry().bounds();
var dissolved = districts.union(/*maxError=*/1);
var dissolvedFc = ee.FeatureCollection([dissolved]);

// 2. LOAD & FILTER THE ANNUAL EMBEDDING
var embedding = ee.ImageCollection('GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL')
  .filterDate('2024-01-01', '2025-01-01')
  .filterBounds(dissolved)
  .mosaic();

var embedding2 = ee.ImageCollection('GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL')
  .filterDate('2024-01-01', '2025-01-01')
  .filterBounds(bounds)
  .first();

var visParams = {min: -0.3, max: 0.3, bands: ['A01', 'A16', 'A09']};

Map.addLayer(embedding, visParams, '2024 embeddings', false);
Map.addLayer(districts, {color: 'lightblue'}, 'India');

// 4. GET BAND NAMES 
var bandList = embedding2.bandNames().getInfo();  
// print(bandList);

// 5. LOOP OVER BANDS AND EXPORT
bandList.forEach(function(band) {
  var img = embedding.select(band);
  //print(img);
  var perc = ee.Reducer.percentile(percentileList)
                .setOutputs(percentileList.map(function(p){return band+'_p'+p;}));
  var sum = ee.Reducer.sum().setOutputs([band+'_sum']);
  var cnt = ee.Reducer.count().setOutputs([band+'_count']);
  var minmax = ee.Reducer.minMax()
                   .setOutputs([band+'_min', band+'_max']);
  var mean = ee.Reducer.mean().setOutputs([band+'_mean']);

  var reducer = perc
    .combine(sum,    '', true)
    .combine(mean,   '', true)
    .combine(minmax, '', true)
    .combine(cnt,    '', true);

  var stats = img.reduceRegions({
    collection: districts,
    reducer: reducer,
    scale: scale
  });

// Export
  Export.table.toDrive({
    collection:     stats,
    description:    'EmbStats_30m'+band+'_'+year,
    folder:         outputFolder,
    fileNamePrefix:'EmbStats_30m'+band+'_'+year,
    fileFormat:     'CSV',
    selectors:      ['di_code','st_code','state23','dist23']
                    .concat(percentileList.map(function(p){return band+'_p'+p;}))
                    .concat([band+'_sum',band+'_mean',band+'_min',band+'_max',band+'_count'])
  });
});
