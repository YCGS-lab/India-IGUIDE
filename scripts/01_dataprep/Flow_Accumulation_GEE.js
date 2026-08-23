/**************************************
 * 1. Input raster
 **************************************/
var flow_accu = ee.Image('WWF/HydroSHEDS/15ACC')
  .select('b1')
  .rename('flow_sum');


/**************************************
 * 2. Zonal statistics (as-is)
 *    Sum of flow accumulation per district
 **************************************/
var dist_with_flow_sum = flow_accu.reduceRegions({
  collection: districts,
  reducer: ee.Reducer.sum(),
  scale: flow_accu.projection().nominalScale(),
  crs: flow_accu.projection()
}).map(function (f) {
  var sum_val = f.get('sum');
  return f
    .set('FlowAcc_sum', sum_val)
    .set('sum', null);
});


/**************************************
 * 3. Raster visualization (context)
 **************************************/
var flowAccumulationVis = {
  min: 0,
  max: 500,
  palette: [
    '000000', '023858', '006837', '1a9850', '66bd63', 'a6d96a',
    'd9ef8b', 'ffffbf', 'fee08b', 'fdae61', 'f46d43', 'd73027'
  ]
};

Map.addLayer(flow_accu, flowAccumulationVis, 'Flow Accumulation');



/**************************************
 * 4. Inspect results (Console)
 **************************************/
print('District-level flow accumulation (sample)',
      dist_with_flow_sum.limit(10));


/**************************************
 * 5. Export zonal stats table
 **************************************/
var outTable = dist_with_flow_sum.map(function(f){
  return ee.Feature(null, f.toDictionary()); // keeps properties only, no geometry
});

Export.table.toDrive({
  collection: outTable,
  description: 'India_FloodPerception_FlowAccumulation',
  folder: 'India_FloodPerception_FlowAccumulation',
  fileFormat: 'CSV'
});
