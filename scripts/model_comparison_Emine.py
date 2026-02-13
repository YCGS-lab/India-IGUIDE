#!/usr/bin/env python3
"""
4 Feature Scenarios × 4 Classifiers
======================================================
Compares different feature combinations with multiple classifiers
using 250km spatial stratification.

Scenarios:
  M1: Demographics only
  M2: M1 + Climate + Flood indicators
  M3: M2 + Spatial features (lag + LISA)
  M4: M3 + Alpha Earth embeddings (everything)

Classifiers:
  - RandomForest
  - GradientBoosting
  - LogisticRegression
  - LightGBM

"""

import os
import pandas as pd
import numpy as np
import geopandas as gpd
import warnings
import time
warnings.filterwarnings('ignore')

from sklearn.preprocessing import LabelEncoder, StandardScaler
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, roc_auc_score, f1_score, precision_score, recall_score
from lightgbm import LGBMClassifier
from libpysal.weights import Queen, KNN
import libpysal
from esda.moran import Moran_Local

# ============================================================================
# SET UP PATHS 
# ============================================================================

# Get the directory where this script is located
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# Project root is one level up from code/
PROJECT_ROOT = os.path.dirname(SCRIPT_DIR)

# Define all file paths
RAW_DATA_PATH = os.path.join(PROJECT_ROOT, 'data', 'raw', 'RAP_Flood_merged_all.csv')
SHAPEFILE_PATH = os.path.join(PROJECT_ROOT, 'data', 'shapefiles', 'district_state_shapefile_2023.shp')
RESULTS_DIR = os.path.join(PROJECT_ROOT, 'results', 'comparison')

print("=" * 80)
print("MODEL COMPARISON: 4 Feature Scenarios × 4 Classifiers")
print("=" * 80)
print(f"\nProject root: {PROJECT_ROOT}")

# ============================================================================
# STEP 1: LOAD DATA
# ============================================================================

print("\n[STEP 1] Loading data...")

data = pd.read_csv(RAW_DATA_PATH)
shapefile = gpd.read_file(SHAPEFILE_PATH)
TARGET = 'n7fy23_recode'

data = data.dropna(subset=[TARGET])
data[TARGET] = data[TARGET].astype(int)
data['di_code'] = data['di_code'].astype(int)

print(f"  Loaded {len(data)} individuals, {data['di_code'].nunique()} districts")


# ============================================================================
# STEP 2: CREATE SPATIAL SPLIT (250km grid, 18% test)
# ============================================================================

print("\n[STEP 2] Creating spatial split...")

SEED = 42
shapefile_utm = shapefile.to_crs(epsg=32644)
shapefile_utm['centroid_x'] = shapefile_utm.geometry.centroid.x
shapefile_utm['centroid_y'] = shapefile_utm.geometry.centroid.y

grid_meters = 250 * 1000
x_min, y_min, _, _ = shapefile_utm.total_bounds
shapefile_utm['grid_cell'] = (
    ((shapefile_utm['centroid_x'] - x_min) // grid_meters).astype(int).astype(str) + '_' +
    ((shapefile_utm['centroid_y'] - y_min) // grid_meters).astype(int).astype(str)
)

np.random.seed(SEED)
all_cells = shapefile_utm['grid_cell'].unique()
n_test_cells = max(1, int(len(all_cells) * 0.18))
test_cells = set(np.random.choice(all_cells, size=n_test_cells, replace=False))
shapefile_utm['split'] = shapefile_utm['grid_cell'].apply(lambda c: 'test' if c in test_cells else 'train')

# Find matching column
shp_code_col = None
for col in shapefile_utm.columns:
    if col in ['geometry', 'centroid_x', 'centroid_y', 'grid_cell', 'split']:
        continue
    try:
        vals = set(shapefile_utm[col].dropna().astype(float).astype(int))
        if len(vals & set(data['di_code'].unique())) > 100:
            shp_code_col = col
            break
    except:
        continue

shapefile_utm = shapefile_utm.dropna(subset=[shp_code_col])
shapefile_utm[shp_code_col] = shapefile_utm[shp_code_col].astype(int)
split_map = shapefile_utm.set_index(shp_code_col)['split'].to_dict()
data['split'] = data['di_code'].map(split_map).fillna('train')

train_n = (data['split'] == 'train').sum()
test_n = (data['split'] == 'test').sum()
print(f"  Train: {train_n} ({train_n/len(data)*100:.1f}%), Test: {test_n} ({test_n/len(data)*100:.1f}%)")


# ============================================================================
# STEP 3: COMPUTE SPATIAL FEATURES
# ============================================================================

print("\n[STEP 3] Computing spatial features...")

# Aggregate to district level
district_data = data.groupby('di_code').agg({TARGET: 'mean'}).reset_index()
district_data.columns = ['di_code', 'worry_rate']

# Add other variables for spatial lag
env_vars = ['precip_sum_mm', 'mean_tmax', 'vulnerability_district',
            'FlowAcc_sum', 'GFD_sum', 'flood_news_count', 'Tone_Avg', 'mean_monthly_avg_rd_MD']
env_vars = [v for v in env_vars if v in data.columns]
env_agg = data.groupby('di_code')[env_vars].first().reset_index()
district_data = district_data.merge(env_agg, on='di_code')

# Match with shapefile
shp_subset = shapefile[[shp_code_col, 'geometry']].copy()
shp_subset = shp_subset.dropna(subset=[shp_code_col])
shp_subset[shp_code_col] = shp_subset[shp_code_col].astype(int)
geo_districts = shp_subset.merge(district_data, left_on=shp_code_col, right_on='di_code')
geo_districts = gpd.GeoDataFrame(geo_districts, geometry='geometry')

# Build spatial weights
weights = Queen.from_dataframe(geo_districts)
islands = [i for i in range(len(geo_districts)) if len(weights.neighbors[i]) == 0]
if islands:
    knn_weights = KNN.from_dataframe(geo_districts, k=5)
    for i in islands:
        weights.neighbors[i] = knn_weights.neighbors[i]
        weights.weights[i] = knn_weights.weights[i]
weights.transform = 'r'

# Compute spatial lag features
lag_variables = ['worry_rate', 'precip_sum_mm', 'mean_tmax', 'vulnerability_district',
                 'FlowAcc_sum', 'GFD_sum', 'flood_news_count', 'Tone_Avg']
lag_variables = [v for v in lag_variables if v in geo_districts.columns]

SPATIAL_LAG_COLS = []
for var in lag_variables:
    col_name = f'lag_{var}'
    values = geo_districts[var].fillna(0).values
    geo_districts[col_name] = libpysal.weights.lag_spatial(weights, values)
    SPATIAL_LAG_COLS.append(col_name)

print(f"  Created {len(SPATIAL_LAG_COLS)} spatial lag features")

# Compute LISA cluster features
lisa_variables = ['worry_rate', 'precip_sum_mm', 'mean_tmax', 'vulnerability_district', 'GFD_sum']
lisa_variables = [v for v in lisa_variables if v in geo_districts.columns]
cluster_names = {1: 'high_high', 2: 'low_high', 3: 'low_low', 4: 'high_low'}

LISA_COLS = []
for var in lisa_variables:
    values = geo_districts[var].fillna(0).values
    lisa = Moran_Local(values, weights, permutations=999, seed=42)
    short_name = var.replace('_district', '').replace('_sum', '')

    col_I = f'lisa_I_{short_name}'
    geo_districts[col_I] = lisa.Is
    LISA_COLS.append(col_I)

    significant = lisa.p_sim <= 0.05
    for code, name in cluster_names.items():
        col = f'lisa_{short_name}_{name}'
        geo_districts[col] = ((lisa.q == code) & significant).astype(int)
        LISA_COLS.append(col)

    col_ns = f'lisa_{short_name}_not_sig'
    geo_districts[col_ns] = (~significant).astype(int)
    LISA_COLS.append(col_ns)

print(f"  Created {len(LISA_COLS)} LISA cluster features")

# Merge spatial features back to individuals
spatial_cols = ['di_code'] + SPATIAL_LAG_COLS + LISA_COLS
spatial_df = pd.DataFrame(geo_districts[spatial_cols])
data = data.merge(spatial_df, on='di_code', how='left')

for col in SPATIAL_LAG_COLS + LISA_COLS:
    data[col] = data[col].fillna(0)

print(f"  Merged to {len(data)} individuals")


# ============================================================================
# STEP 4: DEFINE FEATURE SCENARIOS
# ============================================================================

print("\n[STEP 4] Defining feature scenarios...")

# M1: Demographics only
M1_DEMOGRAPHICS = ['age', 'gender', 'urban', 'caste', 'di_code']

# M2: M1 + Climate + Flood indicators
M2_CLIMATE_FLOOD = [
    # Climate
    'mean_monthly_avg_rd_MD', 'precip_sum_mm', 'precip_mean_mm', 'mean_tmax',
    # Flood sentiment
    'district_flood_sentiment', 'state_flood_sentiment',
    # Flood/vulnerability indicators
    'vulnerability_district', 'FlowAcc_sum', 'GFD_sum',
    # News/Tone
    'flood_news_count', 'Tone_Avg', 'Tone_Positive', 'Tone_Negative',
    'Tone_Polarity', 'Tone_ActivityRef', 'Tone_SelfGroupRef',
    # District education
    'highersecondaryabove_district'
]
M2_CLIMATE_FLOOD = [c for c in M2_CLIMATE_FLOOD if c in data.columns]

# M3: M2 + Spatial features
M3_SPATIAL = SPATIAL_LAG_COLS + LISA_COLS

# M4: Everything including Alpha Earth
ALPHA_EARTH = [col for col in data.columns
               if col.startswith('A') and '_' in col
               and col.split('_')[0][1:].isdigit()]

# Build cumulative feature sets
SCENARIOS = {
    'M1_Demographics': M1_DEMOGRAPHICS,
    'M2_Climate_Flood': M1_DEMOGRAPHICS + M2_CLIMATE_FLOOD,
    'M3_With_Spatial': M1_DEMOGRAPHICS + M2_CLIMATE_FLOOD + M3_SPATIAL,
    'M4_Everything': M1_DEMOGRAPHICS + M2_CLIMATE_FLOOD + M3_SPATIAL + ALPHA_EARTH,
}

# Remove duplicates and filter to existing columns
for name in SCENARIOS:
    SCENARIOS[name] = list(dict.fromkeys(SCENARIOS[name]))
    SCENARIOS[name] = [f for f in SCENARIOS[name] if f in data.columns]

print("\nFeature scenarios:")
for name, features in SCENARIOS.items():
    print(f"  {name}: {len(features)} features")


# ============================================================================
# STEP 5: ENCODE CATEGORICAL VARIABLES
# ============================================================================

print("\n[STEP 5] Encoding categorical variables...")

encoders = {}
for col in ['age', 'gender', 'urban', 'caste']:
    if col in data.columns and data[col].dtype == 'object':
        encoders[col] = LabelEncoder()
        data[col] = encoders[col].fit_transform(data[col].astype(str))
        print(f"  Encoded '{col}'")


# ============================================================================
# STEP 6: DEFINE CLASSIFIERS
# ============================================================================

print("\n[STEP 6] Defining classifiers...")

CLASSIFIERS = {
    'RandomForest': RandomForestClassifier(
        n_estimators=100, max_depth=10, random_state=SEED, n_jobs=-1
    ),
    'GradientBoosting': GradientBoostingClassifier(
        n_estimators=100, max_depth=5, learning_rate=0.1, random_state=SEED
    ),
    'LogisticRegression': LogisticRegression(
        max_iter=1000, random_state=SEED, n_jobs=-1
    ),
    'LightGBM': LGBMClassifier(
        n_estimators=100, max_depth=3, learning_rate=0.1,
        num_leaves=31, random_state=SEED, verbose=-1, n_jobs=-1
    ),
}

print(f"  Classifiers: {list(CLASSIFIERS.keys())}")


# ============================================================================
# STEP 7: TRAIN AND EVALUATE ALL COMBINATIONS
# ============================================================================

print("\n[STEP 7] Training all combinations...")
print("=" * 80)

results = []
feature_importances = {}

total_combinations = len(SCENARIOS) * len(CLASSIFIERS)
current = 0

for scenario_name, features in SCENARIOS.items():
    print(f"\n{'─' * 80}")
    print(f"SCENARIO: {scenario_name} ({len(features)} features)")
    print(f"{'─' * 80}")

    # Prepare data
    X_train = data[data['split'] == 'train'][features].fillna(0).replace([np.inf, -np.inf], 0)
    X_test = data[data['split'] == 'test'][features].fillna(0).replace([np.inf, -np.inf], 0)
    y_train = data[data['split'] == 'train'][TARGET]
    y_test = data[data['split'] == 'test'][TARGET]

    # Scale for Logistic Regression
    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    X_test_scaled = scaler.transform(X_test)

    for clf_name, clf in CLASSIFIERS.items():
        current += 1
        print(f"  [{current}/{total_combinations}] {clf_name}...", end=' ', flush=True)

        start = time.time()

        # Use scaled data for LogisticRegression
        if clf_name == 'LogisticRegression':
            X_tr, X_te = X_train_scaled, X_test_scaled
        else:
            X_tr, X_te = X_train.values, X_test.values

        # Train
        model = clf.__class__(**clf.get_params())
        model.fit(X_tr, y_train)

        # Predict
        y_pred = model.predict(X_te)
        y_prob = model.predict_proba(X_te)[:, 1]

        # Metrics
        elapsed = time.time() - start

        result = {
            'Scenario': scenario_name,
            'Classifier': clf_name,
            'N_Features': len(features),
            'Accuracy': accuracy_score(y_test, y_pred),
            'AUC': roc_auc_score(y_test, y_prob),
            'F1': f1_score(y_test, y_pred),
            'Precision': precision_score(y_test, y_pred),
            'Recall': recall_score(y_test, y_pred),
            'Time_sec': round(elapsed, 1)
        }
        results.append(result)

        print(f"AUC={result['AUC']:.4f}, Acc={result['Accuracy']:.4f}, F1={result['F1']:.4f} ({elapsed:.1f}s)")

        # Feature importance
        if hasattr(model, 'feature_importances_'):
            imp = pd.DataFrame({
                'Feature': features,
                'Importance': model.feature_importances_
            }).sort_values('Importance', ascending=False)
        elif hasattr(model, 'coef_'):
            imp = pd.DataFrame({
                'Feature': features,
                'Importance': np.abs(model.coef_[0])
            }).sort_values('Importance', ascending=False)
        else:
            imp = None

        if imp is not None:
            feature_importances[f"{scenario_name}__{clf_name}"] = imp


# ============================================================================
# STEP 8: CREATE RESULTS TABLE
# ============================================================================

print("\n" + "=" * 80)
print("RESULTS SUMMARY")
print("=" * 80)

results_df = pd.DataFrame(results)

# Pivot table for AUC
print("\n┌─────────────────────────────────────────────────────────────────────────────┐")
print("│                              TEST AUC                                       │")
print("├─────────────────────────────────────────────────────────────────────────────┤")

pivot_auc = results_df.pivot(index='Scenario', columns='Classifier', values='AUC')
pivot_auc = pivot_auc[['RandomForest', 'GradientBoosting', 'LogisticRegression', 'LightGBM']]
print(pivot_auc.round(4).to_string())

print("└─────────────────────────────────────────────────────────────────────────────┘")

# Pivot table for Accuracy
print("\n┌─────────────────────────────────────────────────────────────────────────────┐")
print("│                            TEST ACCURACY                                    │")
print("├─────────────────────────────────────────────────────────────────────────────┤")

pivot_acc = results_df.pivot(index='Scenario', columns='Classifier', values='Accuracy')
pivot_acc = pivot_acc[['RandomForest', 'GradientBoosting', 'LogisticRegression', 'LightGBM']]
print(pivot_acc.round(4).to_string())

print("└─────────────────────────────────────────────────────────────────────────────┘")

# Pivot table for F1
print("\n┌─────────────────────────────────────────────────────────────────────────────┐")
print("│                              TEST F1                                        │")
print("├─────────────────────────────────────────────────────────────────────────────┤")

pivot_f1 = results_df.pivot(index='Scenario', columns='Classifier', values='F1')
pivot_f1 = pivot_f1[['RandomForest', 'GradientBoosting', 'LogisticRegression', 'LightGBM']]
print(pivot_f1.round(4).to_string())

print("└─────────────────────────────────────────────────────────────────────────────┘")


# ============================================================================
# STEP 9: FIND BEST MODEL
# ============================================================================

print("\n" + "=" * 80)
print("BEST MODELS")
print("=" * 80)

# Best by AUC
best_auc_idx = results_df['AUC'].idxmax()
best_auc = results_df.loc[best_auc_idx]
print(f"\nBest by AUC:")
print(f"  Scenario:   {best_auc['Scenario']}")
print(f"  Classifier: {best_auc['Classifier']}")
print(f"  AUC:        {best_auc['AUC']:.4f}")
print(f"  Accuracy:   {best_auc['Accuracy']:.4f}")
print(f"  F1:         {best_auc['F1']:.4f}")
print(f"  Features:   {best_auc['N_Features']}")

# Best per scenario
print("\nBest classifier per scenario:")
for scenario in SCENARIOS.keys():
    scenario_df = results_df[results_df['Scenario'] == scenario]
    best_idx = scenario_df['AUC'].idxmax()
    best = scenario_df.loc[best_idx]
    print(f"  {scenario}: {best['Classifier']} (AUC={best['AUC']:.4f})")


# ============================================================================
# STEP 10: FEATURE IMPORTANCE FOR BEST MODEL
# ============================================================================

print("\n" + "=" * 80)
print("FEATURE IMPORTANCE (Top 20 for Best Model)")
print("=" * 80)

best_key = f"{best_auc['Scenario']}__{best_auc['Classifier']}"
if best_key in feature_importances:
    best_imp = feature_importances[best_key]
    print(f"\n{best_auc['Scenario']} + {best_auc['Classifier']}:")
    print(f"\n{'Rank':<6}{'Feature':<45}{'Importance':>12}")
    print("-" * 65)
    for i, (_, row) in enumerate(best_imp.head(20).iterrows(), 1):
        print(f"{i:<6}{row['Feature']:<45}{row['Importance']:>12.4f}")


# ============================================================================
# STEP 11: SAVE RESULTS
# ============================================================================

print("\n" + "=" * 80)
print("SAVING RESULTS")
print("=" * 80)

os.makedirs(RESULTS_DIR, exist_ok=True)

# Save all results
results_path = os.path.join(RESULTS_DIR, 'model_comparison_results.csv')
results_df.to_csv(results_path, index=False)
print(f"  Saved: {results_path}")

# Save pivot tables
auc_path = os.path.join(RESULTS_DIR, 'auc_by_scenario.csv')
acc_path = os.path.join(RESULTS_DIR, 'accuracy_by_scenario.csv')
f1_path = os.path.join(RESULTS_DIR, 'f1_by_scenario.csv')

pivot_auc.to_csv(auc_path)
pivot_acc.to_csv(acc_path)
pivot_f1.to_csv(f1_path)
print(f"  Saved: {auc_path}")
print(f"  Saved: {acc_path}")
print(f"  Saved: {f1_path}")

# Save feature importances
for key, imp in feature_importances.items():
    filename = os.path.join(RESULTS_DIR, f'importance_{key}.csv')
    imp.to_csv(filename, index=False)
print(f"  Saved: {len(feature_importances)} feature importance files")


# ============================================================================
# FINAL SUMMARY
# ============================================================================

print("\n" + "=" * 80)
print("FINAL SUMMARY")
print("=" * 80)
print(f"""
SCENARIOS TESTED:
  M1_Demographics:    {len(SCENARIOS['M1_Demographics'])} features (age, gender, urban, caste, di_code)
  M2_Climate_Flood:   {len(SCENARIOS['M2_Climate_Flood'])} features (M1 + climate + flood indicators)
  M3_With_Spatial:    {len(SCENARIOS['M3_With_Spatial'])} features (M2 + spatial lag + LISA clusters)
  M4_Everything:      {len(SCENARIOS['M4_Everything'])} features (M3 + Alpha Earth embeddings)

CLASSIFIERS TESTED:
  RandomForest, GradientBoosting, LogisticRegression, LightGBM

TRAIN/TEST SPLIT:
  250km spatial grid stratification (18% test cells)
  Train: {train_n} individuals, Test: {test_n} individuals

BEST MODEL:
  {best_auc['Scenario']} + {best_auc['Classifier']}
  AUC = {best_auc['AUC']:.4f}
  Accuracy = {best_auc['Accuracy']:.4f}
  F1 = {best_auc['F1']:.4f}
""")

print("Done!")
