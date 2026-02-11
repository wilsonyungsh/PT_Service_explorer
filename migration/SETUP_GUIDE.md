# Multi-Version GTFS Shiny App - Setup Guide

## Overview
This updated app supports multiple GTFS versions with seamless switching between different service periods.

## Key Changes from Original

### 1. **Data Organization**
- **Before**: Single `data/` folder with files
- **After**: Version-specific folders under `data/`:
  ```
  data/
  ├── 2025_08_28/
  │   ├── pt_stops.parquet
  │   ├── agg_stops.parquet
  │   └── geo.gpkg
  ├── 2025_11_01/
  │   ├── pt_stops.parquet
  │   ├── agg_stops.parquet
  │   └── geo.gpkg
  └── 2026_02_01/
      ├── pt_stops.parquet
      ├── agg_stops.parquet
      └── geo.gpkg
  ```

### 2. **Version Configuration**
At the top of `app_multiversion.R`, configure your versions:

```r
gtfs_versions <- list(
  "2025-08-28 to 2025-10-27" = "data/2025_08_28",
  "2025-11-01 to 2026-01-15" = "data/2025_11_01",
  "2026-02-01 to 2026-04-30" = "data/2026_02_01"
)
```

### 3. **New Features**
- ✅ **Version selector** in top navbar
- ✅ **Data caching** - each version loads once and stays in memory
- ✅ **Reactive data loading** - UI updates when version changes
- ✅ **Loading notifications** for user feedback
- ✅ **Auto-reset filters** when switching versions

### 4. **Performance Optimizations**
- Data is cached in `reactiveValues()` to prevent reloading
- Only loads new version when selected (lazy loading)
- Clears search selections when switching versions
- Resets filters to default values

## Migration Steps

### Step 1: Reorganize Your Data
```bash
# Create version folders
mkdir -p data/2025_08_28
mkdir -p data/2025_11_01

# Move current data to first version
mv data/pt_stops.parquet data/2025_08_28/
mv data/agg_stops.parquet data/2025_08_28/
mv data/geo.gpkg data/2025_08_28/

# Copy to new version (or process new GTFS)
cp -r data/2025_08_28/* data/2025_11_01/
```

### Step 2: Update Version Configuration
Edit the `gtfs_versions` list at the top of the script:
- Key = Display name (shown in dropdown)
- Value = Path to data folder

### Step 3: Test the App
```r
# Run the app
shiny::runApp("app_multiversion.R")
```

## Adding New GTFS Versions

### Option A: Quick Add (Same Structure)
1. Create new folder: `data/YYYY_MM_DD/`
2. Add your three files: `pt_stops.parquet`, `agg_stops.parquet`, `geo.gpkg`
3. Update `gtfs_versions` list in code
4. Restart app

### Option B: Automated Script
Create a helper script:

```r
# add_gtfs_version.R
add_new_version <- function(version_name, version_folder, 
                           pt_stops_path, agg_stops_path, geo_path) {
  
  # Create folder
  dir.create(file.path("data", version_folder), recursive = TRUE)
  
  # Copy files
  file.copy(pt_stops_path, file.path("data", version_folder, "pt_stops.parquet"))
  file.copy(agg_stops_path, file.path("data", version_folder, "agg_stops.parquet"))
  file.copy(geo_path, file.path("data", version_folder, "geo.gpkg"))
  
  message("✓ Version added: ", version_name)
  message("  Folder: data/", version_folder)
  message("
Remember to update gtfs_versions list in app_multiversion.R:")
  message('  "', version_name, '" = "data/', version_folder, '"')
}

# Example usage:
add_new_version(
  version_name = "2026-02-01 to 2026-04-30",
  version_folder = "2026_02_01",
  pt_stops_path = "raw_data/new_pt_stops.parquet",
  agg_stops_path = "raw_data/new_agg_stops.parquet",
  geo_path = "raw_data/new_geo.gpkg"
)
```

## Architecture Details

### Data Flow
```
User selects version
    ↓
current_data() reactive checks cache
    ↓
If cached → Return immediately
If not → Load from disk + cache it
    ↓
Update selectize inputs with new choices
    ↓
Reset filters and selections
    ↓
Map re-renders with new data
```

### Memory Management
- Each version stays in memory after first load
- Typical memory per version: ~5-20 MB
- With 5 versions: ~25-100 MB total
- Cache cleared when app restarts

### UI Updates on Version Change
1. Selectize inputs get new stop/route choices
2. Current selections are cleared
3. Filters reset to defaults
4. Map re-renders with new geographic data
5. Tables update if stop/route is re-selected

## Customization Options

### 1. Change Default Version
```r
# Set to first (oldest)
default_version <- names(gtfs_versions)[1]

# Set to last (newest)
default_version <- names(gtfs_versions)[length(gtfs_versions)]

# Set to specific version
default_version <- "2025-11-01 to 2026-01-15"
```

### 2. Auto-detect Available Versions
```r
# Automatically scan data/ folder
data_folders <- list.dirs("data", full.names = FALSE, recursive = FALSE)
data_folders <- data_folders[grepl("^20[0-9]{2}_[0-9]{2}_[0-9]{2}$", data_folders)]

gtfs_versions <- setNames(
  file.path("data", data_folders),
  gsub("_", "-", data_folders)
)
```

### 3. Add Metadata Display
Show more info about each version:

```r
gtfs_metadata <- list(
  "2025-08-28 to 2025-10-27" = list(
    path = "data/2025_08_28",
    stops_count = 5234,
    routes_count = 178,
    notes = "Regular service"
  )
)

# Display in UI
output$version_metadata <- renderText({
  meta <- gtfs_metadata[[input$gtfs_version_select]]
  paste0(meta$stops_count, " stops • ", 
         meta$routes_count, " routes • ", 
         meta$notes)
})
```

### 4. Persist User's Version Choice
Use browser cookies or URL parameters:

```r
# In server function
observe({
  query <- parseQueryString(session$clientData$url_search)
  if (!is.null(query$version)) {
    updateSelectInput(session, "gtfs_version_select", 
                     selected = query$version)
  }
})

# Update URL when version changes
observeEvent(input$gtfs_version_select, {
  updateQueryString(paste0("?version=", 
                          URLencode(input$gtfs_version_select)))
})
```

## Troubleshooting

### Issue: Version won't load
**Check:**
- Folder exists at specified path
- All three files present: `pt_stops.parquet`, `agg_stops.parquet`, `geo.gpkg`
- File permissions are correct
- GeoPackage layer name is "pt_route_geom"

### Issue: Memory usage too high
**Solutions:**
- Limit number of cached versions
- Add cache eviction logic:
```r
# Keep only last 3 versions
if (length(data_cache$versions) > 3) {
  oldest <- names(data_cache$versions)[1]
  data_cache$versions[[oldest]] <- NULL
}
```

### Issue: Slow version switching
**Solutions:**
- Pre-load all versions at startup (if memory allows)
- Use `.feather` instead of `.parquet` for faster reads
- Reduce spatial precision in geo.gpkg

## Best Practices

1. **Naming Convention**: Use `YYYY_MM_DD` format for folders
2. **Version Labels**: Include date ranges for clarity
3. **Documentation**: Keep a changelog of what's different between versions
4. **Testing**: Test all versions before deployment
5. **Backup**: Keep raw GTFS feeds for reprocessing
6. **File Size**: Keep parquet files optimized (use compression)

## Future Enhancements

- [ ] Add version comparison mode (side-by-side maps)
- [ ] Show diff stats between versions
- [ ] Export filtered data for selected version
- [ ] Automated version updates from Translink API
- [ ] Admin panel for managing versions
- [ ] A/B testing different routing algorithms
