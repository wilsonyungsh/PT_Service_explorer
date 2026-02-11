# Migration Checklist & Workflows

## Quick Start Migration (5 minutes)

### Step 1: Backup Current Data
```bash
# Create backup folder
mkdir -p backup
cp -r data backup/data_$(date +%Y%m%d)
```

### Step 2: Reorganize Data Structure
```bash
# Create first version folder
mkdir -p data/2025_08_28

# Move existing files
mv data/pt_stops.parquet data/2025_08_28/
mv data/agg_stops.parquet data/2025_08_28/
mv data/geo.gpkg data/2025_08_28/
```

### Step 3: Use Helper Scripts
```r
# In R console
source("version_management.R")

# Verify your data
validate_gtfs_version("2025_08_28")

# Auto-generate the config code
generate_versions_code()
```

### Step 4: Update App File
1. Replace `app.R` with `app_multiversion.R`
2. Paste the generated config code from Step 3
3. Test: `shiny::runApp("app_multiversion.R")`

---

## Workflow 1: Adding Your First Additional Version

```r
# Example: You just downloaded new GTFS data for November

# Step 1: Source helper functions
source("version_management.R")

# Step 2: Add the new version
add_gtfs_version(
  version_name = "2025-11-01 to 2026-01-15",
  version_folder = "2025_11_01",
  pt_stops_path = "raw_gtfs/2025_11/pt_stops.parquet",
  agg_stops_path = "raw_gtfs/2025_11/agg_stops.parquet",
  geo_path = "raw_gtfs/2025_11/geo.gpkg",
  validate = TRUE  # Will check files before copying
)

# Step 3: Update app code as instructed by the function

# Step 4: Verify
list_gtfs_versions()
```

---

## Workflow 2: Bulk Import Multiple Versions

```r
# Suppose you have multiple GTFS periods to import
source("version_management.R")

versions_to_add <- list(
  list(
    name = "2025-08-28 to 2025-10-27",
    folder = "2025_08_28",
    pt_stops = "raw/aug/pt_stops.parquet",
    agg_stops = "raw/aug/agg_stops.parquet",
    geo = "raw/aug/geo.gpkg"
  ),
  list(
    name = "2025-11-01 to 2026-01-15",
    folder = "2025_11_01",
    pt_stops = "raw/nov/pt_stops.parquet",
    agg_stops = "raw/nov/agg_stops.parquet",
    geo = "raw/nov/geo.gpkg"
  ),
  list(
    name = "2026-02-01 to 2026-04-30",
    folder = "2026_02_01",
    pt_stops = "raw/feb/pt_stops.parquet",
    agg_stops = "raw/feb/agg_stops.parquet",
    geo = "raw/feb/geo.gpkg"
  )
)

# Import all versions
for (v in versions_to_add) {
  cat("\n=== Processing:", v$name, "===\n")
  add_gtfs_version(
    version_name = v$name,
    version_folder = v$folder,
    pt_stops_path = v$pt_stops,
    agg_stops_path = v$agg_stops,
    geo_path = v$geo,
    validate = TRUE
  )
}

# Generate config code for all
cat("\n\n")
generate_versions_code()

# Verify all versions
list_gtfs_versions()
```

---

## Workflow 3: Automated GTFS Updates

Create a script to automatically download and process new GTFS data:

```r
# automated_update.R
library(httr)
library(archive)

#' Download latest GTFS from Translink
download_latest_gtfs <- function(output_dir = "raw_gtfs/latest") {
  
  # Translink GTFS feed URL (example - update with actual URL)
  gtfs_url <- "https://gtfsrt.api.translink.com.au/feed/SEQ"
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(output_dir, "gtfs.zip")
  
  cat("Downloading GTFS feed...\n")
  GET(gtfs_url, write_disk(zip_path, overwrite = TRUE))
  
  cat("Extracting...\n")
  archive_extract(zip_path, dir = output_dir)
  
  cat("✓ Downloaded to:", output_dir, "\n")
  return(output_dir)
}

#' Process raw GTFS into app format
#' This is a placeholder - adjust based on your actual processing pipeline
process_gtfs_to_app_format <- function(gtfs_dir, output_dir) {
  
  # Your existing GTFS processing code goes here
  # This should create:
  # - pt_stops.parquet
  # - agg_stops.parquet  
  # - geo.gpkg
  
  cat("Processing GTFS data...\n")
  
  # Example processing (replace with your actual code)
  # stops <- read_csv(file.path(gtfs_dir, "stops.txt"))
  # routes <- read_csv(file.path(gtfs_dir, "routes.txt"))
  # ... your processing logic ...
  
  cat("✓ Processed to:", output_dir, "\n")
}

#' Full update workflow
update_gtfs_version <- function(version_name, version_folder) {
  
  # Download
  raw_dir <- download_latest_gtfs()
  
  # Process
  processed_dir <- file.path("processed", version_folder)
  process_gtfs_to_app_format(raw_dir, processed_dir)
  
  # Add to app
  source("version_management.R")
  add_gtfs_version(
    version_name = version_name,
    version_folder = version_folder,
    pt_stops_path = file.path(processed_dir, "pt_stops.parquet"),
    agg_stops_path = file.path(processed_dir, "agg_stops.parquet"),
    geo_path = file.path(processed_dir, "geo.gpkg"),
    validate = TRUE
  )
  
  cat("\n✓ Version update complete!\n")
}

# Usage:
# update_gtfs_version("2026-05-01 to 2026-07-31", "2026_05_01")
```

---

## Workflow 4: Maintaining Old Versions (Archive Strategy)

```r
# archive_old_versions.R

#' Archive versions older than X months
#' Moves them to archive/ folder instead of deleting
archive_old_versions <- function(keep_months = 6) {
  
  source("version_management.R")
  versions <- list_gtfs_versions()
  
  if (is.null(versions)) return()
  
  # Create archive directory
  archive_dir <- "archive"
  dir.create(archive_dir, showWarnings = FALSE)
  
  # Get current date
  current_date <- Sys.Date()
  
  for (folder in versions$folder) {
    # Extract date from folder name (assumes YYYY_MM_DD format)
    folder_date <- as.Date(paste0(
      substr(folder, 1, 4), "-",
      substr(folder, 6, 7), "-",
      substr(folder, 9, 10)
    ))
    
    # Check if older than threshold
    age_months <- as.numeric(difftime(current_date, folder_date, units = "days")) / 30.44
    
    if (age_months > keep_months) {
      cat("Archiving:", folder, "(", round(age_months, 1), "months old )\n")
      
      # Move to archive
      file.rename(
        file.path("data", folder),
        file.path(archive_dir, folder)
      )
    }
  }
  
  cat("\n✓ Archiving complete\n")
  cat("Active versions:\n")
  list_gtfs_versions()
}

# Usage:
# archive_old_versions(keep_months = 6)
```

---

## Workflow 5: Version Comparison Report

```r
# compare_versions.R

#' Generate comparison report between two versions
compare_gtfs_versions <- function(version_a_folder, version_b_folder) {
  
  cat("Comparing GTFS Versions\n")
  cat("=======================\n")
  cat("Version A:", version_a_folder, "\n")
  cat("Version B:", version_b_folder, "\n\n")
  
  # Load both versions
  pt_a <- read_parquet(file.path("data", version_a_folder, "pt_stops.parquet"))
  pt_b <- read_parquet(file.path("data", version_b_folder, "pt_stops.parquet"))
  
  agg_a <- read_parquet(file.path("data", version_a_folder, "agg_stops.parquet"))
  agg_b <- read_parquet(file.path("data", version_b_folder, "agg_stops.parquet"))
  
  route_a <- st_read(file.path("data", version_a_folder, "geo.gpkg"), 
                     layer = "pt_route_geom", quiet = TRUE)
  route_b <- st_read(file.path("data", version_b_folder, "geo.gpkg"), 
                     layer = "pt_route_geom", quiet = TRUE)
  
  # Compare stops
  cat("STOPS\n")
  cat("  Version A:", nrow(pt_a), "stops\n")
  cat("  Version B:", nrow(pt_b), "stops\n")
  cat("  Change:", nrow(pt_b) - nrow(pt_a), "\n\n")
  
  stops_a <- pt_a$stop_id
  stops_b <- pt_b$stop_id
  new_stops <- setdiff(stops_b, stops_a)
  removed_stops <- setdiff(stops_a, stops_b)
  
  if (length(new_stops) > 0) {
    cat("  New stops (", length(new_stops), "):\n")
    cat("   ", head(new_stops, 10), "...\n\n")
  }
  
  if (length(removed_stops) > 0) {
    cat("  Removed stops (", length(removed_stops), "):\n")
    cat("   ", head(removed_stops, 10), "...\n\n")
  }
  
  # Compare routes
  cat("ROUTES\n")
  cat("  Version A:", nrow(route_a), "routes\n")
  cat("  Version B:", nrow(route_b), "routes\n")
  cat("  Change:", nrow(route_b) - nrow(route_a), "\n\n")
  
  routes_a <- unique(route_a$route_short_name)
  routes_b <- unique(route_b$route_short_name)
  new_routes <- setdiff(routes_b, routes_a)
  removed_routes <- setdiff(routes_a, routes_b)
  
  if (length(new_routes) > 0) {
    cat("  New routes:", paste(new_routes, collapse = ", "), "\n\n")
  }
  
  if (length(removed_routes) > 0) {
    cat("  Removed routes:", paste(removed_routes, collapse = ", "), "\n\n")
  }
  
  # Service comparison
  cat("SERVICE PATTERNS\n")
  
  avg_headway_a <- mean(agg_a$max_headway_in_minutes, na.rm = TRUE)
  avg_headway_b <- mean(agg_b$max_headway_in_minutes, na.rm = TRUE)
  
  cat("  Avg max headway A:", round(avg_headway_a, 2), "min\n")
  cat("  Avg max headway B:", round(avg_headway_b, 2), "min\n")
  cat("  Change:", round(avg_headway_b - avg_headway_a, 2), "min\n\n")
  
  cat("✓ Comparison complete\n")
}

# Usage:
# compare_gtfs_versions("2025_08_28", "2025_11_01")
```

---

## Testing Checklist

Before deploying multi-version app:

- [ ] All versions load without errors
- [ ] Version switcher updates all UI elements correctly
- [ ] Map re-renders properly when switching versions
- [ ] Search inputs reset when switching versions
- [ ] Filters reset when switching versions
- [ ] Tables display correct data for each version
- [ ] Memory usage is acceptable
- [ ] No console errors in browser
- [ ] Mobile view works properly
- [ ] Version labels are clear and correct
- [ ] Default version loads on app startup
- [ ] Cache works (second load of same version is instant)

---

## Production Deployment

```bash
# 1. Final validation
Rscript -e "source('version_management.R'); list_gtfs_versions()"

# 2. Run automated tests (if you have them)
# Rscript tests/test_all_versions.R

# 3. Deploy to Shiny Server / shinyapps.io / Posit Connect
rsync -av --exclude='.git' --exclude='raw*' --exclude='backup' \
  ./ user@server:/srv/shiny-server/brisbane-pt-explorer/

# 4. Restart Shiny Server
# ssh user@server "sudo systemctl restart shiny-server"
```

---

## Troubleshooting Common Issues

### Issue: "Cannot read parquet file"
**Fix:** Check file path and permissions
```r
file.exists("data/2025_08_28/pt_stops.parquet")
file.access("data/2025_08_28/pt_stops.parquet", 4) # 4 = read permission
```

### Issue: "Layer 'pt_route_geom' not found"
**Fix:** Check GeoPackage layers
```r
library(sf)
st_layers("data/2025_08_28/geo.gpkg")
```

### Issue: Memory usage too high
**Fix:** Limit cache or use lazy loading
```r
# In server function, clear old cache entries
if (length(data_cache$versions) > 3) {
  # Keep only 3 most recent
  data_cache$versions <- data_cache$versions[tail(names(data_cache$versions), 3)]
}
```

### Issue: Version dropdown not updating
**Fix:** Check reactive dependencies
```r
# Make sure observeEvent is watching the right input
observeEvent(input$gtfs_version_select, { ... })
```

---

## Maintenance Schedule

### Monthly
- [ ] Check for new GTFS releases
- [ ] Run comparison report
- [ ] Update app with new version

### Quarterly  
- [ ] Archive versions older than 6 months
- [ ] Review memory usage
- [ ] Update documentation

### Yearly
- [ ] Full data audit
- [ ] Performance optimization review
- [ ] User feedback review
