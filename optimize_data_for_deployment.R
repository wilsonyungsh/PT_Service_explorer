# optimize_data_for_deployment.R
# Run this script to reduce file sizes before deploying to shinyapps.io

library(arrow)
library(sf)
library(dplyr)

# ==============================================================================
# CONFIGURATION: Adjust these values to control optimization level
# ==============================================================================

# Geometry simplification tolerances (in meters)
ROUTE_TOLERANCE <- 20        # Routes: 10-30m recommended (higher = smaller files)
ZONE_TOLERANCE <- 0          # Zones: 0 = NO SIMPLIFICATION (keep original)

# Parquet compression level (1-9, higher = smaller but slower)
COMPRESSION_LEVEL <- 9

cat("\n")
cat("========================================\n")
cat("Optimization Settings:\n")
cat("========================================\n")
cat("Route simplification:", ROUTE_TOLERANCE, "meters\n")
cat("Zone simplification:", if(ZONE_TOLERANCE == 0) "NONE (original geometry)" else paste(ZONE_TOLERANCE, "meters"), "\n")
cat("Parquet compression: Level", COMPRESSION_LEVEL, "\n")
cat("========================================\n\n")

#' Optimize a single version folder
optimize_version <- function(version_folder) {
  cat("\n=== Optimizing:", version_folder, "===\n")
  
  base_path <- file.path("data", version_folder)
  
  # 1. Optimize pt_stops.parquet
  pt_stops_path <- file.path(base_path, "pt_stops.parquet")
  if (file.exists(pt_stops_path)) {
    cat("Optimizing pt_stops.parquet...\n")
    original_size <- file.size(pt_stops_path) / 1024^2
    
    pt_stops <- read_parquet(pt_stops_path)
    
    # Keep only essential columns
    essential_cols <- c("stop_id", "stop_name", "mode", "x", "y")
    if (all(essential_cols %in% names(pt_stops))) {
      pt_stops <- pt_stops %>% select(all_of(essential_cols))
    }
    
    # Write with maximum compression
    write_parquet(pt_stops, pt_stops_path, compression = "gzip", compression_level = 9)
    
    new_size <- file.size(pt_stops_path) / 1024^2
    cat(sprintf("  Before: %.2f MB -> After: %.2f MB (saved %.2f MB)\n", 
                original_size, new_size, original_size - new_size))
  }
  
  # 2. Optimize agg_stops.parquet
  agg_stops_path <- file.path(base_path, "agg_stops.parquet")
  if (file.exists(agg_stops_path)) {
    cat("Optimizing agg_stops.parquet...\n")
    original_size <- file.size(agg_stops_path) / 1024^2
    
    agg_stops <- read_parquet(agg_stops_path)
    
    # Remove unnecessary columns
    cols_to_remove <- c("day_cnt", "hours_cnt", "x", "y")
    cols_to_remove <- cols_to_remove[cols_to_remove %in% names(agg_stops)]
    if (length(cols_to_remove) > 0) {
      agg_stops <- agg_stops %>% select(-all_of(cols_to_remove))
    }
    
    # Write with compression
    write_parquet(agg_stops, agg_stops_path, compression = "gzip", compression_level = 9)
    
    new_size <- file.size(agg_stops_path) / 1024^2
    cat(sprintf("  Before: %.2f MB -> After: %.2f MB (saved %.2f MB)\n", 
                original_size, new_size, original_size - new_size))
  }
  
  # 3. Optimize geo.gpkg (simplify geometries)
  geo_path <- file.path(base_path, "geo.gpkg")
  if (file.exists(geo_path)) {
    cat("Optimizing geo.gpkg (simplifying geometries)...\n")
    original_size <- file.size(geo_path) / 1024^2
    
    pt_route <- st_read(geo_path, layer = "pt_route_geom", quiet = TRUE)
    
    # Simplify geometry using configured tolerance
    cat(sprintf("  Using tolerance: %dm\n", ROUTE_TOLERANCE))
    pt_route_simple <- st_simplify(pt_route, dTolerance = ROUTE_TOLERANCE)
    
    # Remove temp file if exists
    temp_geo <- file.path(base_path, "geo_temp.gpkg")
    if (file.exists(temp_geo)) file.remove(temp_geo)
    
    # Write simplified version
    st_write(pt_route_simple, temp_geo, layer = "pt_route_geom", quiet = TRUE)
    
    # Replace original
    file.remove(geo_path)
    file.rename(temp_geo, geo_path)
    
    new_size <- file.size(geo_path) / 1024^2
    cat(sprintf("  Before: %.2f MB -> After: %.2f MB (saved %.2f MB)\n", 
                original_size, new_size, original_size - new_size))
  }
  
  cat("✓ Optimization complete for", version_folder, "\n")
}

#' Optimize zone overlay
optimize_zone_overlay <- function() {
  cat("\n=== Optimizing Zone Overlay ===\n")
  
  geo_path <- "data/common_datasets.gpkg"
  
  if (!file.exists(geo_path)) {
    cat("Zone overlay file not found\n")
    return()
  }
  
  original_size <- file.size(geo_path) / 1024^2
  
  if (ZONE_TOLERANCE == 0) {
    cat("Skipping zone simplification (keeping original geometry)\n")
    cat(sprintf("Zone overlay size: %.2f MB (unchanged)\n", original_size))
    cat("✓ Zone overlay preserved as original\n")
    return()
  }
  
  zone_overlay <- st_read(geo_path, layer = "zone_overlay", quiet = TRUE)
  
  # Simplify zones if tolerance > 0
  cat(sprintf("Using tolerance: %dm\n", ZONE_TOLERANCE))
  zone_simple <- st_simplify(zone_overlay, dTolerance = ZONE_TOLERANCE)
  
  # Remove temp file if exists
  temp_geo <- "data/common_datasets_temp.gpkg"
  if (file.exists(temp_geo)) file.remove(temp_geo)
  
  # Write simplified version
  st_write(zone_simple, temp_geo, layer = "zone_overlay", quiet = TRUE)
  
  # Replace original
  file.remove(geo_path)
  file.rename(temp_geo, geo_path)
  
  new_size <- file.size(geo_path) / 1024^2
  cat(sprintf("Before: %.2f MB -> After: %.2f MB (saved %.2f MB)\n", 
              original_size, new_size, original_size - new_size))
  
  cat("✓ Zone overlay optimization complete\n")
}

#' Main optimization function
optimize_all_data <- function() {
  cat("========================================\n")
  cat("Data Optimization for shinyapps.io\n")
  cat("========================================\n")
  
  total_before <- 0
  total_after <- 0
  
  # Get all version folders
  data_folders <- list.dirs("data", full.names = FALSE, recursive = FALSE)
  data_folders <- data_folders[data_folders != ""]
  
  # Optimize each version
  for (folder in data_folders) {
    if (!grepl("common", folder, ignore.case = TRUE)) {
      optimize_version(folder)
    }
  }
  
  # Optimize zone overlay
  optimize_zone_overlay()
  
  cat("\n========================================\n")
  cat("Optimization Complete!\n")
  cat("========================================\n")
  cat("\nNext steps:\n")
  cat("1. Test locally: shiny::runApp('app_lowmem.R')\n")
  cat("2. Deploy: rsconnect::deployApp()\n")
}

# Show data folder sizes
show_data_sizes <- function() {
  cat("\nCurrent Data Folder Sizes:\n")
  cat("==========================\n")
  
  data_folders <- list.dirs("data", full.names = TRUE, recursive = FALSE)
  
  for (folder in data_folders) {
    files <- list.files(folder, full.names = TRUE, recursive = TRUE)
    total_size <- sum(file.size(files)) / 1024^2
    cat(sprintf("%-30s: %.2f MB\n", basename(folder), total_size))
  }
  
  total <- sum(file.size(list.files("data", full.names = TRUE, recursive = TRUE))) / 1024^2
  cat(sprintf("\n%-30s: %.2f MB\n", "TOTAL", total))
}

# Run this to see current sizes
cat("\n")
show_data_sizes()

cat("\n")
cat("To optimize all data, run:\n")
cat("  optimize_all_data()\n\n")
cat("WARNING: This will modify your data files.\n")
cat("Make sure you have backups before proceeding!\n")
