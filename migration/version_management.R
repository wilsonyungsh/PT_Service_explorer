# GTFS Version Management Helper Functions
# Source this file: source("version_management.R")

library(arrow)
library(sf)
library(dplyr)

#' Add a new GTFS version to the app
#' 
#' @param version_name Display name (e.g., "2026-02-01 to 2026-04-30")
#' @param version_folder Folder name (e.g., "2026_02_01")
#' @param pt_stops_path Path to pt_stops.parquet file
#' @param agg_stops_path Path to agg_stops.parquet file
#' @param geo_path Path to geo.gpkg file
#' @param validate Whether to validate files before copying
#' 
#' @return TRUE if successful, FALSE otherwise
add_gtfs_version <- function(version_name, 
                             version_folder,
                             pt_stops_path, 
                             agg_stops_path, 
                             geo_path,
                             validate = TRUE) {
  
  target_dir <- file.path("data", version_folder)
  
  # Check if files exist
  if (!file.exists(pt_stops_path)) stop("pt_stops file not found: ", pt_stops_path)
  if (!file.exists(agg_stops_path)) stop("agg_stops file not found: ", agg_stops_path)
  if (!file.exists(geo_path)) stop("geo file not found: ", geo_path)
  
  # Validate files if requested
  if (validate) {
    cat("Validating files...\n")
    
    # Check pt_stops
    tryCatch({
      pt_stops <- read_parquet(pt_stops_path)
      required_cols <- c("stop_id", "stop_name", "mode", "x", "y")
      if (!all(required_cols %in% names(pt_stops))) {
        stop("pt_stops missing required columns: ", 
             paste(setdiff(required_cols, names(pt_stops)), collapse = ", "))
      }
      cat("  ✓ pt_stops.parquet valid (", nrow(pt_stops), " rows)\n")
    }, error = function(e) {
      stop("Error reading pt_stops: ", e$message)
    })
    
    # Check agg_stops
    tryCatch({
      agg_stops <- read_parquet(agg_stops_path)
      required_cols <- c("stop_id", "stop_name", "daytype")
      if (!all(required_cols %in% names(agg_stops))) {
        stop("agg_stops missing required columns: ", 
             paste(setdiff(required_cols, names(agg_stops)), collapse = ", "))
      }
      cat("  ✓ agg_stops.parquet valid (", nrow(agg_stops), " rows)\n")
    }, error = function(e) {
      stop("Error reading agg_stops: ", e$message)
    })
    
    # Check geo
    tryCatch({
      geo_layers <- st_layers(geo_path)
      if (!"pt_route_geom" %in% geo_layers$name) {
        stop("geo.gpkg missing 'pt_route_geom' layer. Available: ", 
             paste(geo_layers$name, collapse = ", "))
      }
      pt_route <- st_read(geo_path, layer = "pt_route_geom", quiet = TRUE)
      cat("  ✓ geo.gpkg valid (", nrow(pt_route), " routes)\n")
    }, error = function(e) {
      stop("Error reading geo: ", e$message)
    })
  }
  
  # Create directory
  if (dir.exists(target_dir)) {
    warning("Directory already exists: ", target_dir)
    response <- readline("Overwrite? (yes/no): ")
    if (tolower(response) != "yes") {
      cat("Cancelled.\n")
      return(FALSE)
    }
  } else {
    dir.create(target_dir, recursive = TRUE)
  }
  
  # Copy files
  cat("\nCopying files...\n")
  file.copy(pt_stops_path, file.path(target_dir, "pt_stops.parquet"), overwrite = TRUE)
  cat("  ✓ pt_stops.parquet\n")
  
  file.copy(agg_stops_path, file.path(target_dir, "agg_stops.parquet"), overwrite = TRUE)
  cat("  ✓ agg_stops.parquet\n")
  
  file.copy(geo_path, file.path(target_dir, "geo.gpkg"), overwrite = TRUE)
  cat("  ✓ geo.gpkg\n")
  
  # Success message
  cat("\n✓ Version added successfully!\n\n")
  cat("Next steps:\n")
  cat("1. Open app_multiversion.R\n")
  cat("2. Add this line to the gtfs_versions list:\n")
  cat('   "', version_name, '" = "data/', version_folder, '"\n\n', sep = "")
  
  return(TRUE)
}


#' List all available GTFS versions in data/ folder
#' 
#' @return data.frame with version information
list_gtfs_versions <- function() {
  data_dir <- "data"
  
  if (!dir.exists(data_dir)) {
    cat("No data directory found.\n")
    return(invisible(NULL))
  }
  
  folders <- list.dirs(data_dir, full.names = FALSE, recursive = FALSE)
  folders <- folders[folders != ""]
  
  if (length(folders) == 0) {
    cat("No version folders found in data/\n")
    return(invisible(NULL))
  }
  
  results <- data.frame(
    folder = character(),
    has_pt_stops = logical(),
    has_agg_stops = logical(),
    has_geo = logical(),
    pt_stops_rows = integer(),
    agg_stops_rows = integer(),
    routes_count = integer(),
    file_size_mb = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (folder in folders) {
    folder_path <- file.path(data_dir, folder)
    
    pt_stops_file <- file.path(folder_path, "pt_stops.parquet")
    agg_stops_file <- file.path(folder_path, "agg_stops.parquet")
    geo_file <- file.path(folder_path, "geo.gpkg")
    
    has_pt <- file.exists(pt_stops_file)
    has_agg <- file.exists(agg_stops_file)
    has_geo <- file.exists(geo_file)
    
    pt_rows <- NA
    agg_rows <- NA
    routes_cnt <- NA
    
    if (has_pt) {
      tryCatch({
        pt_rows <- nrow(read_parquet(pt_stops_file))
      }, error = function(e) NA)
    }
    
    if (has_agg) {
      tryCatch({
        agg_rows <- nrow(read_parquet(agg_stops_file))
      }, error = function(e) NA)
    }
    
    if (has_geo) {
      tryCatch({
        routes_cnt <- nrow(st_read(geo_file, layer = "pt_route_geom", quiet = TRUE))
      }, error = function(e) NA)
    }
    
    # Calculate total size
    all_files <- c(pt_stops_file, agg_stops_file, geo_file)
    total_size <- sum(file.size(all_files[file.exists(all_files)]), na.rm = TRUE) / (1024^2)
    
    results <- rbind(results, data.frame(
      folder = folder,
      has_pt_stops = has_pt,
      has_agg_stops = has_agg,
      has_geo = has_geo,
      pt_stops_rows = pt_rows,
      agg_stops_rows = agg_rows,
      routes_count = routes_cnt,
      file_size_mb = round(total_size, 2),
      stringsAsFactors = FALSE
    ))
  }
  
  cat("\nAvailable GTFS Versions:\n")
  cat("========================\n\n")
  print(results, row.names = FALSE)
  
  # Check for incomplete versions
  incomplete <- results[!(results$has_pt_stops & results$has_agg_stops & results$has_geo), ]
  if (nrow(incomplete) > 0) {
    cat("\n⚠ Warning: Incomplete versions detected:\n")
    print(incomplete$folder)
  }
  
  invisible(results)
}


#' Remove a GTFS version
#' 
#' @param version_folder Folder name to remove (e.g., "2025_08_28")
#' @param confirm Whether to ask for confirmation
#' 
#' @return TRUE if successful
remove_gtfs_version <- function(version_folder, confirm = TRUE) {
  target_dir <- file.path("data", version_folder)
  
  if (!dir.exists(target_dir)) {
    cat("Version not found:", target_dir, "\n")
    return(FALSE)
  }
  
  if (confirm) {
    cat("This will permanently delete:", target_dir, "\n")
    response <- readline("Are you sure? (yes/no): ")
    if (tolower(response) != "yes") {
      cat("Cancelled.\n")
      return(FALSE)
    }
  }
  
  unlink(target_dir, recursive = TRUE)
  cat("✓ Removed:", version_folder, "\n")
  cat("\nRemember to update gtfs_versions list in app_multiversion.R\n")
  
  return(TRUE)
}


#' Validate a GTFS version folder
#' 
#' @param version_folder Folder name to validate
#' 
#' @return List with validation results
validate_gtfs_version <- function(version_folder) {
  target_dir <- file.path("data", version_folder)
  
  if (!dir.exists(target_dir)) {
    cat("Folder not found:", target_dir, "\n")
    return(invisible(NULL))
  }
  
  cat("Validating GTFS version:", version_folder, "\n")
  cat("========================================\n\n")
  
  results <- list(valid = TRUE, errors = c(), warnings = c())
  
  # Check files exist
  pt_stops_file <- file.path(target_dir, "pt_stops.parquet")
  agg_stops_file <- file.path(target_dir, "agg_stops.parquet")
  geo_file <- file.path(target_dir, "geo.gpkg")
  
  if (!file.exists(pt_stops_file)) {
    results$errors <- c(results$errors, "Missing pt_stops.parquet")
    results$valid <- FALSE
  }
  if (!file.exists(agg_stops_file)) {
    results$errors <- c(results$errors, "Missing agg_stops.parquet")
    results$valid <- FALSE
  }
  if (!file.exists(geo_file)) {
    results$errors <- c(results$errors, "Missing geo.gpkg")
    results$valid <- FALSE
  }
  
  if (!results$valid) {
    cat("✗ Validation failed:\n")
    for (err in results$errors) cat("  -", err, "\n")
    return(invisible(results))
  }
  
  # Validate pt_stops
  tryCatch({
    pt_stops <- read_parquet(pt_stops_file)
    required_cols <- c("stop_id", "stop_name", "mode", "x", "y")
    missing <- setdiff(required_cols, names(pt_stops))
    if (length(missing) > 0) {
      results$errors <- c(results$errors, 
                         paste("pt_stops missing columns:", paste(missing, collapse = ", ")))
      results$valid <- FALSE
    } else {
      cat("✓ pt_stops.parquet:", nrow(pt_stops), "stops\n")
      cat("  Modes:", paste(unique(pt_stops$mode), collapse = ", "), "\n")
    }
  }, error = function(e) {
    results$errors <- c(results$errors, paste("pt_stops error:", e$message))
    results$valid <- FALSE
  })
  
  # Validate agg_stops
  tryCatch({
    agg_stops <- read_parquet(agg_stops_file)
    required_cols <- c("stop_id", "stop_name", "daytype")
    missing <- setdiff(required_cols, names(agg_stops))
    if (length(missing) > 0) {
      results$errors <- c(results$errors, 
                         paste("agg_stops missing columns:", paste(missing, collapse = ", ")))
      results$valid <- FALSE
    } else {
      cat("✓ agg_stops.parquet:", nrow(agg_stops), "records\n")
      cat("  Day types:", paste(unique(agg_stops$daytype), collapse = ", "), "\n")
    }
  }, error = function(e) {
    results$errors <- c(results$errors, paste("agg_stops error:", e$message))
    results$valid <- FALSE
  })
  
  # Validate geo
  tryCatch({
    layers <- st_layers(geo_file)
    if (!"pt_route_geom" %in% layers$name) {
      results$errors <- c(results$errors, "geo.gpkg missing 'pt_route_geom' layer")
      results$valid <- FALSE
    } else {
      pt_route <- st_read(geo_file, layer = "pt_route_geom", quiet = TRUE)
      cat("✓ geo.gpkg:", nrow(pt_route), "routes\n")
      cat("  CRS:", st_crs(pt_route)$input, "\n")
    }
  }, error = function(e) {
    results$errors <- c(results$errors, paste("geo error:", e$message))
    results$valid <- FALSE
  })
  
  # Final summary
  cat("\n")
  if (results$valid) {
    cat("✓ Validation successful!\n")
  } else {
    cat("✗ Validation failed:\n")
    for (err in results$errors) cat("  -", err, "\n")
  }
  
  if (length(results$warnings) > 0) {
    cat("\n⚠ Warnings:\n")
    for (warn in results$warnings) cat("  -", warn, "\n")
  }
  
  invisible(results)
}


#' Auto-generate gtfs_versions list code
#' 
#' @return Character vector with R code to paste into app
generate_versions_code <- function() {
  data_dir <- "data"
  folders <- list.dirs(data_dir, full.names = FALSE, recursive = FALSE)
  folders <- folders[folders != ""]
  
  if (length(folders) == 0) {
    cat("No version folders found.\n")
    return(invisible(NULL))
  }
  
  cat("Copy this code into app_multiversion.R:\n")
  cat("==========================================\n\n")
  cat("gtfs_versions <- list(\n")
  
  for (i in seq_along(folders)) {
    folder <- folders[i]
    # Convert folder name to display name (e.g., 2025_08_28 -> 2025-08-28)
    display_name <- gsub("_", "-", folder)
    
    cat('  "', display_name, '" = "data/', folder, '"', sep = "")
    if (i < length(folders)) cat(",")
    cat("\n")
  }
  
  cat(")\n")
  
  invisible(NULL)
}

# Print usage instructions when sourced
cat("\n=== GTFS Version Management Tools ===\n\n")
cat("Available functions:\n")
cat("  add_gtfs_version()      - Add a new version\n")
cat("  list_gtfs_versions()    - List all versions\n")
cat("  remove_gtfs_version()   - Remove a version\n")
cat("  validate_gtfs_version() - Validate a version\n")
cat("  generate_versions_code()- Generate config code\n\n")
cat("Example usage:\n")
cat("  list_gtfs_versions()\n")
cat('  validate_gtfs_version("2025_08_28")\n')
cat("  generate_versions_code()\n\n")
