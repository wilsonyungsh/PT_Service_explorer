# Standalone map
pacman::p_load(dplyr, sf, mapgl, arrow)

# Define zone colors (moved outside function so it's available globally)
zone_color <- c(
  LDR = "#FFDCDC",
  LMR = "#FFA4A4",
  MDR = "#FF6565",
  HDR = "#AA0000",
  MU  = "#FF7800"
)

# Load all three datasets
pt_stop_sf <- read_parquet("data/2026_02_10/pt_stops.parquet") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE) %>%
  filter(!is.na(mode)) %>%
  mutate(tooltip_info = paste0(stop_name, " (", stop_id, ") - ", mode))

agg_stop <- read_parquet("data/2026_02_10/agg_stops.parquet") %>%
  # select(-c(day_cnt, hours_cnt, x, y)) %>%
  relocate(daytype, .before = stop_name) %>%
  relocate(routes_list, .after = unique_routes_cnt)

pt_route <- st_read("data/2026_02_10/geo.gpkg", layer = "pt_route_geom", quiet = TRUE) %>%
  mutate(dist_km = round(path_dist_m / 1000, 2)) %>%
  select(-path_dist_m) %>%
  mutate(route_tooltip = paste0("Route ", route_short_name, ": ", trip_headsign, " (", dist_km, " km)"))

zone_overlay <- st_read("data/common_datasets.gpkg", layer = "zone_overlay", quiet = TRUE)

# precompute
unique_modes <- unique(pt_stop_sf$mode)
mode_colors <- c("grey", "orange", "blue")


mapboxgl(style = carto_style("positron")) |>
  fit_bounds(pt_stop_sf, animate = FALSE) |>
  # Add zone layer FIRST (so it appears underneath)
  add_fill_layer(
    source = zone_overlay,
    id = "zone",
    fill_opacity = 0.7, tooltip = "zone_code",
    fill_color = match_expr(
      column = "zone_code",
      values = unique(zone_overlay$zone_code),
      stops = unname(zone_color)
    )
  ) |>
  # Then add routes
  add_line_layer(
    source = pt_route,
    id = "route",
    line_color = match_expr("route_type",
      values = unique_modes,
      stops = mode_colors),
    line_cap = "butt",
    tooltip = "route_tooltip", visibility = "none"
  ) |>
  # Add stops on top
  add_circle_layer(
    id = "pt_stops",
    source = pt_stop_sf,
    circle_stroke_color = "#ffffff",
    circle_color = match_expr("mode",
      values = unique_modes,
      stops = mode_colors),
    hover_options = list(circle_radius = 15, circle_color = "#ffff99"),
    circle_radius = 3,
    tooltip = "tooltip_info",
  ) |>
  turf_buffer(layer_id = "pt_stops",
    radius = 300,
    units = "meters",
    source_id = "buffer") %>%
  add_fill_layer(id = "buffer_map",
    source = "buffer",
    fill_color = "green", fill_opacity = 0.1, visibility = "none") %>%
  turf_buffer(layer_id = "pt_stops",
    radius = 400,
    units = "meters",
    source_id = "buffer2") %>%
  add_fill_layer(id = "buffer_map2",
    source = "buffer2",
    fill_color = "yellow", fill_opacity = 0.1, visibility = "none") %>%
  # Add legend with all layers
  add_categorical_legend(
    legend_title = "Layers",
    values = c(unique_modes, names(zone_color), "300m", "400m"),
    colors = c(mode_colors, unname(zone_color), "green", "yellow"),
    patch_shape = "hexagon"
  ) %>%
  # Add layer controls
  add_layers_control(
    layers = c("Public Transport Stops" = "pt_stops",
      "Routes" = "route",
      "Zoning" = "zone",
      "Buffer 300m" = "buffer_map",
      "Buffer 400m" = "buffer_map2"),
    position = "top-right"
  )






mapboxgl(style = carto_style("positron")) |>
  fit_bounds(pt_stop_sf, animate = FALSE) |>
  add_circle_layer(
    id = "pt_stops",
    source = pt_stop_sf,
    circle_stroke_color = "#ffffff",
    circle_color = match_expr("mode",
      values = unique_modes,
      stops = mode_colors),
    hover_options = list(circle_radius = 15, circle_color = "#ffff99"),
    circle_radius = 3,
    tooltip = "tooltip_info"
  ) |>
  turf_buffer(
    layer_id = "pt_stops",  # Match your circle layer ID
    radius = 10,             # 10 km
    units = "kilometers",    # or "meters" with 10000
    source_id = "buffer"
  ) |>
  add_fill_layer(
    id = "buffer_map",
    source = "buffer",
    fill_color = "green",
    fill_opacity = 0.3  # Add some transparency
  )
