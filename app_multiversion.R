library(shiny)
library(sf)
library(dplyr)
library(bslib)
library(bsicons)
library(mapgl)
library(arrow)

# ==============================================================================
# CONFIGURATION: Define available GTFS versions
# ==============================================================================
gtfs_versions <- list(
  "2025-08-28 to 2025-10-27" = "data/2025_08_28",
  "2026-02-10 to 2026-04-30" = "data/2026_02_10"
)

# Default version (latest)
default_version <- names(gtfs_versions)[length(gtfs_versions)]

# ==============================================================================
# HELPER FUNCTION: Load GTFS data for a specific version
# ==============================================================================
load_gtfs_data <- function(data_path) {
  message("Loading GTFS data from: ", data_path)

  # Load all three datasets
  pt_stop_sf <- read_parquet(file.path(data_path, "pt_stops.parquet")) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE) %>%
    filter(!is.na(mode)) %>%
    mutate(tooltip_info = paste0(stop_name, " (", stop_id, ") - ", mode))

  agg_stop <- read_parquet(file.path(data_path, "agg_stops.parquet")) %>%
    select(-c(day_cnt, hours_cnt, x, y)) %>%
    relocate(daytype, .before = stop_name) %>%
    relocate(routes_list, .after = unique_routes_cnt)

  pt_route <- st_read(file.path(data_path, "geo.gpkg"), layer = "pt_route_geom", quiet = TRUE) %>%
    mutate(dist_km = round(path_dist_m / 1000, 2)) %>%
    select(-path_dist_m) %>%
    mutate(route_tooltip = paste0("Route ", route_short_name, ": ", trip_headsign, " (", dist_km, " km)"))

  # Pre-compute choices
  stop_choices <- as.list(setNames(as.character(pt_stop_sf$stop_id), pt_stop_sf$stop_name))
  route_choices_vec <- unique(pt_route$route_short_name)
  route_choices <- as.list(setNames(route_choices_vec, route_choices_vec))

  # Return as list
  list(
    pt_stop_sf = pt_stop_sf,
    agg_stop = agg_stop,
    pt_route = pt_route,
    stop_choices = stop_choices,
    route_choices = route_choices,
    unique_modes = unique(pt_stop_sf$mode)
  )
}

# ==============================================================================
# UI DEFINITION
# ==============================================================================
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#3498db",
    base_font = font_google("Inter")
  ),

  # Custom CSS
  tags$head(
    tags$style(HTML("
      /* Smooth transitions */
      .selectize-input, .slider-container, .card {
        transition: all 0.2s ease;
      }

      /* Selectize styling */
      .selectize-input {
        border-radius: 6px;
        border: 1px solid #ddd;
      }

      .selectize-input:focus-within {
        border-color: #3498db;
        box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.15);
      }

      /* Slider labels */
      .irs-bar, .irs-from, .irs-to, .irs-single {
        background: #3498db !important;
      }

      /* Search info box */
      #search_info {
        background: #f8f9fa;
        border: none;
        font-size: 0.85rem;
        max-height: 120px;
        overflow-y: auto;
      }

      /* Accordion improvements */
      .accordion-button:not(.collapsed) {
        background-color: #e8f4f8;
        color: #2c3e50;
      }

      /* DataTable styling */
      .dataTables_wrapper {
        font-size: 0.9rem;
      }

      .dataTables_wrapper table {
        margin-top: 0 !important;
      }

      /* Map card */
      .card {
        border: none;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }

      /* Sidebar styling */
      .bslib-sidebar-layout > .sidebar {
        border-right: 1px solid #e0e0e0;
      }

      /* Empty state icons */
      .text-muted i {
        opacity: 0.3;
      }

      /* Version selector styling */
      #gtfs_version_select {
        background: #fff;
        border: 2px solid #3498db;
        font-weight: 500;
      }

      /* Loading indicator */
      .loading-indicator {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        background: rgba(255, 255, 255, 0.95);
        padding: 20px 40px;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.2);
        z-index: 9999;
      }
    "))
  ),

  # Top navigation bar with title and version selector
  div(
    class = "bg-primary text-white p-3 mb-0",
    style = "box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
    div(
      class = "d-flex justify-content-between align-items-center",
      div(
        h4("Brisbane Public Transport Explorer", class = "mb-1"),
        p(textOutput("version_info", inline = TRUE),
          class = "mb-0", style = "font-size: 0.9rem; opacity: 0.9;")
      ),
      div(
        style = "min-width: 280px;",
        selectInput(
          "gtfs_version_select",
          label = NULL,
          choices = names(gtfs_versions),
          selected = default_version,
          width = "100%"
        )
      )
    )
  ),

  # Main layout with sidebar and map
  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      bg = "#f8f9fa",

      # Search Section
      div(
        class = "mb-3",
        h5("Search & Filter", class = "mb-3"),

        selectizeInput(
          "search_stop_id",
          "Find Stop",
          choices = NULL,
          selected = NULL,
          options = list(
            placeholder = "Search by name or ID...",
            searchField = c("label", "value")
          )
        ),

        selectizeInput(
          "search_route",
          "Find Route",
          choices = NULL,
          selected = NULL,
          options = list(placeholder = "Search route number...")
        )
      ),

      # Filter Section
      div(
        class = "mb-3",
        div(
          class = "d-flex justify-content-between align-items-center mb-2",
          h6("Display Filters", class = "mb-0"),
          actionButton(
            "clear_filters",
            "Clear",
            class = "btn-sm btn-outline-secondary",
            style = "font-size: 0.8rem; padding: 2px 8px;"
          )
        ),

        sliderInput(
          "slider",
          "Route Distance (km)",
          value = 0,
          min = 0,
          max = 25,
          step = 0.5
        ),

        sliderInput(
          "maxheadway",
          "Max Headway (min)",
          value = 15,
          min = 5,
          max = 120,
          step = 5
        )
      ),

      # Info Section
      div(
        class = "bg-white p-3 rounded",
        style = "border-left: 3px solid #3498db;",
        h6("Selected Stop", class = "mb-2"),
        verbatimTextOutput("search_info", placeholder = TRUE)
      )
    ),

    # Main content area - Map takes priority
    div(
      style = "height: 100%;",

      # Map card (full height)
      card(
        full_screen = TRUE,
        height = "70vh",
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Interactive Map"),
          span(
            class = "text-muted",
            style = "font-size: 0.85rem;",
            "Click stops for details • Search to highlight"
          )
        ),
        maplibreOutput("map", height = "100%")
      ),

      # Collapsible details below map
      div(
        class = "mt-3",
        accordion(
          id = "details_accordion",
          open = FALSE,

          accordion_panel(
            title = "Stop Service Schedule",
            icon = bsicons::bs_icon("clock"),
            value = "schedule",
            conditionalPanel(
              condition = "input.search_stop_id != ''",
              DT::dataTableOutput("schedule_table")
            ),
            conditionalPanel(
              condition = "input.search_stop_id == ''",
              div(
                class = "text-center text-muted p-4",
                bsicons::bs_icon("search", size = "2rem"),
                p("Select a stop to view service details", class = "mt-2")
              )
            )
          ),

          accordion_panel(
            title = "Route Information",
            icon = bsicons::bs_icon("signpost"),
            value = "route_info",
            conditionalPanel(
              condition = "input.search_route != ''",
              DT::dataTableOutput("route_table")
            ),
            conditionalPanel(
              condition = "input.search_route == ''",
              div(
                class = "text-center text-muted p-4",
                bsicons::bs_icon("search", size = "2rem"),
                p("Select a route to view details", class = "mt-2")
              )
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  # Data cache to avoid reloading
  data_cache <- reactiveValues(
    versions = list(),
    current_version = NULL
  )

  # Pre-compute color scheme
  mode_colors <- c("grey", "orange", "blue")

  # ===========================================================================
  # REACTIVE: Load data based on selected version
  # ===========================================================================
  current_data <- reactive({
    req(input$gtfs_version_select)

    version_key <- input$gtfs_version_select

    # Check cache first
    if (!is.null(data_cache$versions[[version_key]])) {
      message("Using cached data for version: ", version_key)
      return(data_cache$versions[[version_key]])
    }

    # Load new version
    data_path <- gtfs_versions[[version_key]]

    # Show loading state
    showNotification(
      paste("Loading GTFS version:", version_key),
      duration = 2,
      type = "message"
    )

    data <- load_gtfs_data(data_path)

    # Cache it
    data_cache$versions[[version_key]] <- data
    data_cache$current_version <- version_key

    showNotification(
      "Data loaded successfully!",
      duration = 2,
      type = "message"
    )

    return(data)
  })

  # ===========================================================================
  # Update selectize inputs when version changes
  # ===========================================================================
  observeEvent(current_data(), {
    data <- current_data()

    # Clear current selections
    updateSelectizeInput(session, "search_stop_id",
      choices = data$stop_choices,
      selected = character(0),
      server = TRUE)

    updateSelectizeInput(session, "search_route",
      choices = data$route_choices,
      selected = character(0),
      server = TRUE)

    # Reset filters
    updateSliderInput(session, "slider", value = 0)
    updateSliderInput(session, "maxheadway", value = 15)
  })

  # ===========================================================================
  # Version info display
  # ===========================================================================
  output$version_info <- renderText({
    req(input$gtfs_version_select)
    paste("Service Period:", input$gtfs_version_select, "| Data: Translink GTFS")
  })

  # Debounced slider inputs
  slider_debounced <- reactive({
    input$slider
  }) %>% debounce(500)

  maxheadway_debounced <- reactive({
    input$maxheadway
  }) %>% debounce(500)

  # ===========================================================================
  # Filtered stops reactive
  # ===========================================================================
  filtered_stops <- reactive({
    req(input$search_stop_id, current_data())
    search_id <- trimws(as.character(input$search_stop_id))

    current_data()$pt_stop_sf %>%
      filter(
        as.character(stop_id) == search_id |
          tolower(stop_name) == tolower(search_id)
      )
  })

  # ===========================================================================
  # Filtered routes reactive
  # ===========================================================================
  filtered_routes <- reactive({
    req(input$search_route, current_data())
    selected_id <- trimws(as.character(input$search_route))
    current_data()$pt_route %>% filter(route_short_name == selected_id)
  })

  # ===========================================================================
  # Initialize map with current data
  # ===========================================================================
  output$map <- renderMaplibre({
    req(current_data())
    data <- current_data()

    maplibre(style = carto_style("positron")) |>
      fit_bounds(data$pt_stop_sf, animate = FALSE) |>
      add_circle_layer(
        id = "pt_stops",
        source = data$pt_stop_sf,
        circle_stroke_color = "#ffffff",
        circle_color = match_expr("mode",
          values = data$unique_modes,
          stops = mode_colors),
        hover_options = list(circle_radius = 15, circle_color = "#ffff99"),
        circle_radius = 3,
        tooltip = "tooltip_info"
      ) |>
      add_line_layer(
        source = data$pt_route,
        id = "route",
        line_color = match_expr("route_type",
          values = data$unique_modes,
          stops = mode_colors),
        line_cap = "butt",
        tooltip = "route_tooltip"
      ) |>
      add_categorical_legend(
        legend_title = "Stop Mode",
        values = data$unique_modes,
        colors = mode_colors,
        patch_shape = "hexagon"
      ) |>
      add_layers_control(
        layers = c("pt_stops", "route"),
        position = "top-right"
      )
  })

  # ===========================================================================
  # Clear filters button
  # ===========================================================================
  observeEvent(input$clear_filters, {
    updateSliderInput(session, "slider", value = 0)
    updateSliderInput(session, "maxheadway", value = 120)
    maplibre_proxy("map") |> clear_layer("highlighted_stops_headway")
  })

  # ===========================================================================
  # Observe stop search and highlight
  # ===========================================================================
  observeEvent(filtered_stops(), {
    stops_to_highlight <- filtered_stops()
    proxy <- maplibre_proxy("map")

    proxy |> clear_layer("highlighted_stops")

    if (nrow(stops_to_highlight) > 0) {
      coords <- st_coordinates(stops_to_highlight)[1, ]
      proxy |>
        add_circle_layer(
          id = "highlighted_stops",
          source = stops_to_highlight,
          circle_color = "red",
          circle_radius = 8,
          tooltip = "tooltip_info"
        ) |>
        set_view(
          center = as.vector(coords),
          zoom = 13
        )
    }
  })

  # ===========================================================================
  # Observe route search and highlight
  # ===========================================================================
  observeEvent(filtered_routes(), {
    routes_to_highlight <- filtered_routes()
    proxy <- maplibre_proxy("map")

    proxy |> clear_layer("highlighted_routes")

    if (nrow(routes_to_highlight) > 0) {
      proxy |>
        add_line_layer(
          id = "highlighted_routes",
          source = routes_to_highlight,
          line_color = "purple",
          line_width = 4,
          tooltip = "route_tooltip"
        ) |>
        fit_bounds(bbox = st_bbox(routes_to_highlight))
    }
  })

  # ===========================================================================
  # Route distance filter with debouncing
  # ===========================================================================
  observeEvent(slider_debounced(), {
    maplibre_proxy("map") |>
      set_filter("route",
        list(">", get_column("dist_km"), slider_debounced()))
  })

  # ===========================================================================
  # Headway filter with debouncing
  # ===========================================================================
  observeEvent(maxheadway_debounced(), {
    req(current_data())

    headway_stop_ids <- current_data()$agg_stop %>%
      filter(max_headway_in_minutes <= maxheadway_debounced()) %>%
      pull(stop_id) %>%
      as.character()

    stops_to_highlight_headway <- current_data()$pt_stop_sf %>%
      filter(stop_id %in% headway_stop_ids)

    proxy <- maplibre_proxy("map")
    proxy |> clear_layer("highlighted_stops_headway")

    if (nrow(stops_to_highlight_headway) > 0) {
      proxy |>
        add_circle_layer(
          id = "highlighted_stops_headway",
          source = stops_to_highlight_headway,
          circle_color = "yellow",
          circle_radius = 6,
          tooltip = "tooltip_info"
        )
    }
  })

  # ===========================================================================
  # Search info output
  # ===========================================================================
  output$search_info <- renderPrint({
    stops <- filtered_stops()
    if (nrow(stops) == 0) {
      "No stop found with that ID."
    } else {
      list(
        Found_Stop_ID = unique(stops$stop_id),
        Stop_Name = unique(stops$stop_name),
        Mode = unique(stops$mode)
      )
    }
  })

  # ===========================================================================
  # Schedule table
  # ===========================================================================
  output$schedule_table <- DT::renderDataTable(
    {
      req(input$search_stop_id, current_data())
      sid <- trimws(as.character(input$search_stop_id))

      current_data()$agg_stop %>%
        filter(as.character(stop_id) == sid) %>%
        select(-stop_id)
    },
    options = list(
      scrollX = TRUE,
      pageLength = 2,
      autoWidth = TRUE,
      dom = "tip"
    ),
    width = "100%")

  # ===========================================================================
  # Route table
  # ===========================================================================
  output$route_table <- DT::renderDataTable(
    {
      req(input$search_route, current_data())
      rid <- trimws(as.character(input$search_route))

      current_data()$pt_route %>%
        st_drop_geometry() %>%
        filter(as.character(route_short_name) == rid) %>%
        distinct(route_type, route_short_name, trip_headsign, direction_id, dist_km)
    },
    options = list(
      scrollX = TRUE,
      pageLength = 2,
      autoWidth = TRUE,
      dom = "tip"
    ),
    width = "100%")
}

# ==============================================================================
# RUN APP
# ==============================================================================
options(shiny.maxRequestSize = 100 * 1024^2)
shinyApp(ui, server)
