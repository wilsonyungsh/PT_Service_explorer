library(shiny)
library(sf)
library(dplyr)
library(bslib)
library(bsicons)
library(mapgl)
library(arrow)

# Load data once at startup
pt_stop_sf <- read_parquet("data/pt_stops.parquet") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE) %>%
  filter(!is.na(mode)) %>%
  mutate(tooltip_info = paste0(stop_name, " (", stop_id, ") - ", mode))

agg_stop <- read_parquet("data/agg_stops.parquet") %>%
  select(-c(day_cnt, hours_cnt, x, y)) %>%
  relocate(daytype, .before = stop_name) %>%
  relocate(routes_list, .after = unique_routes_cnt)

pt_route <- st_read("data/geo.gpkg", layer = "pt_route_geom") %>%
  mutate(dist_km = round(path_dist_m / 1000, 2)) %>%
  select(-path_dist_m) %>%
  mutate(route_tooltip = paste0("Route ", route_short_name, ": ", trip_headsign, " (", dist_km, " km)"))

# Pre-compute choices as named lists (not vectors) to avoid jsonlite warnings
stop_choices <- as.list(setNames(as.character(pt_stop_sf$stop_id), pt_stop_sf$stop_name))
route_choices_vec <- unique(pt_route$route_short_name)
route_choices <- as.list(setNames(route_choices_vec, route_choices_vec))

# Pre-compute unique modes
unique_modes <- unique(pt_stop_sf$mode)
mode_colors <- c("grey", "orange", "blue")

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
    "))
  ),

  # Top navigation bar with title
  div(
    class = "bg-primary text-white p-3 mb-0",
    style = "box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
    h4("Brisbane Public Transport Explorer", class = "mb-1"),
    p("Service Period: 2025.08.28 - 10.27 | Data: Translink GTFS",
      class = "mb-0", style = "font-size: 0.9rem; opacity: 0.9;")
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

server <- function(input, output, session) {
  # Initialize selectize inputs once on startup
  isolate({
    updateSelectizeInput(session, "search_stop_id",
      choices = stop_choices,
      server = TRUE)
    updateSelectizeInput(session, "search_route",
      choices = route_choices,
      server = TRUE)
  })

  # Debounced slider inputs to reduce update frequency
  slider_debounced <- reactive({
    input$slider
  }) %>% debounce(500)

  maxheadway_debounced <- reactive({
    input$maxheadway
  }) %>% debounce(500)

  # Reactive for filtered stops - searches both ID and name
  filtered_stops <- reactive({
    req(input$search_stop_id)
    search_id <- trimws(as.character(input$search_stop_id))

    # Search in stop_choices to see if input matches a stop_id (value) or stop_name (name)
    # If user selected from dropdown, search_id will be the stop_id
    # If user typed, we need to check both
    pt_stop_sf %>%
      filter(
        as.character(stop_id) == search_id |
          tolower(stop_name) == tolower(search_id)
      )
  })

  # Reactive for filtered routes
  filtered_routes <- reactive({
    req(input$search_route)
    selected_id <- trimws(as.character(input$search_route))
    pt_route %>% filter(route_short_name == selected_id)
  })

  # Initialize map ONCE with all base layers
  output$map <- renderMaplibre({
    maplibre(style = carto_style("positron")) |>
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
      add_line_layer(
        source = pt_route,
        id = "route",
        line_color = match_expr("route_type",
          values = unique_modes,
          stops = mode_colors),
        line_cap = "butt",
        tooltip = "route_tooltip"
      ) |>
      add_categorical_legend(
        legend_title = "Stop Mode",
        values = unique_modes,
        colors = mode_colors,
        patch_shape = "hexagon"
      ) |>
      add_layers_control(
        layers = c("pt_stops", "route"),
        position = "top-right"
      )
  })

  # Clear filters button
  observeEvent(input$clear_filters, {
    updateSliderInput(session, "slider", value = 0)
    updateSliderInput(session, "maxheadway", value = 120)

    # Clear the headway highlight layer
    maplibre_proxy("map") |> clear_layer("highlighted_stops_headway")
  })

  # Observe stop search and highlight
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

  # Observe route search and highlight
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

  # Route distance filter with debouncing
  observeEvent(slider_debounced(), {
    maplibre_proxy("map") |>
      set_filter("route",
        list(">", get_column("dist_km"), slider_debounced()))
  })

  # Headway filter with debouncing and optimization
  observeEvent(maxheadway_debounced(), {
    # Pre-filter IDs first
    headway_stop_ids <- agg_stop %>%
      filter(max_headway_in_minutes <= maxheadway_debounced()) %>%
      pull(stop_id) %>%
      as.character()

    # Then filter spatial data
    stops_to_highlight_headway <- pt_stop_sf %>%
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

  # Search info output
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

  # Schedule table with caching
  output$schedule_table <- DT::renderDataTable(
    {
      req(input$search_stop_id)
      sid <- trimws(as.character(input$search_stop_id))

      agg_stop %>%
        filter(as.character(stop_id) == sid) %>%
        select(-stop_id)
    },
    options = list(
      scrollX = TRUE,
      pageLength = 2,
      autoWidth = TRUE,
      dom = "tip"  # Simplified DOM for better performance
    ),
    width = "100%")

  # Route table
  output$route_table <- DT::renderDataTable(
    {
      req(input$search_route)
      rid <- trimws(as.character(input$search_route))

      pt_route %>%
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

# Increase session timeout
options(shiny.maxRequestSize = 100 * 1024^2)  # 100MB max upload
shinyApp(ui, server)
