library(shiny)
library(sf)
library(tidyverse)
library(bslib)
library(mapgl)
library(arrow)

# con <- dbConnect(drv = duckdb::duckdb(), dbdir = "data/bus_expansion.duckdb")

pt_stop_sf <- read_parquet("data/pt_stops.parquet") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE) %>% filter(!is.na(mode))

agg_stop <- read_parquet("data/agg_stops.parquet") %>%
  select(-c(day_cnt, hours_cnt, x, y)) %>% relocate(daytype, .before = stop_name) %>%
  relocate(routes_list, .after = unique_routes_cnt)

pt_route <- st_read("data/geo.gpkg", layer = "pt_route_geom") %>% mutate(dist_km = round(path_dist_m / 1000, 2)) %>%
  select(-path_dist_m)

# Prepare named vector: names are labels (stop names), values are IDs
stop_choices <- setNames(as.character(pt_stop_sf$stop_id), pt_stop_sf$stop_name)

ui <- page_sidebar(
  title = "Brisbane LGA Public Transport Stops and Services - Service Period : 2025.08.28-10.27 from Translink GTFS",
  sidebar = sidebar(
    selectizeInput("search_stop_id", "Search Stop by ID or Name",
      choices = stop_choices,
      selected = "",
      options = list(placeholder = "Type or select a Stop ID or Stop Name", searchField = c("label", "value"))),
    sliderInput("slider", "Show Route Geometry Distance greater than :",
      value = 0, min = 0, max = 25),
    sliderInput("maxheadway", "Show Stop Maximum Headway less than (Minutes):",
      value = 15, min = 5, max = 120),
    verbatimTextOutput("search_info")),

  # Collapsible card using accordion
  accordion(
    id = "schedule_panel",
    accordion_panel(
      "Stop Service information Summary",
      conditionalPanel(
        condition = "input.search_stop_id != ''",
        DT::dataTableOutput("schedule_table")
      ),
      value = "schedule"
    )
  ),
  card(
    full_screen = TRUE,
    maplibreOutput("map")
  )
)

server <- function(input, output, session) {
  observe({
    updateSelectizeInput(session, "search_stop_id", choices = stop_choices, selected = "", server = TRUE)
  })

  filtered_stops <- reactive({
    req(input$search_stop_id)
    # Trim whitespace for safety
    search_id <- trimws(as.character(input$search_stop_id))
    pt_stop_sf %>% filter(as.character(stop_id) == search_id | tolower(stop_name) == tolower(search_id))
  })

  output$map <- renderMaplibre({
    m <- maplibre(style = carto_style("positron")) |>
      fit_bounds(pt_stop_sf, animate = FALSE) |>
      add_circle_layer(id = "pt_stops",
        source = pt_stop_sf,
        circle_stroke_color = "#ffffff",
        circle_color = match_expr("mode", values = pt_stop_sf$mode %>% unique(),
          stops = c("grey", "orange", "blue")),
        hover_options = list(circle_radius = 8, circle_color = "#ffff99"),
        circle_radius =  3) |>
      add_line_layer(source = pt_route, id = "route",
        line_color = match_expr("route_type", values = pt_stop_sf$mode %>% unique(),
          stops = c("grey", "orange", "blue")), line_cap = "butt",
        tooltip = "trip_headsign") |>
      add_categorical_legend(legend_title = "Stop Mode", values = pt_stop_sf$mode %>% unique(),
        colors = c("grey", "orange", "blue"), patch_shape = "hexagon")
    m
  })
  observeEvent(filtered_stops(), {
    stops_to_highlight <- filtered_stops()
    proxy <- maplibre_proxy("map")
    # Clear old highlighted stops layer if it exists
    proxy |> clear_layer("highlighted_stops")
    if (nrow(stops_to_highlight) > 0) {
      proxy |>
        add_circle_layer(
          id = "highlighted_stops",
          source = stops_to_highlight,
          circle_color = "red",
          circle_radius = 8
        ) |>
        set_view(
          center = st_coordinates(stops_to_highlight)[1, ] %>% as.vector(),
          zoom = 13
        )
    }
  })
  # route distance slider
  observeEvent(input$slider, {
    maplibre_proxy("map") |>
      set_filter("route",
        list(">", get_column("dist_km"), input$slider))
  })

  observeEvent(input$maxheadway, {
    headway_id <- agg_stop %>% filter(max_headway_in_minutes <= input$maxheadway) %>% distinct(stop_id) %>%
      pull() %>% as.character()
    stops_to_highlight_headway <- pt_stop_sf %>% filter(stop_id %in% headway_id)
    proxy <- maplibre_proxy("map")
    proxy |> clear_layer("highlighted_stops_headway")
    if (nrow(stops_to_highlight_headway) > 0) {
      proxy |>
        add_circle_layer(
          id = "highlighted_stops_headway",
          source = stops_to_highlight_headway,
          circle_color = "yellow",
          circle_radius = 6,
          tooltip = "stop_name"
        )
    }
  })

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

  output$schedule_table <- DT::renderDataTable(
    {
      req(input$search_stop_id)
      sid <- trimws(as.character(input$search_stop_id))
      agg_stop %>%
        filter(as.character(stop_id) == sid) %>%
        select(-stop_id)
    },
    options = list(scrollX = TRUE,
      pageLength = 2, autoWidth = TRUE),
    width = "100%")

}

shinyApp(ui, server)
