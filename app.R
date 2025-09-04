pacman::p_load(shiny, sf, tidyverse, bslib, mapgl, arrow)

# con <- dbConnect(drv = duckdb::duckdb(), dbdir = "data/bus_expansion.duckdb")

pt_stop_sf <- read_parquet("data/pt_stops.parquet") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE) %>% filter(!is.na(mode))

agg_stop <- read_parquet("data/agg_stops.parquet") %>%
  select(-c(day_cnt, hours_cnt, x, y)) %>% relocate(daytype, .before = stop_name)

pt_route <- st_read("data/geo.gpkg", layer = "pt_route_geom")

ui <- page_sidebar(
  title = "Brisbane LGA Public Transport Stops and Services",
  sidebar = sidebar(
    textInput("search_stop_id", "Search Stop ID", value = ""),
    verbatimTextOutput("search_info"),
    tableOutput("schedule_table")
  ),
  card(
    full_screen = TRUE,
    maplibreOutput("map")
  )
)

server <- function(input, output, session) {

  filtered_stops <- reactive({
    req(input$search_stop_id)
    # Trim whitespace for safety
    search_id <- trimws(as.character(input$search_stop_id))
    pt_stop_sf %>% filter(as.character(stop_id) == search_id)
  })

  output$map <- renderMaplibre({
    m <- maplibre(style = carto_style("positron")) |>
      fit_bounds(pt_stop_sf, animate = FALSE) |>
      add_circle_layer(id = "bne_pt_stops",
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

    stops_to_highlight <- filtered_stops()
    if (nrow(stops_to_highlight) > 0) {
      m <- m |> add_circle_layer(
        id = "highlighted_stops",
        source = stops_to_highlight,
        circle_color = "red",
        circle_radius = 8
      ) %>%
        # Zoom in to the stop at zoom level 12
        set_view(
          center = st_coordinates(stops_to_highlight)[1, ],
          zoom = 15
        )
    }

    m
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

  output$schedule_table <- renderTable({
    req(input$search_stop_id)
    sid <- trimws(as.character(input$search_stop_id))
    agg_stop %>%
      filter(as.character(stop_id) == sid) %>%
      select(-stop_id)
  })
}

shinyApp(ui, server)
