pacman::p_load(shiny, DBI, duckdb, dplyr, mapgl, sf)

ui <- fluidPage(
  titlePanel("Public Transport Service Explorer (DuckDB + mapgl)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mode", "Select Mode:", choices = "All"),
      actionButton("refresh", "Refresh Data")
    ),
    mainPanel(
      mapglOutput("map", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  # Connect to DuckDB
  con <- DBI::dbConnect(RSQLite::SQLite(), "pt_service_explorer/pt_explorer.sqlite")

  # Load available modes
  # modes <- dbGetQuery(con, "SELECT DISTINCT mode FROM stops")$mode
  # updateSelectInput(session, "mode", choices = c("All", modes))

  stops_data <- reactive({
    query <- "SELECT stop_id, stop_name, mode, lat, lon FROM stops"
    if (input$mode != "All") {
      query <- paste0(query, " WHERE mode = '", input$mode, "'")
    }
    dbGetQuery(con, query)
  })

  # Render MapGL
  output$map <- renderMapgl({
    df <- stops_data()

    mapgl(
      access_token = Sys.getenv("MAPBOX_TOKEN"),  # 👈 you need your Mapbox token
      style = "mapbox://styles/mapbox/streets-v12",
      center = c(mean(df$lon), mean(df$lat)),
      zoom = 11
    ) %>%
      add_markers(
        lng = df$lon,
        lat = df$lat,
        popup = paste0("<b>", df$stop_name, "</b><br/>Mode: ", df$mode)
      )
  })

  # Close connection when app stops
  onStop(function() {
    dbDisconnect(con, shutdown = TRUE)
  })
}

shinyApp(ui, server)
