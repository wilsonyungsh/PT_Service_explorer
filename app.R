pacman::p_load(shiny, sf, tidyverse, bslib, mapgl, arrow)


# con<-dbConnect(drv = RSQLite::SQLite(),dbdir = "data/pt_explorer.sqlite")
# dbListTables(con)
full_schedule <- read_parquet("data/full_schedule.parquet")

pt_stop_sf <- full_schedule %>% distinct(stop_id, stop_name, x, y) %>% st_as_sf(coords = c("x", "y"), crs = 4326)

ui <- page_sidebar(
  title = "Brisbane LGA Public Transport Stops and Services",
  sidebar = sidebar(
    selectInput("sel_daytype", "Day Type", choices = c("weekday", "weekend")),
    verbatimTextOutput("clicked_feature"),
    tableOutput("schedule_table")
  ),
  card(
    full_screen = TRUE,
    maplibreOutput("map")
  )
)

server <- function(input, output, session) {
  output$map <- renderMaplibre({
    maplibre(style = carto_style("positron")) |>
      fit_bounds(pt_stop_sf, animate = FALSE) |>
      add_circle_layer(id = "bne_pt_stops",
        source = pt_stop_sf,
        circle_color = "blue",
        circle_radius =  3)
  })
  output$clicked_feature <- renderPrint({
    req(input$map_feature_click)
    props <- input$map_feature_click$properties
    # Show specific properties, e.g., stop_name and stop_id
    list(
      stop_id = props$stop_id,
      stop_name = props$stop_name
    )
  })
  output$schedule_table <- renderTable({
    req(input$map_feature_click, input$sel_daytype)
    sid <- input$map_feature_click$properties$stop_id %>% pluck(1)
    if (is.null(sid) || length(sid) == 0 || is.na(sid)) {
      return(NULL)
    }
    daytype <- input$sel_daytype
    full_schedule %>%
      dplyr::filter(stop_id == sid, daytype == daytype) # Filter rows
  })


}

shinyApp(ui, server)
