pacman::p_load(shiny, sf, tidyverse, bslib, mapgl, arrow)


# con<-dbConnect(drv = RSQLite::SQLite(),dbdir = "data/pt_explorer.sqlite")
# dbListTables(con)
full_schedule <- read_parquet("data/full_schedule.parquet")

pt_stop_sf <- full_schedule %>% distinct(stop_id, stop_id, x, y) %>% st_as_sf(coords = c("x", "y"), crs = 4326)

ui <- page_sidebar(
  title = "Brisbane LGA Public Transport Stops and Services",
  sidebar = sidebar(),
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
}

shinyApp(ui, server)
