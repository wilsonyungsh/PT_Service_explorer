#' get_bne_pt_route_geom
#'
#' This function is used to get public transport route geometry intersecting with Brisbane LGA
#' @param gtfs_source default to translink api for gtfs files, can also take local path.
#' @return returns dataset in designated file format based on the specified statement.
#' @export
#' @examples
get_bne_pt_route_geom <- function(gtfs_source = "https://gtfsrt.api.translink.com.au/GTFS/SEQ_GTFS.zip") {
  ## get bne lga boundary as ref
  bne_lga <- strayr::read_absmap(name = "lga2022", remove_year_suffix = TRUE) %>%
    filter(lga_code == 31000)

  ## Create PT routes
  # read gtfs
  message("Read in GTFS...")
  seq <- read_gtfs(gtfs_source)

  # get pt unique routes shapes (per shape_id)
  message("connect the shape points to line....")
  shape_sf <- seq$shapes %>% st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>% group_by(shape_id) %>%
    summarise(geometry = st_cast(st_combine(geometry), "LINESTRING")) %>%
    mutate(path_dist_m = st_length(geometry))

  # join geometry shape with route id
  message("Associating route lines with additional information....")
  pt_route_geom <-
    seq$routes %>% distinct(route_id, route_type, route_short_name, route_color, route_text_color) %>%
    mutate(route_type = case_match(route_type, 0 ~ "TRAM", 2 ~ "RAIL", 3 ~ "BUS", 4 ~ "FERRY") # ,
      # version = str_extract(file_url, pattern = "\\d{8}")
    ) %>%
    left_join(seq$trips %>% distinct(route_id, shape_id, trip_headsign, direction_id),
      by = "route_id") %>%
    left_join(shape_sf, by = "shape_id") %>% st_as_sf() %>%
    st_filter(bne_lga)
  message("writing out output....")
  r <-
    list("pt_route_geom" = pt_route_geom,
      "gtfs" = seq)

  return(r)
}
