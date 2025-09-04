#' get_full_pt_schedule
#'
#' This function is used to produce full public transport schedule from gtfs file.
#' @param gtfs_source default to translink api for gtfs files, can also take local path.
#' @return returns a list contains of summary information, such as service period, stop count..etc as well as the full schedule data.
#' @import tidyverse,tidytransit,assertthat
#' @export
#' @examples
get_full_pt_schedule <- function(gtfs_zip_path = "https://gtfsrt.api.translink.com.au/GTFS/SEQ_GTFS.zip") {
  # helper function to check file path
  # check_file_path <- function(path) {
  #   assertthat::assert_that( assertthat::is.string(path), msg = "Path must be a string.")  # Ensure it's a string
  #   assertthat::assert_that(file.exists(path), msg = "File does not exist at the provided path.")
  # }
  #
  # Replace 'your_file_path_here' with an actual file path on your system.
  # check_file_path(gtfs_zip_path)

  # load gtfs using tidytransit
  g <- tidytransit::read_gtfs(gtfs_zip_path)
  message("Loading gtfs zip file")

  # enrich trips with route info
  trip_info <- g$trips %>%
    left_join(g$routes, by = "route_id") %>%
    select(-c(route_url, block_id)) %>%
    mutate(mode = case_match(route_type, 0 ~ "TRAM", 2 ~ "RAIL", 3 ~ "BUS", 4 ~ "FERRY"))
  message("Enriching trips with route information")

  # Enrich stop time with trip information - key: trip_id
  trip_stop_times <- g$stop_times %>%
    left_join(trip_info, by = "trip_id")
  message("Enriching stop times with trip info")

  # Expand 'calendar' into individual dates, group by service ids (for recurring services)
  full_service_dates <- g$calendar %>% rowwise() %>%
    mutate(service_date = if_else(start_date == end_date, list(start_date), list(seq.Date(start_date, end_date, by = "day"))),
      service_period = if_else(start_date == end_date, as.character(start_date), paste0(start_date, "|", end_date))) %>%
    select(-c(start_date, end_date)) %>%
    unnest(service_date) %>%
    pivot_longer(-c(service_id, service_date, service_period), names_to = "dow", values_to = "flag") %>%
    mutate(dow = substr(str_to_title(dow), 1, 3), service_date_dow = wday(service_date, label = TRUE)) %>%
    filter(flag == 1 & dow == service_date_dow) %>%
    select(-c(service_date_dow, flag)) %>%
    group_by(service_id, service_period) %>% summarise(service_date = list(service_date))
  message("Reshape caldenar date per service id")
  # join stop time with full service dates
  trip_schedule <-
    trip_stop_times %>% left_join(full_service_dates, by = "service_id", relationship = "many-to-many") %>%
    unnest(service_date) %>% mutate(dow = wday(service_date, label = TRUE)) %>%
    select(mode, service_period, route_id, route_short_name, route_long_name, route_desc,
      trip_id, trip_headsign, direction_id, dow, service_date, departure_time, arrival_time,
      stop_id, stop_sequence, shape_id, service_id, route_color, route_text_color) %>%
    arrange(route_id, trip_id, stop_sequence)
  message("Producing trip schedule with all dates, all trips at each stop")

  # Summary stats
  summary <- trip_schedule %>% group_by(mode) %>%
    summarise(route_cnt = n_distinct(route_id), trip_cnt = n_distinct(trip_id), stop_cnt = n_distinct(stop_id),
      service_period = paste0(min(service_date), " - ", max(service_date)))
  print(summary)
  return(list(summary = summary,
    trip_schedule = trip_schedule))
}
