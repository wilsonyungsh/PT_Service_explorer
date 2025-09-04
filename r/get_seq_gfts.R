get_seq_gtfs <- function(x) {
  if (!dir.exists("data")) {
    message("data directory doesn't exist")
    dir.create("data")
    message("data folder created....")
  }
  gtfs_url <- "https://gtfsrt.api.translink.com.au/GTFS/SEQ_GTFS.zip"
  file_name <- str_c(str_remove_all(Sys.Date(), "-"), "_", tolower(basename(gtfs_url)))
  local_path <- str_c("data/", file_name)
  curl::curl_download(url = gtfs_url, destfile = local_path, quiet = FALSE)
  message(paste0("data is saved on ", local_path))
  return(local_path)
}
