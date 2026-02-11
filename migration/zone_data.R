source("~/Documents/git/bcc_data_sourcing/r/query_bcc_api.R")


zones <-
  query_bcc_api(dataset = "city_plan_2014", output_format = "geojson",
    select_statement = "zone_code,lvl1_zone,lvl2_zone",
    where_statement = "zone_code in ('MU','LDR','LMR','MDR','HDR')")


zones %>% st_write("data/common_datasets.gpkg", layer = "zone_overlay")
