get_locs_sf <- function(adfg_locations, nsb_locations) {

  tryCatch({
    con <- dbConnect(RPostgres::Postgres(),
                      dbname = 'pep', 
                      host = Sys.getenv('PEP_PG_IP'),
                      user = keyringr::get_kc_account("pgpep_londonj"),
                      password = keyringr::decrypt_kc_pw("pgpep_londonj"))
  },
  error = function(cond) {
    print("Unable to connect to Database.")
  })
  on.exit(dbDisconnect(con))

  deployments_db <- tbl(con, in_schema("telem","tbl_tag_deployments")) %>%
    dplyr::select(speno, deployid, tag_family, deploy_dt, end_dt) %>%
    rename(tag_type = tag_family) %>% collect()

  spenos_db <- tbl(con, in_schema("capture","for_telem")) %>% collect()

  locs_qry <- "SELECT deployid, ptt, type, error_radius, locs_dt, quality, geom as geometry
              FROM telem.geo_wc_locs_qa WHERE qa_status != 'tag_actively_transmitting';"

  locs_sf <- read_sf(con, query = locs_qry) %>%
    filter(!deployid %in% c("PL2017_9001_16U2112")) %>%
    left_join(deployments_db, by = 'deployid') %>%
    left_join(spenos_db, by = 'speno') %>%
    filter(species %in% c('Bearded seal', 'Ribbon seal', 'Spotted seal')) %>%
    filter(lubridate::month(locs_dt) %in% c(3,4,5,6,7)) %>%
    mutate(unique_day =
             glue::glue("{lubridate::year(locs_dt)}",
                  "{lubridate::yday(locs_dt)}",
                  .sep = "_"))
  dbDisconnect(con)

  locs_sf <- locs_sf %>% bind_rows(adfg_locations) %>%
    bind_rows(nsb_locations)

  return(locs_sf)

}
