create_grid_sf <- function(data_sf) {

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
  on.exit(odbc::dbDisconnect(con))
  
  sf::st_read(con,
              query = "SELECT * FROM base.geo_analysis_grid") %>%
    st_filter(st_bbox(data_sf) %>% st_as_sfc())
}
