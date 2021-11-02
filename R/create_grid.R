create_grid_sf <- function(data_sf) {
  stopifnot(
    "PEP Postgres Database Not Available; did you start VPN? ;)" =
      is_up("161.55.120.122", "5432")
  )

  con <- dbConnect(RPostgres::Postgres(),dbname = 'pep',
                   host = '161.55.120.122',
                   port = 5432,
                   user = keyringr::get_kc_account("pgpep_londonj"),
                   password = keyringr::decrypt_kc_pw("pgpep_londonj"))
  on.exit(dbDisconnect(con))
  sf::st_read(con,
              query = "SELECT * FROM base.geo_analysis_grid") %>%
    st_filter(st_bbox(data_sf) %>% st_as_sfc())
}
