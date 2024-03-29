get_survey_area <- function() {
  
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
  
  boss_lines_qry <- "SELECT * FROM surv_boss.geo_tracks_by_effort;"
  chess_lines_qry <- "SELECT * FROM surv_chess.geo_track_by_effort;"
  jobss_lines_qry <- "SELECT * FROM surv_jobss.geo_track_by_effort;"
  
  ru_area_2012 <- read_sf(here::here('data_raw/ru_tracks/ru_tracks_2012/Routes_2012.shp')) |> 
    sf::st_transform(3571) |> 
    concaveman::concaveman(concavity = 8) |> 
    st_simplify(dTolerance = 10000, preserveTopology = TRUE)
  
  ru_area_apr_2013 <- read_sf(here::here('data_raw/ru_tracks/ru_tracks_Apr_2013/ru_tracks_Apr_2013.shp')) |> 
    sf::st_transform(3571) |> 
    concaveman::concaveman(concavity = 8) |> 
    sf::st_zm() |> 
    st_simplify(dTolerance = 10000, preserveTopology = TRUE)
  
  ru_area_may_2013 <- read_sf(here::here('data_raw/ru_tracks/ru_tracks_May_2013/ru_tracks_May_2013.shp')) |> 
    sf::st_transform(3571) |> 
    concaveman::concaveman(concavity = 10) |> 
    sf::st_zm() |> 
    st_simplify(dTolerance = 10000, preserveTopology = TRUE)
  
  ru_area_2016 <- read_sf(here::here('data_raw/ru_tracks/ru_tracks_2016/2016_Russian_tracks.shp')) |> 
    sf::st_transform(3571) |> 
    concaveman::concaveman(concavity = 8) |> 
    st_simplify(dTolerance = 10000, preserveTopology = TRUE)
  
  boss_area_sf <- read_sf(con, query = boss_lines_qry) |> 
    dplyr::filter(effort == "On", st_length(geom) > units::set_units(0,m)) |> 
    sf::st_transform(3571) |> 
    concaveman::concaveman(concavity = 4) |> 
    st_simplify(dTolerance = 25000, preserveTopology = TRUE)
  
  chess_area_sf <- read_sf(con, query = chess_lines_qry) |> 
    dplyr::filter(effort_type == "ON", st_length(geom) > units::set_units(0,m)) |> 
    sf::st_transform(3571) |> 
    concaveman::concaveman(concavity = 6) |> 
    st_simplify(dTolerance = 10000, preserveTopology = TRUE)
  
  jobss_area_sf <- read_sf(con, query = jobss_lines_qry) |> 
    dplyr::filter(effort_reconciled == "ON", st_length(st_makeline) > units::set_units(0,m)) |> 
    sf::st_transform(3571) |> 
    concaveman::concaveman(concavity = 6) |> 
    st_simplify(dTolerance = 10000, preserveTopology = TRUE)
  
  

  survey_area <- st_union(boss_area_sf, chess_area_sf) |> 
    st_union(jobss_area_sf) |> 
    st_union(ru_area_2012) |> 
    st_union(ru_area_apr_2013) |> 
    st_union(ru_area_may_2013) |> 
    st_union(ru_area_2016)
  
  return(survey_area)
}
  