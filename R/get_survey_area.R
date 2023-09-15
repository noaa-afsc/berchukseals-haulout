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
  
  boss_area_sf <- read_sf(con, query = boss_lines_qry) |> 
    dplyr::filter(effort == "On", st_length(geom) > units::set_units(0,m)) |> 
    concaveman::concaveman(concavity = 4) |> 
    st_simplify(dTolerance = 25000, preserveTopology = TRUE)
  
  chess_area_sf <- read_sf(con, query = chess_lines_qry) |> 
    dplyr::filter(effort_type == "ON", st_length(geom) > units::set_units(0,m)) |> 
    concaveman::concaveman(concavity = 6) |> 
    st_simplify(dTolerance = 10000, preserveTopology = TRUE)

  survey_area <- st_union(boss_area_sf, chess_area_sf)
  
  return(survey_area)
}
  