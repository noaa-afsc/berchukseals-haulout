create_deploy_details <- function(deploy_details_file) {
  deploy_tbl <- readr::read_csv(deploy_details_file)
  
  con <- dbConnect(
    odbc(),
    dsn = "PostgreSQL pep",
    uid = keyringr::get_kc_account("pgpep_londonj"),
    pwd = keyringr::decrypt_kc_pw("pgpep_londonj")
  )
  
  deploy_db <- tbl(con, in_schema("telem","tbl_tag_deployments")) %>% 
    dplyr::select(speno, deploy_lat, deploy_long, capture_lat, capture_long) %>% 
    dplyr::mutate(deploy_lat = if_else(is.na(deploy_lat), capture_lat, deploy_lat),
                  deploy_long = if_else(is.na(deploy_long), capture_long, deploy_long)) %>% 
    collect()
  
  dbDisconnect(con)
  
  deploy_tbl <- deploy_tbl %>% 
    # dplyr::select(speno, species, location, region, latitude, longitude) %>% 
    left_join(deploy_db, by = "speno") %>% 
    mutate(latitude = coalesce(latitude,deploy_lat),
           longitude = coalesce(longitude, deploy_long))
}