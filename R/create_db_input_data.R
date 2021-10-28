create_source_data <- function(locs_sf, timeline_data) {

  loc_qual_tbl <- tribble(
    ~quality, ~error_radius,
    "3", 250,
    "2", 500,
    "1", 1500,
    "0", 2500,
    "A", 2500,
    "B", 2500
  )

  locs_daily <- locs_sf %>%
    dplyr::filter(quality %in% c("3","2","1","0","A","B")) %>%
    left_join(loc_qual_tbl, by = "quality") %>%
    st_transform(3571) %>%
    mutate(x = st_coordinates(.)[,"X"],
           y = st_coordinates(.)[,"Y"]) %>%
    st_set_geometry(NULL) %>%
    mutate(error_radius.x = ifelse(is.na(error_radius.x),
                                          error_radius.y,
                                          error_radius.x)) %>%
    rename(error_radius = error_radius.x) %>%
    dplyr::select(-error_radius.y) %>%
    mutate(error_radius = ifelse(type %in% c("GPS","FastGPS"),
                                        50,error_radius),
                  error_radius = ifelse(type %in% c("User"),
                                        50,error_radius)) %>%
    group_by(speno,unique_day,age,sex,species) %>%
    summarise(x = weighted.mean(x,1/error_radius),
                     y = weighted.mean(y,1/error_radius))

  tbl_percent_locs <- timeline_data %>%
    full_join(locs_daily,
                     by = c("speno","unique_day","age",
                            "sex","species")) %>%
    arrange(speno,unique_day,timeline_start_dt) %>%
    group_by(speno) %>% nest() %>%
    mutate(start_idx = map_int(data,~ which.max(!is.na(.x$x))),
           data = map2(data, start_idx, ~ slice(.x, .y:nrow(.x)))) %>%
    dplyr::select(-start_idx) %>%
    unnest(cols = c(data)) %>%
    mutate(fill_xy = ifelse(is.na(x), TRUE, FALSE)) %>%
    group_by(speno) %>%
    fill(x,y) %>%
    ungroup() %>%
    dplyr::filter(!is.na(percent_dry)) %>%
    dplyr::filter(!speno %in% c("EB2005_5995","PL2006_5984","PL2006_5987")) %>%
    dplyr::filter(!is.na(x)) %>%
    st_as_sf(coords = c("x","y")) %>%
    st_set_crs(3571) %>%
    rename(haulout_dt = timeline_start_dt) %>%
    dplyr::select(speno,species,age,sex,haulout_dt,percent_dry,n_tags,fill_xy)

  stopifnot(
    "PEP Postgres Database Not Available; did you start VPN? ;)" =
      is_up("161.55.120.122", "5432")
  )

  con <- dbConnect(RPostgres::Postgres(),dbname = 'pep',
                   host = '161.55.120.122',
                   port = 5432,
                   user = keyringr::get_kc_account("pgpep_sa"),
                   password = keyringr::decrypt_kc_pw("pgpep_sa"))
  on.exit(dbDisconnect(con))

  st_write(obj = tbl_percent_locs,
           dsn = con,
           delete_layer = TRUE,
           layer = SQL("telem.res_iceseal_haulout")
  )

  dbExecute(con, "ALTER TABLE telem.res_iceseal_haulout RENAME COLUMN geometry TO geom")
  dbExecute(con, "SELECT telem.fxn_iceseal_pred_idx();")
  dbExecute(con, "SELECT telem.fxn_iceseal_haulout_cov();")

  qry <- {
    "SELECT *
  FROM telem.res_iceseal_haulout_cov
  WHERE
  EXTRACT(MONTH FROM haulout_dt) IN (3,4,5,6,7) AND
  rast_vwnd IS NOT NULL AND
  species != 'Ph' AND
  speno != 'EB2009_7010' AND
  speno != 'EB2009_3002' "
  }

  sf::st_read(con, query = qry)
}
