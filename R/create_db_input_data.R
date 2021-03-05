##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param timeline_data
##' @param locs_sf
##' @return
##' @author Josh.London
##' @export
create_db_input_data <- function(timeline_data, locs_sf) {

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
    filter(quality != "Z") %>%
    left_join(loc_qual_tbl, by = "quality") %>%
    st_transform(3571) %>%
    mutate(x = st_coordinates(.)[,"X"],
           y = st_coordinates(.)[,"Y"]) %>%
    st_set_geometry(NULL) %>%
    mutate(error_radius.x = ifelse(is.na(error_radius.x),
                                          error_radius.y,
                                          error_radius.x)) %>%
    rename(error_radius = error_radius.x) %>%
    select(-error_radius.y) %>%
    mutate(error_radius = ifelse(type %in% c("GPS","FastGPS"),
                                        50,error_radius),
                  error_radius = ifelse(type == "User",
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
    select(-start_idx) %>%
    unnest(cols = c(data)) %>%
    mutate(fill_xy = ifelse(is.na(x), TRUE, FALSE)) %>%
    group_by(speno) %>%
    fill(x,y) %>%
    ungroup() %>%
    filter(!is.na(percent_dry)) %>%
    filter(!speno %in% c("EB2005_5995","PL2006_5984","PL2006_5987")) %>%
    filter(!is.na(x)) %>%
    st_as_sf(coords = c("x","y")) %>%
    st_set_crs(3571) %>%
    rename(haulout_dt = timeline_start_dt) %>%
    select(speno,species,age,sex,haulout_dt,percent_dry,n_tags,fill_xy)

  stopifnot(
    "PEP Postgres Database Not Available; did you start VPN? ;)" =
      is_up("161.55.120.122", "5432")
  )

  con <- dbConnect(
    odbc(),
    dsn = "PostgreSQL pep",
    uid = get_kc_account("pgpep_londonj"),
    pwd = decrypt_kc_pw("pgpep_londonj")
  )

  st_write(obj = tbl_percent_locs,
           dsn = con,
           layer = "res_iceseal_haulout_jml",
           layer_options = "OVERWRITE=true")

  return(tbl_percent_locs)

}
