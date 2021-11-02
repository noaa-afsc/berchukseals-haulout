get_nsb_deployments <- function(con) {
  stopifnot(
    "PEP Postgres Database Not Available; did you start VPN? ;)" =
      pingr::is_up("161.55.120.122", "5432")
  )

  con <- dbConnect(
    odbc::odbc(),
    dsn = "PostgreSQL pep",
    uid = keyringr::get_kc_account("pgpep_londonj"),
    pwd = keyringr::decrypt_kc_pw("pgpep_londonj")
  )
  on.exit(odbc::dbDisconnect(con))

  deploy_qry <- "select speno, deployid, common_name as species, age_class as age, sex, tag_family, deploy_dt, end_dt
from telem.nsb_capture c
left join telem.nsb_deploy d
on c.id = d.capture_id
left join telem.lku_tag_family
using (tag_family_lku)
left join capture.lku_species
using (species_lku)
left join capture.lku_age_class
using (age_class_lku)
left join capture.lku_sex
using (sex_lku)"

  nsb_deployments <- tbl(con, sql(deploy_qry)) %>%
    dplyr::select(speno, deployid, species, age, sex, tag_family, deploy_dt, end_dt) %>%
    collect()
  nsb_deployments2 <- tibble::tribble(
    ~speno, ~deployid, ~species, ~age, ~sex, ~tag_family, ~deploy_dt, ~end_dt,
    "EB2019BW02", 149452, "Bearded seal", "Pup", "Female", "SPLASH", "2019-09-19 18:30:00", "2020-05-24 17:30:00",
    "EB2019BW02", 171333, "Bearded seal", "Pup", "Female", "SPOT", "2019-09-19 18:30:00", "2020-08-08 06:30:00"
  ) %>%
    dplyr::mutate(deploy_dt = lubridate::ymd_hms(deploy_dt),
                  end_dt = lubridate::ymd_hms(end_dt))

  nsb_deployments <- dplyr::bind_rows(nsb_deployments, nsb_deployments2)

  return(nsb_deployments)
}

get_nsb_locs <- function(nsb_deployments) {
  stopifnot(
    "PEP Postgres Database Not Available; did you start VPN? ;)" =
      pingr::is_up("161.55.120.122", "5432")
  )

  con <- dbConnect(
    odbc::odbc(),
    dsn = "PostgreSQL pep",
    uid = keyringr::get_kc_account("pgpep_londonj"),
    pwd = keyringr::decrypt_kc_pw("pgpep_londonj")
  )
  on.exit(odbc::dbDisconnect(con))

  locs2 <- readr::read_csv(here::here("data_raw/nsb/iceSeals_NSB_Alaska_vonDuyke.csv")) %>%
    janitor::clean_names() %>%
    dplyr::filter(tag_local_identifier %in% c(149452, 171333)) %>%
    dplyr::select(deployid = tag_local_identifier,
                  locs_dt = timestamp,
                  quality = argos_lc,
                  location_long, location_lat) %>%
    dplyr::mutate(deployid = as.integer(deployid)) %>%
    sf::st_as_sf(coords = c("location_long","location_lat"), crs = 4326)

  locs_qry <- "SELECT deployid, loc_dt as locs_dt, argos_lc as quality, loc_geom as geometry
              FROM telem.nsb_locs;"

  nsb_locations <- read_sf(con, query = locs_qry) %>%
    dplyr::bind_rows(locs2) %>%
    left_join(nsb_deployments, by = 'deployid') %>%
    filter(species %in% c('Bearded seal', 'Ribbon seal', 'Spotted seal')) %>%
    filter(lubridate::month(locs_dt) %in% c(3,4,5,6,7)) %>%
    rename(ptt = deployid) %>%
    mutate(species_code = case_when(
      species == "Bearded seal" ~ "EB",
      species == "Ribbon seal" ~ "HF",
      species == "Spotted seal" ~ "PL"
    )) %>%
    mutate(deployid = glue::glue("{species_code}",
                                 "{lubridate::year(locs_dt)}_{ptt}")) %>%
    mutate(unique_day =
             glue::glue("{lubridate::year(locs_dt)}",
                        "{lubridate::yday(locs_dt)}",
                        .sep = "_")) %>%
    dplyr::select(-species_code) %>%
    relocate(deployid,ptt)

  return(nsb_locations)
}

get_nsb_timelines <- function(nsb_deployments) {
  stopifnot(
    "PEP Postgres Database Not Available; did you start VPN? ;)" =
      pingr::is_up("161.55.120.122", "5432")
  )

  con <- dbConnect(
    odbc::odbc(),
    dsn = "PostgreSQL pep",
    uid = keyringr::get_kc_account("pgpep_londonj"),
    pwd = keyringr::decrypt_kc_pw("pgpep_londonj")
  )
  on.exit(odbc::dbDisconnect(con))

  nsb_timelines2 <- wcUtils::read_histos(
    here::here('data_raw/nsb/iceSeals_NSB_Alaska_vonDuyke_updatesOnly_20211008-Histos.csv'),
    dt_fmt = "%Y-%m-%d %H:%M:%S") %>%
    wcUtils::tidyTimelines() %>%
    dplyr::mutate(deployid = as.integer(deployid)) %>%
    dplyr::filter(deployid %in% c(149452, 171333)) %>%
    dplyr::select(-hist_type) %>% as_tibble()

  nsb_timelines <- tbl(con, in_schema("telem","nsb_histos_timeline")) %>%
    dplyr::select(deployid,timeline_start_dt, percent_dry) %>%
    collect() %>%
    dplyr::bind_rows(nsb_timelines2) %>%
    left_join(nsb_deployments, by = 'deployid') %>%
    filter(species %in% c('Bearded seal', 'Ribbon seal', 'Spotted seal')) %>%
    filter(lubridate::month(timeline_start_dt) %in% c(3,4,5,6,7)) %>%
    rename(ptt = deployid) %>%
    mutate(species_code = case_when(
      species == "Bearded seal" ~ "EB",
      species == "Ribbon seal" ~ "HF",
      species == "Spotted seal" ~ "PL"
    )) %>%
    mutate(deployid = glue::glue("{species_code}",
                                 "{lubridate::year(timeline_start_dt)}_{ptt}")) %>%
    mutate(unique_day =
             glue::glue("{lubridate::year(timeline_start_dt)}",
                        "{lubridate::yday(timeline_start_dt)}",
                        .sep = "_")) %>%
    dplyr::select(-species_code) %>%
    relocate(deployid,ptt) %>%
    dplyr::arrange(deployid,timeline_start_dt)
}
