get_timeline_data <- function(adfg_timelines, nsb_timelines) {

  stopifnot(
    "PEP Postgres Database Not Available; did you start VPN? ;)" =
      pingr::is_up("161.55.120.122", "5432")
  )

  con <- dbConnect(
    odbc(),
    dsn = "PostgreSQL pep",
    uid = keyringr::get_kc_account("pgpep_londonj"),
    pwd = keyringr::decrypt_kc_pw("pgpep_londonj")
  )

  timeline_db <- tbl(con, in_schema("telem","tbl_wc_histos_timeline_qa")) %>%
    dplyr::select(deployid,timeline_start_dt, percent_dry)
  deployments_db <- tbl(con, in_schema("telem","tbl_tag_deployments")) %>%
    dplyr::select(speno, deployid, tag_family, deploy_dt, end_dt)
  spenos_db <- tbl(con, in_schema("capture","for_telem"))

  timeline_data <- timeline_db  %>%
    left_join(deployments_db, by = 'deployid') %>%
    left_join(spenos_db, by = 'speno') %>%
    filter(!deployid %in% c("PL2017_9001_16U2112")) %>%
    filter(species %in% c('Bearded seal', 'Ribbon seal', 'Spotted seal')) %>%
    collect() %>%
    filter(lubridate::month(timeline_start_dt) %in% c(3,4,5,6,7)) %>%
    mutate(unique_day =
             glue::glue("{lubridate::year(timeline_start_dt)}",
                        "{lubridate::yday(timeline_start_dt)}",
                        .sep = "_"))

  timeline_data <- timeline_data %>%
    bind_rows(adfg_timelines) %>%
    bind_rows(nsb_timelines) %>%
    group_by(speno, species, sex, age, unique_day, timeline_start_dt) %>%
    summarize(n_tags = n(),
              percent_dry = ifelse(
                n_tags == 1, percent_dry, percent_dry[tag_family == "SPOT"]
              ))


  dbDisconnect(con)

  return(timeline_data)

}
