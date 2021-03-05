##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author Josh.London
##' @export
get_timeline_data <- function() {

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

  timeline_db <- tbl(con, in_schema("telem","tbl_wc_histos_timeline_qa")) %>%
    dplyr::select(deployid,timeline_start_dt, percent_dry)
  deployments_db <- tbl(con, in_schema("telem","tbl_tag_deployments")) %>%
    dplyr::select(speno, deployid, tag_family, deploy_dt, end_dt)
  spenos_db <- tbl(con, in_schema("capture","for_telem"))

  timeline_data <- timeline_db  %>%
    left_join(deployments_db, by = 'deployid') %>%
    left_join(spenos_db, by = 'speno') %>%
    filter(species %in% c('Bearded seal', 'Ribbon seal', 'Spotted seal')) %>%
    collect()

  timeline_data <- timeline_data %>%
    group_by(speno, species, sex, age, timeline_start_dt) %>%
    summarize(n_tags = n(),
              percent_dry = ifelse(
                n_tags == 1, percent_dry, percent_dry[tag_family == "SPOT"]
              )) %>%
    filter(month(timeline_start_dt) %in% c(3,4,5,6,7)) %>%
    mutate(unique_day =
             glue("{year(timeline_start_dt)}",
                  "{yday(timeline_start_dt)}",
                  .sep = "_"))

  dbDisconnect(con)

  return(timeline_data)

}
