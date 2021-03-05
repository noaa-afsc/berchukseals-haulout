##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author Josh.London
##' @export
get_locs_sf <- function() {

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

  deployments_db <- tbl(con, in_schema("telem","tbl_tag_deployments")) %>%
    select(speno, deployid, tag_family, deploy_dt, end_dt) %>% collect()

  spenos_db <- tbl(con, in_schema("capture","for_telem")) %>% collect()

  locs_qry <- "SELECT deployid,
                locs_dt,
                type,
                quality,
                error_radius,
                geom
              FROM telem.geo_wc_locs_qa"

  locs_sf <- read_sf(con, query = locs_qry) %>%
    left_join(deployments_db, by = 'deployid') %>%
    left_join(spenos_db, by = 'speno') %>%
    filter(species %in% c('Bearded seal', 'Ribbon seal', 'Spotted seal')) %>%
    filter(month(locs_dt) %in% c(3,4,5,6,7)) %>%
    mutate(unique_day =
             glue("{year(locs_dt)}",
                  "{yday(locs_dt)}",
                  .sep = "_"))
  dbDisconnect(con)

  return(locs_sf)

}
