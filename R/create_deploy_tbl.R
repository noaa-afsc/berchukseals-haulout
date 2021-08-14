create_deploy_tbl <- function(data_sf) {
  deploy_tbl <- data_sf %>%
    tibble::as_tibble() %>%
    dplyr::group_by(speno,species, sex, age) %>%
    dplyr::summarise(min_dt = min(haulout_dt),
                     max_dt = max(haulout_dt),
                     n_days = difftime(max_dt, min_dt, units = "days"),
                     n_hours = n()
    ) %>%
    dplyr::select(-c("min_dt","max_dt"))
}
