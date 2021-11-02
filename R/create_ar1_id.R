create_ar1_id <- function(h_dt) {
  ar1_id <- tibble(
    t_seq = seq(from = h_dt[1], to = h_dt[length(h_dt)], by = "hour",)
  ) %>%
    left_join(tibble(h_dt = h_dt, haulout_dt = h_dt), by = c("t_seq" = "h_dt")) %>%
    mutate(x = is.na(haulout_dt),
           rle = rlenc_id(x)) %>%
    filter(!is.na(haulout_dt)) %>%
    pull(rle)

  return(ar1_id)
}
