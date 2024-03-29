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

create_ar1_start <- function(ar1_id) {
  rle_ar1 = ar1_id %>% rle()
  end = cumsum(rle_ar1$lengths)
  start = c(1, lag(end)[-1] + 1)
  
  ar1_start <- vector(length = length(ar1_id))
  ar1_start[seq_along(ar1_start)] <- FALSE
  ar1_start[start] <- TRUE
  
  return(ar1_start)
}