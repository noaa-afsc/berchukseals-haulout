create_data_sf <- function(locs_sf, source_data) {
  dat.sf <- source_data %>%
    filter(lubridate::yday(haulout_dt) < 197 ) %>%
    mutate(age = case_when(
      age %in% c("pup", "PUP", "p", "Pup") ~ "YOUNG OF YEAR",
      age %in% c("yearling","YEARLING","subadult","Subadult") ~ "SUBADULT",
      age %in% c("adult","ADULT","Adult") ~ "ADULT",
      TRUE ~ age),
      species = case_when(
        species == "bearded" ~ "Bearded seal",
        species == "spotted" ~ "Spotted seal",
        TRUE ~ species),
      sex = case_when(
        sex %in% c("Female","female") ~ "F",
        sex %in% c("Male","male","m") ~ "M",
        TRUE ~ sex)
    ) %>%
    filter(!is.na(age))

  deploy_meta <- locs_sf %>%
    as_tibble() %>%
    dplyr::select(speno, deploy_dt) %>%
    filter(speno %in% !!unique(dat.sf$speno)) %>%
    group_by(speno) %>%
    summarise(deploy_dt = min(deploy_dt, na.rm = TRUE))

  dat.sf <- dat.sf %>% left_join(deploy_meta) %>%
    mutate(bday = as.Date(paste0(lubridate::year(deploy_dt),"-03-01")),
           age_days = difftime(haulout_dt, bday, tz="UTC", units = "days"),
           age = case_when(
             age_days > 360 & age == "YOUNG OF YEAR" ~ "SUBADULT",
             TRUE ~ age
           )) %>%
    mutate(age = forcats::fct_relevel(age, c("ADULT", "SUBADULT", "YOUNG OF YEAR"))) %>%
    dplyr::select(-c(bday,age_days)) %>%
    relocate(deploy_dt, .after = age) %>%
    filter(!(species == "Bearded seal" & age == "YOUNG OF YEAR"))

  # read in world land polygons and buffer in by 100m; want to ensure only locations well
  world <- ne_countries(scale = "large", returnclass = "sf") %>%
    filter(continent != "Antarctica") %>%
    st_wrap_dateline() %>% st_transform(st_crs(dat.sf)) %>%
    st_buffer(-100)


  not_intersects <- function(x,y) {
    !sf::st_intersects(x,y)
  }

  dat.sf %>%
    sf::st_filter(st_combine(world), .predicate = not_intersects)
}
