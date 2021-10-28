create_model_input <- function(dat.sf) {
  dat.sf  %>%
  mutate(dry = round(percent_dry/100)) %>%
  mutate(hour_utc = lubridate::hour(haulout_dt),
         yday = lubridate::yday(haulout_dt),
         year = lubridate::year(haulout_dt)) %>%
  mutate(coords_x = st_coordinates(st_transform(.,4326))[,1],
         coords_y = st_coordinates(st_transform(.,4326))[,2],
         solar_hour = solaR::local2Solar(haulout_dt,coords_x) %>% lubridate::hour(),
         sin1 = sin(pi*solar_hour/12),
         cos1 = cos(pi*solar_hour/12),
         sin2 = sin(pi*solar_hour/6),
         cos2 = cos(pi*solar_hour/6),
         sin3 = sin(pi*solar_hour/4),
         cos3 = sin(pi*solar_hour/4),
         day = (yday-120)/10,
         day2 = day^2,
         day3 = day^3
  ) %>%
  mutate(precip = rast_acpcp,
         temp2 = (rast_air2m-270)/27,
         temp0 = (rast_airsfc-270)/27,
         pressure = (rast_prmsl-100000)/10000,
         wind = sqrt(rast_uwnd^2 + rast_vwnd^2)/10
  ) %>%
  mutate(northing = st_coordinates(.)[,2]/mean(st_coordinates(.)[,2]),
         north_of_bering = case_when(
           coords_y > 66.5 ~ TRUE,
           TRUE ~ FALSE
         )) %>%
  mutate(species = forcats::as_factor(species),
         year = forcats::as_factor(year),
         solar_hour = forcats::as_factor(solar_hour)) %>%
  arrange(speno,haulout_dt) %>%
  group_by(speno) %>%
  mutate(ar1_id = paste(speno,create_ar1_id(haulout_dt),sep="_")) %>%
  ungroup() %>%
  mutate(ar1_id = forcats::as_factor(ar1_id),
         speno = forcats::as_factor(speno)) %>%
  relocate(ar1_id, .after = speno) %>%
  dplyr::select(-c(coords_x, coords_y)) %>%
  group_by(ar1_id) %>% mutate(time_vec = row_number(ar1_id) - 1) %>%
  ungroup() %>%
  mutate(age_sex = case_when(
    age == "YOUNG OF YEAR" ~ "YOUNG OF YEAR",
    age == "SUBADULT" ~ "SUBADULT",
    age == "ADULT" & sex == "F" ~ "ADULT.F",
    age == "ADULT" & sex == "M" ~ "ADULT.M"),

    age_sex_inter = case_when(
      age == "YOUNG OF YEAR" ~ "ADULT.F",
      TRUE ~ age_sex),

    age_sex = forcats::as_factor(age_sex) %>%
      forcats::fct_relevel(c("ADULT.F","ADULT.M","SUBADULT","YOUNG OF YEAR")),
    age_sex_inter = forcats::as_factor(age_sex_inter) %>%
      forcats::fct_relevel(c("ADULT.F","ADULT.M","SUBADULT"))
  )
}
