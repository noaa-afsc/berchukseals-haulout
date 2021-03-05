library(tidyverse)
library(DBI)
library(dbplyr)
library(sf)

con <- dbConnect(
  odbc::odbc(),
  dsn = "PostgreSQL pep",
  uid = keyringr::get_kc_account("pgpep_londonj"),
  pwd = keyringr::decrypt_kc_pw("pgpep_londonj"),
  sslmode = "disable"
)

load(here::here('data/dat_sf.Rdata'))

grid.sf <- sf::st_read(con,
                       query = "SELECT * FROM base.geo_analysis_grid") %>%
  sf::st_crop(st_bbox(dat.sf) %>% st_as_sfc())

cov_means <- tbl(con, in_schema("base","tbl_analysis_grid_cov_wx")) %>%
  filter(cell %in% !!grid.sf$cell) %>%
  filter(month(fdatetime_range_start) %in% c(3,4,5,6,7),
         year(fdatetime_range_start) != '2004') %>%
  group_by(year = year(fdatetime_range_start),
           yday = yday(fdatetime_range_start)) %>%
  summarise(precip = mean(rast_acpcp, na.rm=TRUE),
            temp2 = (mean(rast_air2m, na.rm=TRUE)-270),
            temp0 = (mean(rast_airsfc, na.rm=TRUE)-270),
            pressure = (mean(rast_prmsl, na.rm=TRUE)-100000),
            wind = sqrt(mean(rast_uwnd, na.rm=TRUE)^2 + mean(rast_vwnd, na.rm=TRUE)^2)
            )

cov_means_daily <- tbl(con, in_schema("base","tbl_analysis_grid_cov_wx")) %>%
  filter(cell %in% !!grid.sf$cell) %>%
  filter(month(fdatetime_range_start) %in% c(3,4,5,6,7),
         year(fdatetime_range_start) != '2004') %>%
  group_by(year = year(fdatetime_range_start),
           yday = yday(fdatetime_range_start),
           cell) %>%
  summarise(precip = mean(rast_acpcp, na.rm=TRUE),
            temp2 = (mean(rast_air2m, na.rm=TRUE)-270),
            temp_sfc = (mean(rast_airsfc, na.rm=TRUE)-270),
            pressure = (mean(rast_prmsl, na.rm=TRUE)-100000),
            wind = sqrt(mean(rast_uwnd, na.rm=TRUE)^2 + mean(rast_vwnd, na.rm=TRUE)^2)
  )

cov_means <- cov_means %>% collect()
save(cov_means, file = here::here('data/cov_means.Rdata'))
cov_means_daily <- cov_means_daily %>% collect()
save(cov_means_daily, file = here::here('data/cov_means_daily.Rdata'))



