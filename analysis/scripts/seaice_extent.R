library(tidyverse)
library(lubridate)
library(glue)
library(DBI)
library(dbplyr)
library(sf)

load(here::here("data/dat_sf.Rdata"))

xmin <- st_bbox(dat.sf)$xmin %>% as.integer()
ymin <- st_bbox(dat.sf)$ymin %>% as.integer()
xmax <- st_bbox(dat.sf)$xmax %>% as.integer()
ymax <- st_bbox(dat.sf)$ymax %>% as.integer()
epsg <- 3571

con <- dbConnect(
  odbc::odbc(),
  dsn = "PostgreSQL pep",
  uid = keyringr::get_kc_account("pgpep_londonj"),
  pwd = keyringr::decrypt_kc_pw("pgpep_londonj"),
  sslmode = "disable"
)

extent_list <- vector(mode = "list")

for(y in 2005:2020) {
  print(y)
  start_date <- glue::glue('{y}-02-15')
  end_date <- glue::glue('{y}-07-15')

  sql<- glue_sql("
           SELECT *
  FROM environ.fxn_sea_ice_extent_by_date(
    0.15,{xmin},{ymin},{xmax},{ymax},
    {epsg},
    {start_date},{end_date} )",
  .con = con)
  extent_list[[as.character(y)]] <- DBI::dbGetQuery(con, sql)
}

dbDisconnect(con)
