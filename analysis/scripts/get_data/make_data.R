
source(here('R/get_locs_sf.R'))
source(here('R/get_timeline_data.R'))
source(here('R/create_db_input_data.R'))

timeline_data = get_timeline_data()
locs_sf = get_locs_sf()
db_input_data = create_db_input_data(timeline_data, locs_sf)

save(db_input_data, file = here::here("data/tbl_percent_locs.Rdata"))

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

st_write(obj = db_input_data,
         dsn = con,
         layer = "res_iceseal_haulout_jml",
         layer_options = "OVERWRITE=true")

dbDisconnect(con)
