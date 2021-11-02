library(here)
source(here('R/get_locs_sf.R'))
source(here('R/get_timeline_data.R'))
source(here('R/create_db_input_data.R'))

timeline_data = get_timeline_data()
locs_sf = get_locs_sf()
db_input_data = create_db_input_data(timeline_data, locs_sf)

save(db_input_data, file = here::here("data/tbl_percent_locs.Rdata"))
