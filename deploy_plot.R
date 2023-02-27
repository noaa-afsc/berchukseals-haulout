library(googlesheets4)
library(ggplot2)
library(rnaturalearth)
library(ggspatial)
library(dplyr)
library(dbplyr)
library(odbc)

deploy_tbl <- read_sheet("1MpCiyAKWLBXufwnzW8QTnb2SsTkrKSuPlWdpWMnqoto")

con <- dbConnect(
  odbc(),
  dsn = "PostgreSQL pep",
  uid = keyringr::get_kc_account("pgpep_londonj"),
  pwd = keyringr::decrypt_kc_pw("pgpep_londonj")
)

deploy_db <- tbl(con, in_schema("telem","tbl_tag_deployments")) %>% 
  dplyr::select(speno, deploy_lat, deploy_long, capture_lat, capture_long) %>% 
  dplyr::mutate(deploy_lat = if_else(is.na(deploy_lat), capture_lat, deploy_lat),
                deploy_long = if_else(is.na(deploy_long), capture_long, deploy_long)) %>% 
  collect()

dbDisconnect(con)

deploy_tbl <- deploy_tbl %>% 
  # dplyr::select(speno, species, location, region, latitude, longitude) %>% 
  left_join(deploy_db, by = "speno") %>% 
  mutate(latitude = coalesce(latitude,deploy_lat),
         longitude = coalesce(longitude, deploy_long))

deploy_sf <- deploy_tbl %>% 
  sf::st_as_sf(coords = c('longitude','latitude'), crs = 4326) %>% 
  st_transform(3571) %>% 
  group_by(species,location, region) %>% 
  group_modify(~concaveman(., concavity = 4),.keep=TRUE) %>% 
  st_sf(crs = 3571) %>% 
  st_buffer(40*1000) 
  


world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  sf::st_transform(st_crs(deploy_sf))

ggplot() +
  layer_spatial(deploy_sf, aes(fill=species),
                linewidth=0) +
  annotation_spatial(world,
                     linewidth=0,
                     fill = "grey70") +
  facet_grid(cols = vars(species)) +
  scale_fill_manual(values = rev(met.brewer("Isfahan2")),
                       aesthetics = "fill",
                       name = "species") + 
  theme(legend.position = "bottom")

