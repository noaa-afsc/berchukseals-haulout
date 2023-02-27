library(dbplyr)
nsb_captures <- tbl(con, in_schema("telem","nsb_capture")) %>% 
  select(speno, agency) %>% 
  group_by(speno, agency) %>% 
  summarise() %>% 
  collect() %>% 
  mutate(source = "database")
mml_deployments <- tbl(con, in_schema("telem","tbl_tag_deployments")) %>% 
  select(speno, agency = effort_lead) %>% 
  group_by(speno, agency) %>% 
  summarise() %>% 
  collect() %>% 
  mutate(source = "database")

targets::tar_load(adfg_deployments, store = here::here("_targets"))

adfg_deployments <- adfg_deployments %>% 
  group_by(speno) %>% 
  summarise() %>% 
  mutate(agency = "ADFG", source = "file")

deploy_details <- mml_deployments %>% 
  bind_rows(nsb_captures) %>% 
  bind_rows(adfg_deployments %>% select(speno,agency,source))

analysis_data %>% 
  sf::st_drop_geometry() %>% 
  select(speno, species, sex, age, deploy_dt, haulout_dt) %>%
  left_join(deploy_details, by = "speno") %>% 
  mutate(deploy_year = lubridate::year(deploy_dt)) %>% 
  group_by(agency, speno, species, deploy_year) %>% 
  summarise(n_seals = n_distinct(speno), across(sex:age, ~paste(unique(.), collapse = ", "))) %>% 
  select(-n_seals) %>% 
  arrange(agency, species, deploy_year, speno) %>% 
  sheet_write()