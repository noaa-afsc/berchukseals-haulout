View(dat.sf)

library(DBI)
library(odbc)
library(pingr)
stopifnot(
"PEP Postgres Database Not Available; did you start VPN? ;)" =
pingr::is_up("161.55.120.122", "5432")
)

con <- dbConnect(
  odbc::odbc(),
  dsn = "PostgreSQL pep",
  uid = keyringr::get_kc_account("pgpep_londonj"),
  pwd = keyringr::decrypt_kc_pw("pgpep_londonj")
)

qry <- "SELECT d.speno, d.serial_num, d.ptt, d.field_effort_lku, effort_lead, meta_project
FROM telem.tbl_tag_deployments d
LEFT JOIN capture.tbl_event USING (speno)
ORDER BY speno"

res <- con %>% tbl(dplyr::sql(qry)) %>%
  collect()

dat.sf %>% as_tibble() %>%
  group_by(speno,species,age,sex,deploy_dt) %>% count() %>%
  left_join(res, by="speno") %>%
  arrange(deploy_dt) %>%
  write_csv("haulout_deployments.csv")

