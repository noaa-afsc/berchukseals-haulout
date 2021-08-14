get_data_wxcov <- function() {
  con <- dbConnect(RPostgres::Postgres(),dbname = 'pep',
                   host = '161.55.120.122',
                   port = 5432,
                   user = keyringr::get_kc_account("pgpep_sa"),
                   password = keyringr::decrypt_kc_pw("pgpep_sa"))
  on.exit(dbDisconnect(con))

  dbExecute(con, "SELECT telem.fxn_iceseal_pred_idx();")
  dbExecute(con, "SELECT telem.fxn_iceseal_haulout_cov();")

  qry <- {
    "SELECT *
  FROM telem.res_iceseal_haulout_cov
  WHERE
  EXTRACT(MONTH FROM haulout_dt) IN (3,4,5,6,7) AND
  rast_vwnd IS NOT NULL AND
  species != 'Ph' AND
  speno != 'EB2009_7010' AND
  speno != 'EB2009_3002' "
  }

  sf::st_read(con, query = qry)
}
