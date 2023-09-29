qry_sea_ice_extent <- function(con,
                               xmin,ymin,xmax,ymax,
                               epsg,
                               start_date,
                               end_date) {
  qry <- glue::glue(
    "SELECT * FROM environ.fxn_sea_ice_extent_by_date(0.15, --concentration
                                              {round(xmin)}, --xmin
                                              {round(ymin)}, --ymin
                                              {round(xmax)}, --xmax
                                              {round(ymax)}, --ymax
                                              {epsg}, --epsg of the bounding box
                                              '{start_date}', --start_date
                                              '{end_date}' -- end_date
      )"
  )
  RPostgres::dbGetQuery(conn = con, 
                        statement = qry)
}

get_sea_ice_extent <- function(min_year, max_year, bbox, epsg) {
  tryCatch({
    con <- dbConnect(RPostgres::Postgres(),
                      dbname = 'pep', 
                      host = Sys.getenv('PEP_PG_IP'),
                      user = keyringr::get_kc_account("pgpep_londonj"),
                      password = keyringr::decrypt_kc_pw("pgpep_londonj"))
  },
  error = function(cond) {
    print("Unable to connect to Database.")
  })
  on.exit(dbDisconnect(con))
  
  res <- vector("list", length(min_year:max_year))
  names(res) <- paste0('y',min_year:max_year)
  
  epsg <- strsplit(epsg,":")[[1]][2]
  xmin <- bbox[1]
  ymin <- bbox[2]
  xmax <- bbox[3]
  ymax <- bbox[4]
  
  for(y in names(res)) {
    start_date <- paste0(substr(y,2,5),'-','02-15')
    end_date <- paste0(substr(y,2,5),'-','07-15')
    res[[y]] <- qry_sea_ice_extent(con,
                                 xmin,ymin,xmax,ymax,
                                 epsg,
                                 start_date,end_date)
  }
  
  return(res)
}