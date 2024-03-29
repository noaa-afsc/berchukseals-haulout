add_ssrd <- function(d) {
  years <- 2005:2021
  
  raster_merge <- vector(mode = "list", length = length(years))
  
  for (i in 1:length(years)) {
    left_file <- here::here('data_raw','era5_ssrd',paste0('era5_ssrd_',years[i],'_left.nc'))
    right_file <- here::here('data_raw','era5_ssrd',paste0('era5_ssrd_',years[i],'_right.nc'))
    r1 <- terra::rast(left_file)
    r2 <- terra::rast(right_file)
    raster_merge[[i]] <- terra::merge(r1,r2)
  }
  
  ssrd_dataset <- terra::rast(raster_merge)
  
  pts_vec <- project(terra::vect(d), 'EPSG:4326')
  
  # list to store for-loop results
  pts_ssrd_list <- vector(mode = "list", length = length(years))
  
  for (i in 1:length(years)) {
    r <- subset(ssrd_dataset, lubridate::year(time(ssrd_dataset)) == years[i])
    v <- subset(pts_vec, lubridate::year(pts_vec$haulout_dt) == years[i])
    tm <- match(v$haulout_dt,time(r))
    pts_ssrd_list[[i]] <- terra::extract(r,v,layer=tm, bind=TRUE)
  }
  
  ssrd_data <- do.call(rbind,pts_ssrd_list) %>%
    sf::st_as_sf() %>% 
    dplyr::rename(era5_ssrd = value) %>% 
    dplyr::mutate(era5_ssrd_watts = era5_ssrd/3600) %>%
    dplyr::select(-layer)
  
  return(ssrd_data)
}