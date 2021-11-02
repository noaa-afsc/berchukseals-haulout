adfg_clean_deploy <- function(file1,file2) {
  file1 <- fs::path(file1)
  file2 <- fs::path(file2)

 cols <- readr::cols(
    DeployIDs = readr::col_character(),
    MapID = readr::col_character(),
    Species = readr::col_character(),
    Age = readr::col_character(),
    Sex = readr::col_character(),
    PrimaryTagType = readr::col_character(),
    FlipperTagType = readr::col_character()
  )

  adfg_file1 <- readr::read_csv(file1,col_types = cols) %>%
    dplyr::mutate(DateTagged = parse_datetime(DateTagged,format = "%m/%d/%Y %H:%M")) %>%
    dplyr::mutate(DateTagged = lubridate::force_tz(DateTagged,
                                                   'America/Anchorage')) %>%
    dplyr::mutate(DateTagged = lubridate::as_datetime(DateTagged,
                                                      tz = 'America/Anchorage')) %>%
    dplyr::filter( ! (DeployIDs %in% c("EB16SMK-158429")) )

  cols <- readr::cols(
    DeployIDs = readr::col_character(),
    Species = readr::col_character(),
    Age = readr::col_character(),
    Sex = readr::col_character(),
    FlipperTagType = readr::col_character()
  )

  adfg_file2 <- readr::read_csv(file2,col_types = cols) %>%
    dplyr::mutate(DateTagged = parse_datetime(DateTagged,format = "%m/%d/%y")) %>%
    dplyr::mutate(DateTagged = lubridate::force_tz(DateTagged,
                                                   'America/Anchorage')) %>%
    dplyr::mutate(DateTagged = lubridate::as_datetime(DateTagged,
                                                      tz = 'America/Anchorage'))

  adfg_deployments <- adfg_file1 %>%
    dplyr::bind_rows(adfg_file2) %>%
    dplyr::rename(speno = DeployIDs,
                  mapid = MapID,
                  deploy_dt = DateTagged,
                  primary_tag_type = PrimaryTagType,
                  flipper_tag_type = FlipperTagType,
                  flipper_last_signal = FlipperSatTagLastSignal
    ) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(species = tolower(species),
                  age = tolower(age),
                  age = case_when(
                    age %in% c("1","3") ~ "SUBADULT",
                    TRUE ~ age
                  ),
                  sex = tolower(sex)) %>%
    tidyr::gather(tag_position, tag_family, c(primary_tag_type,flipper_tag_type)) %>%
    dplyr::mutate(tag_position = ifelse(tag_position == "primary_tag_type",
                                        "primary","flipper")) %>%
    dplyr::filter(!is.na(tag_family)) %>%
    dplyr::arrange(deploy_dt)

  return(adfg_deployments)
}



adfg_clean_locs <- function(file1, file2, adfg_deployments) {
  file1 <- fs::path(file1)
  file2 <- fs::path(file2)

  cols <- readr::cols(
    DeployID = readr::col_character(),
    LocationQuality = readr::col_character(),
    Date = readr::col_double(),
    Latitude = readr::col_double(),
    Longitude = readr::col_double(),
    Ptt = readr::col_double(),
    TagType = readr::col_character(),
    MonthNumb = readr::col_character()
  )

  adfg_file1 <- readr::read_csv(file1,col_types = cols) %>%
    dplyr::mutate(Date = as.Date(Date, origin = "1899-12-30", tz ="UTC") %>%
                    lubridate::as_datetime(),
                  Ptt = as.integer(Ptt)) %>%
    dplyr::filter( ! (DeployID %in% c("EB16SMK-158429")) )

  cols <- readr::cols(
    DeployID = readr::col_character(),
    LocationQuality = readr::col_character(),
    Date = readr::col_double(),
    Latitude = readr::col_double(),
    Longitude = readr::col_double(),
    Ptt = readr::col_double(),
    TagType = readr::col_character(),
    SerialNum = readr::col_character()
  )

  adfg_file2 <- readr::read_csv(file2,col_types = cols) %>%
    dplyr::mutate(Date = as.Date(Date, origin = "1899-12-30", tz ="UTC") %>%
                    lubridate::as_datetime(),
                  Ptt = as.integer(Ptt))

  adfg_locations <- adfg_file1 %>%
    bind_rows(adfg_file2) %>%
    dplyr::rename(speno = DeployID,
                  quality = LocationQuality,
                  locs_dt = Date,
                  tag_family = TagType) %>%
    dplyr::filter(quality %in% c("3","2","1","0","A","B")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::left_join(adfg_deployments, by = c("speno","tag_family")) %>%
    mutate(species_code = case_when(
      species == "bearded" ~ "EB",
      species == "ribbon" ~ "HF",
      species == "spotted" ~ "PL"
    )) %>%
    mutate(deployid = glue::glue("{species_code}",
                                 "{lubridate::year(deploy_dt)}_{ptt}")) %>%
    mutate(unique_day =
             glue::glue("{lubridate::year(locs_dt)}",
                        "{lubridate::yday(locs_dt)}",
                        .sep = "_")) %>%
    dplyr::select(-species_code) %>%
    relocate(deployid,ptt) %>%
    filter(species %in% c('bearded', 'ribbon', 'spotted')) %>%
    filter(lubridate::month(locs_dt) %in% c(3,4,5,6,7)) %>%
    sf::st_as_sf(coords = c("longitude","latitude")) %>%
    sf::st_set_crs(value = "EPSG:4326")

  return(adfg_locations)
}


adfg_clean_tl <- function(file1,file2, adfg_deployments) {
  # DeployIDs "EB16SMK-158429" manually removed from file1 csv
  file1 <- fs::path(file1)
  file2 <- fs::path(file2)

  cols <- cols(
    .default = readr::col_double(),
    DeployIDs = readr::col_character(),
    GMTDate = readr::col_datetime(format = "%m/%d/%y"),
    TagType = readr::col_character(),
    HistType = readr::col_character(),
    LocationQuality = readr::col_character(),
    Sum = readr::col_character()
  )

  csv_files <- c(file1,file2)

  adfg_timelines <- csv_files %>%
    purrr::map_dfr(read_csv, col_types = cols) %>%
    dplyr::mutate(GMTDate = lubridate::as_datetime(GMTDate) %>% lubridate::force_tz("UTC"),
                  Ptt = as.integer(Ptt),
                  TagType = toupper(TagType),
                  TagType = case_when(
                    TagType == "UT" ~ "SPOT",
                    TRUE ~ TagType
                  )) %>%
    dplyr::rename(speno = DeployIDs,
                  tag_family = TagType,
                  hist_type = HistType,
                  timeline_start_dt = GMTDate) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::left_join(adfg_deployments, by = c("speno","tag_family")) %>%
    mutate(species_code = case_when(
      species == "bearded" ~ "EB",
      species == "ribbon" ~ "HF",
      species == "spotted" ~ "PL"
    )) %>%
    mutate(deployid = glue::glue("{species_code}",
                                 "{lubridate::year(deploy_dt)}_{ptt}")) %>%
    mutate(unique_day =
             glue::glue("{lubridate::year(timeline_start_dt)}",
                        "{lubridate::yday(timeline_start_dt)}",
                        .sep = "_")) %>%
    dplyr::relocate(deployid) %>%
    filter(species %in% c('bearded', 'ribbon', 'spotted')) %>%
    filter(lubridate::month(timeline_start_dt) %in% c(3,4,5,6,7)) %>%
    dplyr::select(-c(monthnumb,latitude,longitude,locationquality,sum,hist_type))

  bins <- list(bin = paste("bin", 1:24, sep=""), hour=0:23) %>% as_tibble()

  adfg_timelines <- adfg_timelines %>%
    tidyr::gather(bin, percent_dry, starts_with('bin')) %>%
    dplyr::left_join(bins, by = "bin") %>%
    dplyr::mutate(timeline_start_dt = timeline_start_dt + lubridate::hours(hour)) %>%
    dplyr::select(c(deployid,speno,species,age,sex,deploy_dt,ptt,tag_family,timeline_start_dt,unique_day,percent_dry)) %>%
    dplyr::arrange(deployid,timeline_start_dt)

  return(adfg_timelines)
}
