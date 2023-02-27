library(targets)
library(future)
library(future.callr)
plan(callr)

files <- fs::dir_ls("R",recurse = TRUE, glob = "*.R")
sapply(files, source)

# Set target-specific options such as packages.
tar_option_set(packages = c(
  "tidyverse",
  "ggplot2",
  "sf",
  "ggspatial",
  "lubridate",
  "odbc",
  "RPostgres",
  "DBI",
  "dbplyr",
  "here",
  "keyringr",
  "pingr",
  "rnaturalearth",
  "rnaturalearthdata",
  "glmmLDTS",
  "mgcv",
  "solaR",
  "splines",
  "purler",
  "wcUtils",
  "MetBrewer",
  "patchwork",
  "concaveman"
  )
)

# End this file with a list of target objects.
list(
  tar_target(adfg_deploy_file1, "data_raw/adfg/ADFG_SealDeploymentTable3.csv",format = "file"),
  tar_target(adfg_deploy_file2, "data_raw/adfg/ADFG_SealDeploymentTable_NewRecords.csv", format = "file"),
  tar_target(adfg_deployments, adfg_clean_deploy(adfg_deploy_file1, adfg_deploy_file2)),
  tar_target(adfg_locs_file1, "data_raw/adfg/ADFG_SealLocationData3.csv",format = "file"),
  tar_target(adfg_locs_file2, "data_raw/adfg/ADFG_SealLocationData_NewRecords.csv", format = "file"),
  tar_target(adfg_locations, adfg_clean_locs(adfg_locs_file1, adfg_locs_file2, adfg_deployments)),
  tar_target(adfg_timelines_file1, "data_raw/adfg/ADFG_SealTimelineData3.csv",format = "file"),
  tar_target(adfg_timelines_file2, "data_raw/adfg/ADFG_SealTimelineData_NewRecords.csv", format = "file"),
  tar_target(adfg_timelines, adfg_clean_tl(adfg_timelines_file1, adfg_timelines_file2, adfg_deployments)),
  tar_target(deploy_details_file, "data_raw/deploy_details.csv",format = "file"),
  tar_target(deploy_details, create_deploy_details(deploy_details_file)),

  tar_target(nsb_deployments, get_nsb_deployments()),
  tar_target(nsb_locations, get_nsb_locs(nsb_deployments)),
  tar_target(nsb_timelines, get_nsb_timelines(nsb_deployments)),

  tar_target(locs_sf, get_locs_sf(adfg_locations, nsb_locations)),
  tar_target(timeline_data, get_timeline_data(adfg_timelines, nsb_timelines)),

  tar_target(source_data, create_source_data(locs_sf, timeline_data)),

  tar_target(analysis_data, create_data_sf(locs_sf, source_data)),
  tar_target(grid, create_grid_sf(analysis_data)),
  tar_target(deploy_table, create_deploy_tbl(analysis_data)),

  tar_target(model_data, create_model_input(analysis_data)),

  tar_target(ribbon_model_data, create_ribbon_data(model_data)),
  tar_target(ribbon_fit, fit_ribbon(ribbon_model_data)),
  tar_target(ribbon_noagesex_fit, fit_ribbon_noagesex(ribbon_model_data)),
  tar_target(ribbon_year_fit, fit_ribbon_year(ribbon_model_data)),
  tar_target(spotted_model_data, create_spotted_data(model_data)),
  tar_target(spotted_fit, fit_spotted(spotted_model_data)),
  tar_target(spotted_noagesex_fit, fit_spotted_noagesex(spotted_model_data)),
  tar_target(spotted_year_fit, fit_spotted_year(spotted_model_data)),
  tar_target(bearded_model_data, create_bearded_data(model_data)),
  tar_target(bearded_fit, fit_bearded(bearded_model_data)),

  tar_target(ribbon_newdata, create_ribbon_newdata(ribbon_fit)),
  tar_target(spotted_newdata, create_spotted_newdata(spotted_fit)),
  tar_target(bearded_newdata, create_bearded_newdata(bearded_fit)),
  
  tar_target(ribbon_wx_plot, plot_ribbon_wx(ribbon_fit)),
  tar_target(spotted_wx_plot, plot_spotted_wx(spotted_fit)),
  tar_target(bearded_wx_plot, plot_bearded_wx(bearded_fit)),

  tar_target(ribbon_newdata_year, ribbon_newdata_yr(ribbon_year_fit)),
  tar_target(spotted_newdata_year, spotted_newdata_yr(spotted_year_fit))
)
