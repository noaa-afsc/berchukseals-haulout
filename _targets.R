library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
files <- fs::dir_ls("R",recurse = TRUE, glob = "*.R")
sapply(files, source)


# Set target-specific options such as packages.
tar_option_set(packages = c(
  "tidyverse",
  "sf",
  "lubridate",
  "odbc",
  "RPostgres",
  "DBI",
  "dbplyr",
  "here",
  "keyringr",
  "pingr",
  "rnaturalearth",
  "rnaturalearthdata"
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

  tar_target(nsb_deployments, get_nsb_deployments()),
  tar_target(nsb_locations, get_nsb_locs(nsb_deployments)),
  tar_target(nsb_timelines, get_nsb_timelines(nsb_deployments)),

  tar_target(locs_sf, get_locs_sf(adfg_locations, nsb_locations)),
  tar_target(timeline_data, get_timeline_data(adfg_timelines, nsb_timelines)),

  tar_target(source_data, create_source_data(locs_sf, timeline_data)),

  tar_target(analysis_data, create_data_sf(locs_sf, source_data)),
  tar_target(grid, create_grid_sf(analysis_data)),
  tar_target(deploy_table, create_deploy_tbl(analysis_data))
)
