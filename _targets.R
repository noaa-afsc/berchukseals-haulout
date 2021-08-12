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
  "here",
  "keyringr"
  )
)

# End this file with a list of target objects.
list(
  tar_target(adfg_deploy_file1, "data_raw/adfg/ADFG_SealDeploymentTable3.csv",format = "file"),
  tar_target(adfg_deploy_file2, "data_raw/adfg/ADFG_SealDeploymentTable_NewRecords.csv", format = "file"),
  tar_target(adfg_clean_deployments, adfg_clean_deploy(adfg_deploy_file1, adfg_deploy_file2))
)
