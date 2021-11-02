#library(icesealhaulout)
library(RPostgreSQL)
library(tidyverse)

con <- RPostgreSQL::dbConnect(PostgreSQL(),
                              dbname = Sys.getenv("pep_db"),
                              host = Sys.getenv("pep_ip"),
                              user = Sys.getenv("pep_admin"),
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

#data("percent_locs")
# load("//AKC0SS-N086/NMML_Users/Stacie.Hardy/Desktop/icesealhaulout/data/percent_locs.rda")
# percent_locs %>% dplyr::ungroup()
# percent_locs$fill_xy <- as.character(percent_locs$fill_xy)
# percent_locs$id <- 1:nrow(percent_locs)
# percent_locs <- percent_locs[, c(10, 1, 5, 6, 7, 3, 4, 8, 9)]
# colnames(percent_locs)[[6]] <- "haulout_dt"
# RPostgreSQL::dbSendQuery(con, "DROP TABLE IF EXISTS telem.res_iceseal_haulout")
# sf::st_write_db(con, percent_locs, c("telem", "res_iceseal_haulout"), row.names = FALSE)
# RPostgreSQL::dbSendQuery(con, "ALTER TABLE telem.res_iceseal_haulout RENAME COLUMN geometry TO geom")

# Create index on haulout data
dbSendQuery(con, "SELECT telem.fxn_iceseal_pred_idx();")
RPostgreSQL::dbClearResult(RPostgreSQL::dbListResults(con)[[1]])

# Create covariate tables
RPostgreSQL::dbSendQuery(con, "SELECT telem.fxn_iceseal_haulout_cov();")
RPostgreSQL::dbClearResult(RPostgreSQL::dbListResults(con)[[1]])

RPostgreSQL::dbDisconnect(con)
