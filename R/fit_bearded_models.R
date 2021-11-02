create_bearded_data <- function(dat.sf) {
  dat.sf %>%
    filter(species == "Bearded seal") %>%
    st_set_geometry(NULL) %>%
    as.data.frame() %>%
    droplevels()
}

fit_bearded <- function (HO_bearded) {
  glmmLDTS(fixed.formula = dry ~ sin1 + cos1 + sin2 + cos2 + sin3 + cos3 + day + day2+ day3 + northing + temp2 + wind + pressure + precip + wind*temp2 +
             sin1*day + cos1*day + sin2*day + cos2*day + sin3*day + cos3*day +
             sin1*day2 + cos1*day2 + sin2*day2 + cos2*day2 + sin3*day2 + cos3*day2 +
             northing*day + northing*day2,
           random.formula = dry ~ speno,
           data = HO_bearded,
           EstMeth = "REML",
           timecol = "time_vec",
           #ridge.reg = "global",
           #lambda = 0.5,
           group.vec = "ar1_id")
}
