create_spotted_data <- function(dat.sf) {
  dat.sf %>%
    filter(species == "Spotted seal") %>%
    st_set_geometry(NULL) %>%
    as.data.frame() %>%
    droplevels()
}

fit_spotted <- function(HO_spotted) {
  glmmLDTS(fixed.formula = dry ~ age_sex + sin1 + cos1 + sin2 + cos2 + sin3 + cos3 + day + day2+ day3 + temp2 + wind + pressure + precip + wind*temp2 +
             sin1*day + cos1*day + sin2*day + cos2*day + sin3*day + cos3*day +
             sin1*day2 + cos1*day2 + sin2*day2 + cos2*day2 + sin3*day2 + cos3*day2 +
             age_sex:day + age_sex:day2 + age_sex:day3,
           random.formula = dry ~ speno,
           data = HO_spotted,
           EstMeth = "REML",
           timecol = "time_vec",
           #ridge.reg = "global",
           #lambda = 0.5,
           group.vec = "ar1_id")
}

fit_spotted_bam <- function(HO_spotted) {
  mgcv::bam(
    dry ~ age_sex + s(ar1_id, bs = "re") + sin1 + cos1 + sin2 + cos2 + sin3 + cos3 + day + day2+ day3 + temp2 + wind + pressure + precip + wind*temp2 +
      sin1*day + cos1*day + sin2*day + cos2*day + sin3*day + cos3*day +
      sin1*day2 + cos1*day2 + sin2*day2 + cos2*day2 + sin3*day2 + cos3*day2 +
      age_sex:day + age_sex:day2 + age_sex:day3,
    data = HO_spotted,
    discrete = TRUE
  )
}

fit_spotted_noagesex <- function(HO_spotted) {
  glmmLDTS(fixed.formula = dry ~ sin1 + cos1 + sin2 + cos2 + sin3 + cos3 + day + day2+ day3 + temp2 + wind + pressure + precip + wind*temp2 +
             sin1*day + cos1*day + sin2*day + cos2*day + sin3*day + cos3*day +
             sin1*day2 + cos1*day2 + sin2*day2 + cos2*day2 + sin3*day2 + cos3*day2,
           random.formula = dry ~ speno,
           data = HO_spotted,
           EstMeth = "REML",
           timecol = "time_vec",
           #ridge.reg = "global",
           #lambda = 0.5,
           group.vec = "ar1_id")
}

fit_spotted_year <- function(HO_spotted) {
  glmmLDTS(fixed.formula = dry ~ age_sex + sin1 + cos1 + sin2 + cos2 + sin3 + cos3 + day + day2+ day3 + temp2 + wind + pressure + precip + wind*temp2 +
             sin1*day + cos1*day + sin2*day + cos2*day + sin3*day + cos3*day +
             sin1*day2 + cos1*day2 + sin2*day2 + cos2*day2 + sin3*day2 + cos3*day2 +
             age_sex:day + age_sex:day2 + age_sex:day3 + year*day + year*day2,
           random.formula = dry ~ speno,
           data = HO_spotted,
           EstMeth = "REML",
           timecol = "time_vec",
           #ridge.reg = "global",
           #lambda = 0.5,
           group.vec = "ar1_id")
}
