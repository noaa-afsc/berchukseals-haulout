create_ribbon_data <- function(dat.sf) {
  dat.sf %>%
    filter(species == "Ribbon seal") %>%
    st_set_geometry(NULL) %>%
    as.data.frame() %>%
    droplevels()
}

fit_ribbon <- function(HO_ribbon) {
  glmmLDTS(
    fixed.formula = dry ~ age_sex + 
      sin1 + cos1 + sin2 + cos2 + sin3 + cos3 + 
      day + day_2 + day_3 +  
      temp2m*wind + pressure + precip + 
      age_sex:day + age_sex:day_2 + age_sex:day_3 + 
      sin1:day + sin1:day_2 + sin1:day_3 +
      cos1:day + cos1:day_2 + cos1:day_3 +
      sin2:day + sin2:day_2 + sin2:day_3 +
      cos2:day + cos2:day_2 + cos2:day_3 +
      sin3:day + sin3:day_2 + sin3:day_3 +
      cos3:day + cos3:day_2 + cos3:day_3,
    random.formula = dry ~ speno,
    data = HO_ribbon,
    EstMeth = "REML",
    timecol = "time_vec",
    group.vec = "ar1_id"
  )
}


# fit_ribbon_noagesex <- function(HO_ribbon) {
#   glmmLDTS(
#     fixed.formula = dry ~ sin1 + cos1 + sin2 + cos2 + sin3 + cos3 + day + day2 +
#       day3 + temp2 + wind + pressure + precip + wind * temp2 +
#       sin1 * day + cos1 * day + sin2 * day + cos2 *
#       day + sin3 * day + cos3 * day +
#       sin1 * day2 + cos1 * day2 + sin2 * day2 + cos2 *
#       day2 + sin3 * day2 + cos3 * day2,
#     random.formula = dry ~ speno,
#     data = HO_ribbon,
#     EstMeth = "REML",
#     timecol = "time_vec",
#     group.vec = "ar1_id"
#   )
# }

fit_ribbon_year <- function(HO_ribbon) {
  glmmLDTS(fixed.formula = dry ~ age_sex + 
             sin1 + cos1 + sin2 + cos2 + sin3 + cos3 + 
             day + day_2 + day_3 + year + 
             temp2m*wind + pressure + precip +
             sin1:day + sin1:day_2 + sin1:day_3 +
             cos1:day + cos1:day_2 + cos1:day_3 +
             sin2:day + sin2:day_2 + sin2:day_3 +
             cos2:day + cos2:day_2 + cos2:day_3 +
             sin3:day + sin3:day_2 + sin3:day_3 +
             cos3:day + cos3:day_2 + cos3:day_3,
           age_sex:day + age_sex:day_2 + age_sex:day_3 +  
             year:day + year:day_2 + year:day_3,
           random.formula = dry ~ speno,
           data = HO_ribbon,
           EstMeth = "REML",
           timecol = "time_vec",
           group.vec = "ar1_id")
}
