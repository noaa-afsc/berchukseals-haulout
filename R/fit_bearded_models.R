create_bearded_data <- function(dat.sf) {
  dat.sf %>%
    filter(species == "Bearded seal") %>%
    st_set_geometry(NULL) %>%
    as.data.frame() %>%
    droplevels()
}

fit_bearded <- function (HO_bearded) {
  glmmLDTS(fixed.formula = dry ~  
             sin1 + cos1 + sin2 + cos2 + sin3 + cos3 + 
             day + day_2 + day_3 + day_4 + 
             temp2m*wind + pressure + precip + northing +
             northing:day + northing:day_2 + northing:day_3 + northing_day_4,
           random.formula = dry ~ speno,
           data = HO_bearded,
           EstMeth = "REML",
           timecol = "time_vec",
           group.vec = "ar1_id")
}
