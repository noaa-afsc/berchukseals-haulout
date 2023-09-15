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
             day + day_2 + day_3 +  
             temp2m*wind + pressure + precip + 
             northing:day + northing:day_2 + northing:day_3 + 
             sin1:day + sin1:day_2 + sin1:day_3 +
             cos1:day + cos1:day_2 + cos1:day_3 +
             sin2:day + sin2:day_2 + sin2:day_3 +
             cos2:day + cos2:day_2 + cos2:day_3 +
             sin3:day + sin3:day_2 + sin3:day_3 +
             cos3:day + cos3:day_2 + cos3:day_3,
           random.formula = dry ~ speno,
           data = HO_bearded,
           EstMeth = "REML",
           timecol = "time_vec",
           group.vec = "ar1_id")
}
