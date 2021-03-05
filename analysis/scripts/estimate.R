library(glmmLDTS)

fit_ribbon <- readRDS(here::here("data/fit_ribbon.rds"))
#available here: https://drive.google.com/file/d/1rsC7PM11QcSFsxe-XKFiNtJq1ZkeG1QL/view?usp=sharing

# example from Jay's email
# for(i in 1:ndates) {
#   results[[i]] <- estimate.glmp.ts(glmp.ts.object = all.fit.1,
#                                    fixed.effect.list = list(
#                                      eff1 = list(effect = "MinFHi.Cat",
#                                                  level = as.character(survcov[i,'MinFHi.Cat']),
#                                                  weight = 1),
#                                      eff2 = list(effect = "MFAug15",
#                                                  weight = survcov[i,'MFAug15']),
#                                      eff3 = list(effect = "hour",
#                                                  level = as.character(survcov[i,'hour']),
#                                                  weight = 1),
#                                      eff4 = list(effect = "hour:MFAug15",
#                                                  level = paste(as.character(survcov[i,'hour']),",", sep = ""),
#                                                  weight = survcov[i,'MFAug15']),
#                                      eff5 = list(effect = "Year",
#                                                  level = c("102", "105", "106"),
#                                                  weight = c(0.333333, 0.333333, 0.333334))
#                                    )
#   )
# }

# let's remind ourselves of the model formula
fit_ribbon$fixed.formula

# estimate for solar hour 12 on day 106
#
# eventually, we want to estimate across all hours for a series of days
# to create our plots. So, this would become a nested for-loop

fix_solar_hour = 12
fix_sin1 <- sin(pi*fix_solar_hour/12)
fix_cos1 <- cos(pi*fix_solar_hour/12)
fix_sin2 <- sin(pi*fix_solar_hour/6)
fix_cos2 <- cos(pi*fix_solar_hour/6)
fix_sin3 <- sin(pi*fix_solar_hour/4)
fix_cos3 <- sin(pi*fix_solar_hour/4)

# yday 106 is April 15
fix_day <- (106-120)/10
fix_day2 <- fix_day^2
fix_day3 <- fix_day^3

# for now, we'll just use mean wx covariates
fix_temp2 <- fit_ribbon$dataset$temp2
fix_wind <- fit_ribbon$dataset$wind
fix_pressure <- fit_ribbon$dataset$pressure
fix_precip <- fit_ribbon$dataset$precip

# create our fixed effect list
fe_list <- list(
  eff_age_sex = list(effect = "age_sex",
                     level = c("ADULT.F","ADULT.M","SUBADULT","YOUNG OF YEAR"),
                     weight = c(0.25, 0.25, 0.25, 0.25)),
  eff_sin1 = list(effect = "sin1",
                  level = fix_sin1),
  eff_cos1 = list(effect = "cos1",
                  level = fix_cos1),
  eff_sin2 = list(effect = "sin2",
                  level = fix_sin2),
  eff_cos2 = list(effect = "cos2",
                  level = fix_cos2),
  eff_sin3 = list(effect = "sin3",
                  level = fix_sin3),
  eff_cos3 = list(effect = "cos3",
                  level = fix_cos3),
  eff_day = list(effect = "day",
                 level = fix_day),
  eff_day2 = list(effect = "day2",
                  level = fix_day2),
  eff_day3 = list(effect = "day3",
                  level = fix_day3),
  eff_temp2 = list(effect = "temp2",
                   level = fix_temp2),
  eff_wind = list(effect = "wind",
                  level = fix_wind),
  eff_pressure = list(effect = "pressure",
                      level = fix_pressure),
  eff_precip = list(effect = "precip",
                    level = fix_precip),
  eff_temp2_wind = list(effect = "temp2:wind",
                        level = c(fix_temp2,fix_wind)) # not sure I've specified level correct
)


res <- estimate.glmp.ts(glmp.ts.object = fit_ribbon,
                 fixed.effect.list = fe_list
                 )
