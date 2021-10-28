# function to create a prediction data frame for a given age_sex group
create_newdata <- function(data, age_sex) {
  df_list <- vector(mode = "list", length = length({{age_sex}} ))

  for (a_s in {{age_sex}}) {
    range_yday <- data %>% filter(age_sex == a_s ) %>%
      summarize(start_day = min(yday),
                end_day = max(yday))
    start_day <- range_yday$start_day
    end_day <- range_yday$end_day
    n_days = (end_day - start_day) + 1

    # for wx covariates we'll use a gam to get values by day/hour since wx is likely
    # to vary w/in day over the season

    gam.baro <-
      gam(pressure ~ s(yday), data = data)
    gam.temp <-
      gam(temp2 ~ s(yday) + s(as.numeric(solar_hour)), data = data)
    gam.wind <-
      gam(wind ~ s(yday) + s(as.numeric(solar_hour)), data = data)
    gam.precip <-
      gam(precip ~ s(yday), data = data)

    wx_new_data <- data.frame(
      solar_hour = rep(0:23, each = n_days),
      yday = rep(start_day:end_day, times = 24)
    )

    temp_pred <- predict(gam.temp, newdata = wx_new_data)
    wind_pred <- predict(gam.wind, newdata = wx_new_data)
    baro_pred <- predict(gam.baro, newdata = wx_new_data)
    precip_pred <- predict(gam.precip, newdata = wx_new_data)

    df <- data.frame(
      age_sex = a_s,
      solar_hour = rep(0:23, each = n_days),
      yday = rep(start_day:end_day, times = 24),
      northing = mean(data$northing),
      temp2 = temp_pred,
      wind = wind_pred,
      pressure = baro_pred,
      precip = precip_pred
    ) %>%
      mutate(
        sin1 = sin(pi * solar_hour / 12),
        cos1 = cos(pi * solar_hour / 12),
        sin2 = sin(pi * solar_hour / 6),
        cos2 = cos(pi * solar_hour / 6),
        sin3 = sin(pi * solar_hour / 4),
        cos3 = sin(pi * solar_hour / 4),
      ) %>%
      mutate(day = (yday - 120) / 10,
             day2 = day ^ 2,
             day3 = day ^ 3)
    df_list[[a_s]] <- df
  }
  if(length({{age_sex}}) > 1) {
    df_out <- bind_rows(df_list) %>%
      mutate(age_sex = forcats::fct_relevel(
        age_sex,c("ADULT.F","ADULT.M","SUBADULT","YOUNG OF YEAR"))
      )
  } else {
    df_out <- bind_rows(df_list)
  }
}

create_ribbon_newdata <- function(fit_ribbon) {

  ribbon_newdata <- create_newdata(fit_ribbon$dataset,
                 age_sex = levels(fit_ribbon$dataset$age_sex))

  # create the model matrix
  ribbon_mm <- model.matrix(fit_ribbon$fixed.formula[-2],
                            data = ribbon_newdata)

  #clean up extra intercept and get coef
  fit_coef <- fit_ribbon$fixed.effects %>%
    filter(!is.na(std.err)) %>%
    pull(estimate)

  ribbon_newdata <- ribbon_newdata %>%
    mutate(logit_fits = as.vector(ribbon_mm %*% fit_coef),
           logit_fits_var = diag(ribbon_mm %*% fit_ribbon$covb %*% t(ribbon_mm)),
           logit_fits_lo95 = logit_fits - 1.96*sqrt(logit_fits_var),
           logit_fits_up95 = logit_fits + 1.96*sqrt(logit_fits_var),
           ho_prob = plogis(ribbon_mm %*% fit_coef),
           lower95 = plogis(logit_fits_lo95),
           upper95 = plogis(logit_fits_up95)
    )
}

create_spotted_newdata <- function(fit_spotted) {

  spotted_newdata <- create_newdata(fit_spotted$dataset,
                 age_sex = levels(fit_spotted$dataset$age_sex))

  # create the model matrix
  spotted_mm <- model.matrix(fit_spotted$fixed.formula[-2],
                            data = spotted_newdata)

  #clean up extra intercept and get coef
  fit_coef <- fit_spotted$fixed.effects %>%
    filter(!is.na(std.err)) %>%
    pull(estimate)

  spotted_newdata <- spotted_newdata %>%
    mutate(logit_fits = as.vector(spotted_mm %*% fit_coef),
           logit_fits_var = diag(spotted_mm %*% fit_spotted$covb %*% t(spotted_mm)),
           logit_fits_lo95 = logit_fits - 1.96*sqrt(logit_fits_var),
           logit_fits_up95 = logit_fits + 1.96*sqrt(logit_fits_var),
           ho_prob = plogis(spotted_mm %*% fit_coef),
           lower95 = plogis(logit_fits_lo95),
           upper95 = plogis(logit_fits_up95)
    )
}

create_bearded_newdata <- function(fit_bearded) {

  eb_data <- fit_bearded$dataset %>%
    mutate(age_sex = forcats::as_factor("ALL AGES"))

  bearded_newdata <- create_newdata(eb_data,
                                    age_sex = levels(eb_data$age_sex))

  # create the model matrix
  bearded_mm <- model.matrix(fit_bearded$fixed.formula[-2],
                             data = bearded_newdata)

  #clean up extra intercept and get coef
  fit_coef <- fit_bearded$fixed.effects %>%
    filter(!is.na(std.err)) %>%
    pull(estimate)

  bearded_newdata <- bearded_newdata %>%
    mutate(logit_fits = as.vector(bearded_mm %*% fit_coef),
           logit_fits_var = diag(bearded_mm %*% fit_bearded$covb %*% t(bearded_mm)),
           logit_fits_lo95 = logit_fits - 1.96*sqrt(logit_fits_var),
           logit_fits_up95 = logit_fits + 1.96*sqrt(logit_fits_var),
           ho_prob = plogis(bearded_mm %*% fit_coef),
           lower95 = plogis(logit_fits_lo95),
           upper95 = plogis(logit_fits_up95)
    )
}


create_newdata_year <- function(data, age_sex) {
  df_list <- vector(mode = "list", length = length({{age_sex}} ))
  df_list_years <- vector(mode = "list", length = length(levels(data$year)))

  for (a_s in {{age_sex}}) {
    for (y in levels(data$year)) {
      d <- data %>% filter(year == y, age_sex == a_s)
      if(nrow(d) == 0) { next }
      range_yday <- d %>%
        summarize(start_day = min(yday),
                  end_day = max(yday))
      if(range_yday$start_day > 152) {next}
      if(range_yday$end_day < 121) {next}
      #print(range_yday)
      start_day <- range_yday$start_day
      end_day <- range_yday$end_day
      n_days = (end_day - start_day) + 1

      # for wx covariates we'll use a gam to get values by day/hour since wx is likely
      # to vary w/in day over the season

      gam.baro <-
        gam(pressure ~ s(yday), data = data)
      gam.temp <-
        gam(temp2 ~ s(yday) + s(as.numeric(solar_hour)), data = data)
      gam.wind <-
        gam(wind ~ s(yday) + s(as.numeric(solar_hour)), data = data)
      gam.precip <-
        gam(precip ~ s(yday), data = data)

      wx_new_data <- data.frame(
        solar_hour = 12,
        yday = start_day:end_day
      )

      temp_pred <- predict(gam.temp, newdata = wx_new_data)
      wind_pred <- predict(gam.wind, newdata = wx_new_data)
      baro_pred <- predict(gam.baro, newdata = wx_new_data)
      precip_pred <- predict(gam.precip, newdata = wx_new_data)

      df <- data.frame(
        age_sex = a_s,
        year = y,
        solar_hour = 12,
        yday = start_day:end_day,
        northing = mean(data$northing),
        temp2 = temp_pred,
        wind = wind_pred,
        pressure = baro_pred,
        precip = precip_pred
      ) %>%
        mutate(
          sin1 = sin(pi * solar_hour / 12),
          cos1 = cos(pi * solar_hour / 12),
          sin2 = sin(pi * solar_hour / 6),
          cos2 = cos(pi * solar_hour / 6),
          sin3 = sin(pi * solar_hour / 4),
          cos3 = sin(pi * solar_hour / 4),
        ) %>%
        mutate(day = (yday - 120) / 10,
               day2 = day ^ 2,
               day3 = day ^ 3)
      df_list_years[[y]] <- df
    }
    df_list[[a_s]] <- bind_rows(df_list_years)
  }
  df_out <- bind_rows(df_list) %>%
    mutate(age_sex = forcats::fct_relevel(
      age_sex,c("ADULT.F","ADULT.M","SUBADULT","YOUNG OF YEAR")),
      year = forcats::as_factor(as.numeric(year))
    )

  return(df_out)
}

ribbon_newdata_yr <- function(fit_ribbon_year) {
  ribbon_newdata_year <- create_newdata_year(fit_ribbon_year$dataset,
                                           age_sex = levels(fit_ribbon_year$dataset$age_sex))

# create the model matrix
ribbon_mm <- model.matrix(fit_ribbon_year$fixed.formula[-2],
                          data = ribbon_newdata_year)

#clean up extra intercept and get coef
fit_coef <- fit_ribbon_year$fixed.effects %>%
  filter(!is.na(std.err)) %>%
  pull(estimate)

# multiply by the model coefficients and convert to probabilities with plogis
# (this came from J Laake code for Hood Canal estimates)
ribbon_newdata_year <- ribbon_newdata_year %>%
  mutate(logit_fits = as.vector(ribbon_mm %*% fit_coef),
         logit_fits_var = diag(ribbon_mm %*% fit_ribbon_year$covb %*% t(ribbon_mm)),
         logit_fits_lo95 = logit_fits - 1.96*sqrt(logit_fits_var),
         logit_fits_up95 = logit_fits + 1.96*sqrt(logit_fits_var),
         ho_prob = plogis(ribbon_mm %*% fit_coef),
         lower95 = plogis(logit_fits_lo95),
         upper95 = plogis(logit_fits_up95)
  )
}


spotted_newdata_yr <- function(fit_spotted_year) {
  spotted_newdata_year <- create_newdata_year(fit_spotted_year$dataset,
                                            age_sex = levels(fit_spotted_year$dataset$age_sex))

# create the model matrix
spotted_mm <- model.matrix(fit_spotted_year$fixed.formula[-2],
                           data = spotted_newdata_year)

#clean up extra intercept and get coef
fit_coef <- fit_spotted_year$fixed.effects %>%
  filter(!is.na(std.err)) %>%
  pull(estimate)

# multiply by the model coefficients and convert to probabilities with plogis
# (this came from J Laake code for Hood Canal estimates)
spotted_newdata_year <- spotted_newdata_year %>%
  mutate(logit_fits = as.vector(spotted_mm %*% fit_coef),
         logit_fits_var = diag(spotted_mm %*% fit_spotted_year$covb %*% t(spotted_mm)),
         logit_fits_lo95 = logit_fits - 1.96*sqrt(logit_fits_var),
         logit_fits_up95 = logit_fits + 1.96*sqrt(logit_fits_var),
         ho_prob = plogis(spotted_mm %*% fit_coef),
         lower95 = plogis(logit_fits_lo95),
         upper95 = plogis(logit_fits_up95)
  )
}
