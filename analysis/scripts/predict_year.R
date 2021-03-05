library(tidyverse)
library(lubridate)
library(mgcv)
library(glmmLDTS)

# function to create a prediction data frame for a given age_sex group
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
    print(range_yday)
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



select_groups <- fit_ribbon_year$dataset %>%
  filter(age_sex != "YOUNG OF YEAR") %>%
  group_by(age_sex, year) %>%
  summarise(n = n_distinct(speno), group_id = paste0(age_sex,year)) %>%
  filter(n >1) %>% pull(group_id)

plot_df <- ribbon_newdata_year %>%
  mutate(species = "Ribbon seal",
         date = lubridate::as_date(yday, origin = "2015-01-01"),
         month = lubridate::month(date,label=TRUE),
         day = lubridate::day(date)) %>%
  filter(month != "Aug", paste0(age_sex,year) %in% select_groups)

peak_ho_df <- plot_df %>% filter(age_sex != "YOUNG OF YEAR",
                                 yday > 100) %>%
  group_by(species, age_sex, year) %>% slice_max(ho_prob)

select_groups <- fit_spotted_year$dataset %>%
  filter(age_sex != "YOUNG OF YEAR") %>%
  group_by(age_sex, year) %>%
  summarise(n = n_distinct(speno), group_id = paste0(age_sex,year)) %>%
  filter(n >1) %>% pull(group_id)

plot_df2 <- spotted_newdata_year %>%
  mutate(species = "Spotted seal",
         date = lubridate::as_date(yday, origin = "2015-01-01"),
         month = lubridate::month(date,label=TRUE),
         day = lubridate::day(date)) %>%
  filter(month != "Aug", paste0(age_sex,year) %in% select_groups)

peak_ho_df2 <- plot_df2 %>% filter(age_sex != "YOUNG OF YEAR",
                                 yday > 100) %>%
  group_by(species, age_sex, year) %>% slice_max(ho_prob)

plot_df <- plot_df %>% bind_rows(plot_df2)
peak_ho_df <- peak_ho_df %>% bind_rows(peak_ho_df2)


p <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(yday, ho_prob)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.25, stroke = 0, color = "grey50") +
  geom_pointrange(data = peak_ho_df,
                  mapping = aes(yday, ho_prob, ymin = 0, ymax = ho_prob, color = year),
                  size = 1, shape = 15) +
  scale_color_brewer(palette = "Spectral") +
  scale_x_continuous(breaks = c(74,105,135,166),
                     labels = c("15-Mar", "15-Apr", "15-May", "15-Jun")) +
  facet_grid(age_sex~species) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("haul-out probability") +
  ggtitle("Annual variability in the timing of peak haul-out probability")
