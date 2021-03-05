library(glmmLDTS)
library(mgcv)
library(dplyr)
library(forcats)
library(ggplot2)

# get the ribbon seal fit object
fit_ribbon <- readRDS(here::here("data/fit_ribbon.Rdata"))

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
  df_out <- bind_rows(df_list) %>%
    mutate(age_sex = forcats::fct_relevel(
      age_sex,c("ADULT.F","ADULT.M","SUBADULT","YOUNG OF YEAR"))
    )
}

ribbon_newdata <- create_newdata(fit_ribbon$dataset,
                                 age_sex = levels(fit_ribbon$dataset$age_sex))

# create the model matrix
ribbon_mm <- model.matrix(fit_ribbon$fixed.formula[-2],
                data = ribbon_newdata)

#clean up extra intercept and get coef
fit_coef <- fit_ribbon$fixed.effects %>%
  filter(!is.na(std.err)) %>%
  pull(estimate)

# multiply by the model coefficients and convert to probabilities with plogis
# (this came from J Laake code for Hood Canal estimates)
ribbon_newdata <- ribbon_newdata %>%
  mutate(logit_fits = as.vector(ribbon_mm %*% fit_coef),
         logit_fits_var = diag(ribbon_mm %*% fit_ribbon$covb %*% t(ribbon_mm)),
         logit_fits_lo95 = logit_fits - 1.96*sqrt(logit_fits_var),
         logit_fits_up95 = logit_fits + 1.96*sqrt(logit_fits_var),
         ho_prob = plogis(ribbon_mm %*% fit_coef),
         lower95 = plogis(logit_fits_lo95),
         upper95 = plogis(logit_fits_up95)
         )

####################################################
####################################################
# begin Jay's code here
####################################################
####################################################

# #check to make sure Josh's model matrix matches exactly that produced by glmmLDTS
# colnames(fit_ribbon$covb) == colnames(ribbon_mm)
# #everything looks fine as far as ordering of labels.  Can't vouch for what is in them.
# # get the fits on the logit scale
# ribbon_newfits =ribbon_mm%*%fit_coef
# # get variances on logit scale.  This may take a while if ribbon_mm has lots of rows
# # break up the matrix if it is too large (or use a loop, row by row)
# ribbon_newfits_var = diag(ribbon_mm%*%fit_ribbon$covb %*% t(ribbon_mm))
# #These variances are on the logit scale.  If you want intervals, I would suggest
# # creating them on the logit scale, and then backtransform
# #quantiles are preserved when back-transforming
# ribbon_newfits_lo95 = ribbon_newfits - 1.96*sqrt(ribbon_newfits_var)
# ribbon_newfits_up95 = ribbon_newfits+ 1.96*sqrt(ribbon_newfits_var)
#
# ribbon_newfitsDF = data.frame(fits = plogis(ribbon_newfits),
#                               lower95 = plogis(ribbon_newfits_lo95),
#                               upper95 = plogis(ribbon_newfits_up95))
# head(ribbon_newfitsDF)
#
# max(ribbon_newfitsDF$fits)
# #there seems to be a lot of values near 1 in the prediction data set
# sum(ribbon_newfitsDF$fits > .9)/dim(ribbon_newfitsDF)[1]
# # greater than 12 %
# # check the fitted data
# sum(fit_ribbon$fit.table[,'mu'] > .9)/length(fit_ribbon$fit.table[,'mu'])
# # less than 0.1 %

####################################################
####################################################
# end Jay's code here
####################################################
####################################################



# make a pretty plot

plot_df <- ribbon_newdata %>%
  mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
         month = lubridate::month(date,label=TRUE),
         day = lubridate::day(date)) %>%
  filter(month != "Aug")

p <- ggplot(plot_df,aes(day,solar_hour,fill=ho_prob))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_viridis_c(name = "haulout probability", aesthetics = "fill",
                       guide = guide_colorbar(title.position = "bottom", barwidth = 15,
                                              barheight = 0.5, title.hjust = 0.5)
  )
p <- p + facet_grid(age_sex~month)
p <- p + scale_x_continuous(breaks = c(1,10,20,30))
p <- p + theme_minimal()
p <- p + theme(legend.position = "bottom") +
  theme(strip.background = element_rect(colour="white")) +
  theme(axis.ticks=element_blank()) +
  xlab("day of month") + ylab("solar hour") +
  ggtitle("Predicted haul-out probability of ribbon seals")
p

p_temp_wind <- ggplot(data = fit_ribbon$dataset %>%
                        filter(age_sex != "YOUNG OF YEAR"),
                      aes(temp2, wind, color = percent_dry, size = percent_dry)) +
  geom_point(alpha = 0.15,stroke = 0) +
  scale_color_viridis_c(option = "D") +
  facet_grid(age_sex ~ .) +
  theme_minimal()

library(ggsci)

p_temp <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(temp2*27, ho_prob, color = age_sex)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.1, stroke = 0) +
  scale_color_d3() +
  theme_minimal() +
  xlab("temperature (C)") + ylab("haul out probability") +
  ggtitle("... when temperatures are warmer")

p_wind <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(wind, ho_prob, color = age_sex)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.1, stroke = 0) +
  scale_color_d3() +
  theme_minimal() +
  xlab("wind") + ylab("haul out probability") +
  ggtitle("... when winds are lower")

p_pressure <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(pressure, ho_prob, color = age_sex)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.1, stroke = 0) +
  scale_color_d3() +
  theme_minimal() +
  xlab("barometric pressure") + ylab("haul out probability") +
  ggtitle("... during periods of high pressure")

p_precip <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(precip, ho_prob, color = age_sex)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.1, stroke = 0) +
  scale_color_d3() +
  theme_minimal() +
  xlab("precipitation") + ylab("haul out probability") +
  ggtitle("... during times of low precipitation")

library(patchwork)

p_temp + p_wind + p_pressure + p_precip +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = 'Ribbon seal haul-out behavior increases ...')


# get the spotted seal fit object
fit_spotted <- readRDS(here::here("data/fit_spotted.rds"))

spotted_newdata <- create_newdata(fit_spotted$dataset,
                                 age_sex = levels(fit_spotted$dataset$age_sex))

# create the model matrix
spotted_mm <- model.matrix(fit_spotted$fixed.formula[-2],
                          data = spotted_newdata)

#clean up extra intercept and get coef
fit_coef <- fit_spotted$fixed.effects %>%
  filter(!is.na(std.err)) %>%
  pull(estimate)

# multiply by the model coefficients and convert to probabilities with plogis
# (this came from J Laake code for Hood Canal estimates)
spotted_newdata <- spotted_newdata %>%
  mutate(logit_fits = as.vector(spotted_mm %*% fit_coef),
         logit_fits_var = diag(spotted_mm %*% fit_spotted$covb %*% t(spotted_mm)),
         logit_fits_lo95 = logit_fits - 1.96*sqrt(logit_fits_var),
         logit_fits_up95 = logit_fits + 1.96*sqrt(logit_fits_var),
         ho_prob = plogis(spotted_mm %*% fit_coef),
         lower95 = plogis(logit_fits_lo95),
         upper95 = plogis(logit_fits_up95)
  )

# make a pretty plot

plot_df <- spotted_newdata %>%
  mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
         month = lubridate::month(date,label=TRUE),
         day = lubridate::day(date)) %>%
  filter(month != "Aug")

p <- ggplot(plot_df,aes(day,solar_hour,fill=ho_prob))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_viridis_c(name = "haulout probability", aesthetics = "fill",
                       guide = guide_colorbar(title.position = "bottom", barwidth = 15,
                                              barheight = 0.5, title.hjust = 0.5)
  )
p <- p + facet_grid(age_sex~month)
p <- p + scale_x_continuous(breaks = c(1,10,20,30))
p <- p + theme_minimal()
p <- p + theme(legend.position = "bottom") +
  theme(strip.background = element_rect(colour="white")) +
  theme(axis.ticks=element_blank()) +
  xlab("day of month") + ylab("solar hour") +
  ggtitle("Predicted haul-out probability of spotted seals")
p

library(ggsci)

p_temp <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(temp2*27, ho_prob, color = age_sex)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.1, stroke = 0) +
  scale_color_d3() +
  theme_minimal() +
  xlab("temperature (C)") + ylab("haul out probability") +
  ggtitle("... increases when temperatures are warmer")

p_wind <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(wind, ho_prob, color = age_sex)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.1, stroke = 0) +
  scale_color_d3() +
  theme_minimal() +
  xlab("wind") + ylab("haul out probability") +
  ggtitle("... is not influenced by wind")

p_pressure <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(pressure, ho_prob, color = age_sex)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.1, stroke = 0) +
  scale_color_d3() +
  theme_minimal() +
  xlab("barometric pressure") + ylab("haul out probability") +
  ggtitle("... increases slightly during high pressure")

p_precip <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(precip, ho_prob, color = age_sex)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.1, stroke = 0) +
  scale_color_d3() +
  theme_minimal() +
  xlab("precipitation") + ylab("haul out probability") +
  ggtitle("... is minimally influenced by precipitation")

library(patchwork)

p_temp + p_wind + p_pressure + p_precip +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = 'Spotted seal haul-out behavior ...')


# get the bearded seal fit object
fit_bearded <- readRDS(here::here("data/fit_bearded.rds"))

bearded_newdata <- create_newdata(fit_bearded$dataset,
                                  age_sex = levels(fit_bearded$dataset$age_sex))

# create the model matrix
bearded_mm <- model.matrix(fit_bearded$fixed.formula[-2],
                           data = bearded_newdata)

#clean up extra intercept and get coef
fit_coef <- fit_bearded$fixed.effects %>%
  filter(!is.na(std.err)) %>%
  pull(estimate)

# multiply by the model coefficients and convert to probabilities with plogis
# (this came from J Laake code for Hood Canal estimates)
bearded_newdata <- bearded_newdata %>%
  mutate(logit_fits = as.vector(bearded_mm %*% fit_coef),
         logit_fits_var = diag(bearded_mm %*% fit_bearded$covb %*% t(bearded_mm)),
         logit_fits_lo95 = logit_fits - 1.96*sqrt(logit_fits_var),
         logit_fits_up95 = logit_fits + 1.96*sqrt(logit_fits_var),
         ho_prob = plogis(bearded_mm %*% fit_coef),
         lower95 = plogis(logit_fits_lo95),
         upper95 = plogis(logit_fits_up95)
  )

# make a pretty plot

plot_df <- bearded_newdata %>%
  mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
         month = lubridate::month(date,label=TRUE),
         day = lubridate::day(date)) %>%
  filter(month != "Aug",
         age_sex != "YOUNG OF YEAR")

p <- ggplot(plot_df,aes(day,solar_hour,fill=ho_prob))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_viridis_c(name = "haulout probability", aesthetics = "fill",
                       guide = guide_colorbar(title.position = "bottom", barwidth = 15,
                                              barheight = 0.5, title.hjust = 0.5)
  )
p <- p + facet_grid(age_sex~month)
p <- p + scale_x_continuous(breaks = c(1,10,20,30))
p <- p + theme_minimal()
p <- p + theme(legend.position = "bottom") +
  theme(strip.background = element_rect(colour="white")) +
  theme(axis.ticks=element_blank()) +
  xlab("day of month") + ylab("solar hour") +
  ggtitle("Predicted haul-out probability of bearded seals")
p

library(ggsci)

p_temp <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(temp2*27, ho_prob, color = age_sex)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.1, stroke = 0) +
  scale_color_d3() +
  theme_minimal() +
  xlab("temperature (C)") + ylab("haul out probability") +
  ggtitle("... increases when temperatures are warmer")

p_wind <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(wind, ho_prob, color = age_sex)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.1, stroke = 0) +
  scale_color_d3() +
  theme_minimal() +
  xlab("wind") + ylab("haul out probability") +
  ggtitle("... is not influenced by wind")

p_pressure <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(pressure, ho_prob, color = age_sex)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.1, stroke = 0) +
  scale_color_d3() +
  theme_minimal() +
  xlab("barometric pressure") + ylab("haul out probability") +
  ggtitle("... increases slightly during high pressure")

p_precip <- ggplot(plot_df %>% filter(age_sex != "YOUNG OF YEAR"), aes(precip, ho_prob, color = age_sex)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95), alpha = 0.1, stroke = 0) +
  scale_color_d3() +
  theme_minimal() +
  xlab("precipitation") + ylab("haul out probability") +
  ggtitle("... is minimally influenced by precipitation")

library(patchwork)

p_temp + p_wind + p_pressure + p_precip +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = 'Bearded seal haul-out behavior ...')

