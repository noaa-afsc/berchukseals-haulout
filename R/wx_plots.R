plot_ribbon_wx <- function(ribbon_fit_obj,age_sex_colors,age_sex_labels) {
  
  theme_set(theme_minimal(base_family = 'Spline Sans',
                          base_size = 9))
  theme_update(
    plot.title.position = "plot",
    # left-align title
    plot.caption.position = "plot",
    # right-align caption
    plot.title = element_text(face = "bold"),
    # larger, bold title
    plot.subtitle = element_textbox_simple(
      color = "grey30",
      margin = ggplot2::margin(t = 0, b = 12, r = 60)
    ),
    plot.caption = element_text(color = "grey30"),
    # change color of caption
    panel.grid.minor = element_blank() # no minor grid lines
  )
  
  p_temp <- create_ribbon_newdata(ribbon_fit_obj, 
                                  margins=TRUE, 
                                  solar_hour = 12, yday = 135, 
                                  term = "temp2m") %>% 
    mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
           month = lubridate::month(date,label = TRUE),
           day = lubridate::day(date)) %>% 
    filter(age_sex != "YOUNG OF YEAR") %>% 
    group_by(age_sex) %>% 
    
    ggplot(aes(temp2m*27, ho_prob, color = age_sex)) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, 
                    fill = age_sex), linetype = 0, alpha = 0.2,
                show.legend = FALSE) +
    geom_pointrange(aes(ymin = lower95, ymax = upper95), 
                    size = 0, alpha = 0.25, stroke = 0,
                    show.legend = FALSE) +
    geom_line() +
    scale_color_manual(values= age_sex_colors,
                       labels = age_sex_labels,
                       name = "Ribbon seal",
                       guide = guide_legend(override.aes = list(alpha = 1),
                                            title.position = "top",
                                            title.hjust = 0,
                                            direction = "horizontal")) +
    scale_fill_manual(values = age_sex_colors,
                      labels = age_sex_labels) +
    theme(legend.position = "top",
          legend.justification = "left") +
    xlab("temperature (C)") + ylab("haul-out probability")
  
  p_wind <- create_ribbon_newdata(ribbon_fit_obj, 
                                  margins=TRUE, 
                                  solar_hour = 12, yday = 135, 
                                  term = "wind") %>%
    mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
           month = lubridate::month(date,label = TRUE),
           day = lubridate::day(date)) %>% 
    filter(age_sex != "YOUNG OF YEAR") %>% 
    group_by(age_sex) %>% 
    
    ggplot(aes(wind*10, ho_prob, color = age_sex)) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, 
                    fill = age_sex), linetype = 0, alpha = 0.2,
                show.legend = FALSE) +
    geom_pointrange(aes(ymin = lower95, ymax = upper95), 
                    size = 0, alpha = 0.25, stroke = 0) +
    geom_line() +
    scale_color_manual(values= age_sex_colors,
                       labels = age_sex_labels,
                       guide = guide_legend(override.aes = list(alpha = 1),
                                            direction = "horizontal")) +
    scale_fill_manual(values = age_sex_colors,
                      labels = age_sex_labels) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    xlab("wind (m/s)") + ylab("haul-out probability")
  
  p_pressure <- create_ribbon_newdata(ribbon_fit_obj, margins=TRUE, 
                                      solar_hour = 12, yday = 135, 
                                      term = "pressure") %>%
    mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
           month = lubridate::month(date,label = TRUE),
           day = lubridate::day(date)) %>% 
    filter(age_sex != "YOUNG OF YEAR") %>% 
    group_by(age_sex) %>% 
    
    ggplot(aes(((pressure * 10000) + 100000)/1000, ho_prob, color = age_sex)) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, 
                    fill = age_sex), linetype = 0, alpha = 0.2,
                show.legend = FALSE) +
    geom_pointrange(aes(ymin = lower95, ymax = upper95), 
                    size = 0, alpha = 0.25, stroke = 0) +
    geom_line() +
    scale_color_manual(values= age_sex_colors,
                       labels = age_sex_labels,
                       guide = guide_legend(override.aes = list(alpha = 1),
                                            direction = "horizontal")) +
    scale_fill_manual(values = age_sex_colors,
                      labels = age_sex_labels) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    xlab("pressure (kPa)") + ylab("haul-out probability")
  
  p_precip <- create_ribbon_newdata(ribbon_fit_obj, margins=TRUE, 
                                    solar_hour = 12, yday = 135, 
                                    term = "precip") %>% 
    mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
           month = lubridate::month(date,label = TRUE),
           day = lubridate::day(date)) %>% 
    filter(age_sex != "YOUNG OF YEAR", 
           precip < 1) %>% 
    group_by(age_sex) %>% 
    
    ggplot(aes(precip/3, ho_prob, color = age_sex)) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, 
                    fill = age_sex), linetype = 0, alpha = 0.2,
                show.legend = FALSE) +
    geom_pointrange(aes(ymin = lower95, ymax = upper95), 
                    size = 0, alpha = 0.25, stroke = 0) +
    geom_line() +
    scale_color_manual(values= age_sex_colors,
                       labels = age_sex_labels,
                       guide = guide_legend(override.aes = list(alpha = 1),
                                            direction = "horizontal")) +
    scale_fill_manual(values = age_sex_colors,
                      labels = age_sex_labels) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    xlab("precip (mm/hr)") + ylab("haul-out probability")
  
  apply_consistent_y_lims <- function(this_plot){
    this_plot & ylim(0, 1)
  }
  
  p_combo <- p_temp + p_wind + p_pressure + p_precip +
    plot_annotation(
      caption = 'marginal effects are calculated for 15 May at local solar noon')
  
  p_combo <- apply_consistent_y_lims(p_combo)
  
  return(p_combo)
}

plot_spotted_wx <- function(spotted_fit_obj, age_sex_colors,age_sex_labels) {
  theme_set(theme_minimal(base_family = 'Spline Sans',
                          base_size = 9))
  theme_update(
    plot.title.position = "plot",
    # left-align title
    plot.caption.position = "plot",
    # right-align caption
    plot.title = element_text(face = "bold"),
    # larger, bold title
    plot.subtitle = element_textbox_simple(
      color = "grey30",
      margin = ggplot2::margin(t = 0, b = 12, r = 60)
    ),
    plot.caption = element_text(color = "grey30"),
    # change color of caption
    panel.grid.minor = element_blank() # no minor grid lines
  )
  
  p_temp <- create_spotted_newdata(spotted_fit_obj, 
                                   margins=TRUE, 
                                   solar_hour = 12, yday = 135, 
                                   term = "temp2m") %>% 
    mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
           month = lubridate::month(date,label = TRUE),
           day = lubridate::day(date)) %>% 
    filter(age_sex != "YOUNG OF YEAR") %>% 
    group_by(age_sex) %>% 
    
    ggplot(aes(temp2m*27, ho_prob, color = age_sex)) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, 
                    fill = age_sex), linetype = 0, alpha = 0.2,
                show.legend = FALSE) +
    geom_pointrange(aes(ymin = lower95, ymax = upper95), 
                    size = 0, alpha = 0.25, stroke = 0,
                    show.legend = FALSE) +
    geom_line() +
    scale_color_manual(values= age_sex_colors,
                       labels = age_sex_labels,
                       name = "Spotted seal",
                       guide = guide_legend(override.aes = list(alpha = 1),
                                            title.position = "top",
                                            title.hjust = 0,
                                            direction = "horizontal")) +
    scale_fill_manual(values = age_sex_colors,
                      labels = age_sex_labels,
                      guide = "none") +
    theme(legend.position = "top",
          legend.justification = "left") +
    xlab("temperature (C)") + ylab("haul-out probability")
  
  p_wind <- create_spotted_newdata(spotted_fit_obj, 
                                   margins=TRUE, 
                                   solar_hour = 12, yday = 135, 
                                   term = "wind") %>%
    mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
           month = lubridate::month(date,label = TRUE),
           day = lubridate::day(date)) %>% 
    filter(age_sex != "YOUNG OF YEAR") %>% 
    group_by(age_sex) %>% 
    
    ggplot(aes(wind*10, ho_prob, color = age_sex)) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, 
                    fill = age_sex), linetype = 0, alpha = 0.2,
                show.legend = FALSE) +
    geom_pointrange(aes(ymin = lower95, ymax = upper95), 
                    size = 0, alpha = 0.25, stroke = 0) +
    geom_line() +
    scale_color_manual(values= age_sex_colors,
                       labels = age_sex_labels,
                       guide = guide_legend(override.aes = list(alpha = 1),
                                            direction = "horizontal")) +
    scale_fill_manual(values = age_sex_colors,
                      labels = age_sex_labels,
                      guide = "none") +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    xlab("wind (m/s)") + ylab("haul-out probability")
  
  p_pressure <- create_spotted_newdata(spotted_fit_obj, margins=TRUE, 
                                       solar_hour = 12, yday = 135, 
                                       term = "pressure") %>%
    mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
           month = lubridate::month(date,label = TRUE),
           day = lubridate::day(date)) %>% 
    filter(age_sex != "YOUNG OF YEAR") %>% 
    group_by(age_sex) %>% 
    
    ggplot(aes(((pressure * 10000) + 100000)/1000, ho_prob, color = age_sex)) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, 
                    fill = age_sex), linetype = 0, alpha = 0.2,
                show.legend = FALSE) +
    geom_pointrange(aes(ymin = lower95, ymax = upper95), 
                    size = 0, alpha = 0.25, stroke = 0) +
    geom_line() +
    scale_color_manual(values= age_sex_colors,
                       labels = age_sex_labels,
                       guide = guide_legend(override.aes = list(alpha = 1),
                                            direction = "horizontal")) +
    scale_fill_manual(values = age_sex_colors,
                      labels = age_sex_labels,
                      guide = "none") +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    xlab("pressure (kPa)") + ylab("haul-out probability")
  
  p_precip <- create_spotted_newdata(spotted_fit_obj, margins=TRUE, 
                                     solar_hour = 12, yday = 135, 
                                     term = "precip") %>% 
    mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
           month = lubridate::month(date,label = TRUE),
           day = lubridate::day(date)) %>% 
    filter(age_sex != "YOUNG OF YEAR", 
           precip < 1) %>% 
    group_by(age_sex) %>% 
    
    ggplot(aes(precip/3, ho_prob, color = age_sex)) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, 
                    fill = age_sex), linetype = 0, alpha = 0.2,
                show.legend = FALSE) +
    geom_pointrange(aes(ymin = lower95, ymax = upper95), 
                    size = 0, alpha = 0.25, stroke = 0) +
    geom_line() +
    scale_color_manual(values= age_sex_colors,
                       labels = age_sex_labels,
                       guide = guide_legend(override.aes = list(alpha = 1),
                                            direction = "horizontal")) +
    scale_fill_manual(values = age_sex_colors,
                      labels = age_sex_labels,
                      guide = "none") +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    xlab("precip (mm/hr)") + ylab("haul-out probability")
  
  apply_consistent_y_lims <- function(this_plot){
    this_plot & ylim(0, 1)
  }
  
  p_combo <- p_temp + p_wind + p_pressure + p_precip +
    plot_annotation(
      caption = 'marginal effects are calculated for 15 May at local solar noon')
  
  p_combo <- apply_consistent_y_lims(p_combo)
  
  return(p_combo)
}

plot_bearded_wx <- function(bearded_fit_obj, age_sex_colors,age_sex_labels) {
  theme_set(theme_minimal(base_family = 'Spline Sans',
                          base_size = 9))
  theme_update(
    plot.title.position = "plot",
    # left-align title
    plot.caption.position = "plot",
    # right-align caption
    plot.title = element_text(face = "bold"),
    # larger, bold title
    plot.subtitle = element_textbox_simple(
      color = "grey30",
      margin = ggplot2::margin(t = 0, b = 12, r = 60)
    ),
    plot.caption = element_text(color = "grey30"),
    # change color of caption
    panel.grid.minor = element_blank() # no minor grid lines
  )
  
  p_temp <- create_bearded_newdata(bearded_fit_obj, 
                                  margins=TRUE, 
                                  solar_hour = 12, yday = 135, 
                                  term = "temp2m") %>% 
    mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
           month = lubridate::month(date,label = TRUE),
           day = lubridate::day(date)) %>% 
    group_by(age_sex) %>% 
    
    ggplot(aes(temp2m*27, ho_prob, color = age_sex)) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, 
                    fill = age_sex), linetype = 0, alpha = 0.2,
                show.legend = FALSE) +
    geom_pointrange(aes(ymin = lower95, ymax = upper95), 
                    size = 0, alpha = 0.25, stroke = 0,
                    show.legend = FALSE) +
    geom_line() +
    scale_color_manual(values= age_sex_colors,
                       labels = age_sex_labels,
                       name = "Bearded seal",
                       guide = guide_legend(override.aes = list(alpha = 1),
                                            title.position = "top",
                                            title.hjust = 0,
                                            direction = "horizontal")) +
    scale_fill_manual(values = age_sex_colors,
                      labels = age_sex_labels,
                      guide = "none") +
    theme(legend.position = "top",
          legend.justification = "left") +
    xlab("temperature (C)") + ylab("haul-out probability")
  
  p_wind <- create_bearded_newdata(bearded_fit_obj, 
                                  margins=TRUE, 
                                  solar_hour = 12, yday = 135, 
                                  term = "wind") %>%
    mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
           month = lubridate::month(date,label = TRUE),
           day = lubridate::day(date)) %>% 
    group_by(age_sex) %>% 
    
    ggplot(aes(wind*10, ho_prob, color = age_sex)) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, 
                    fill = age_sex), linetype = 0, alpha = 0.2,
                show.legend = FALSE) +
    geom_pointrange(aes(ymin = lower95, ymax = upper95), 
                    size = 0, alpha = 0.25, stroke = 0) +
    geom_line() +
    scale_color_manual(values= age_sex_colors,
                       labels = age_sex_labels,
                       guide = guide_legend(override.aes = list(alpha = 1),
                                            direction = "horizontal")) +
    scale_fill_manual(values = age_sex_colors,
                      labels = age_sex_labels,
                      guide = "none") +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    xlab("wind (m/s)") + ylab("haul-out probability")
  
  p_pressure <- create_bearded_newdata(bearded_fit_obj, margins=TRUE, 
                                      solar_hour = 12, yday = 135, 
                                      term = "pressure") %>%
    mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
           month = lubridate::month(date,label = TRUE),
           day = lubridate::day(date)) %>% 
    group_by(age_sex) %>% 
    
    ggplot(aes(((pressure * 10000) + 100000)/1000, ho_prob, color = age_sex)) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, 
                    fill = age_sex), linetype = 0, alpha = 0.2,
                show.legend = FALSE) +
    geom_pointrange(aes(ymin = lower95, ymax = upper95), 
                    size = 0, alpha = 0.25, stroke = 0) +
    geom_line() +
    scale_color_manual(values= age_sex_colors,
                       labels = age_sex_labels,
                       guide = guide_legend(override.aes = list(alpha = 1),
                                            direction = "horizontal")) +
    scale_fill_manual(values = age_sex_colors,
                      labels = age_sex_labels,
                      guide = "none") +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    xlab("pressure (kPa)") + ylab("haul-out probability")
  
  p_precip <- create_bearded_newdata(bearded_fit_obj, margins=TRUE, 
                                    solar_hour = 12, yday = 135, 
                                    term = "precip") %>% 
    mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
           month = lubridate::month(date,label = TRUE),
           day = lubridate::day(date)) %>% 
    filter(precip < 1) %>% 
    group_by(age_sex) %>% 
    
    ggplot(aes(precip/3, ho_prob, color = age_sex)) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95, 
                    fill = age_sex), linetype = 0, alpha = 0.2,
                show.legend = FALSE) +
    geom_pointrange(aes(ymin = lower95, ymax = upper95), 
                    size = 0, alpha = 0.25, stroke = 0) +
    geom_line() +
    scale_color_manual(values = age_sex_colors,
                       labels = age_sex_labels,
                       guide = guide_legend(override.aes = list(alpha = 1),
                                            direction = "horizontal")) +
    scale_fill_manual(values = age_sex_colors,
                      labels = age_sex_labels,
                      guide = "none") +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    xlab("precip (mm/hr)") + ylab("haul-out probability")
  
  apply_consistent_y_lims <- function(this_plot){
    this_plot & ylim(0, 1)
  }
  p_combo <- p_temp + p_wind + p_pressure + p_precip +
    plot_annotation(
      caption = 'marginal effects are calculated for 15 May at local solar noon')
  
  p_combo <- apply_consistent_y_lims(p_combo)
  
  return(p_combo)
}