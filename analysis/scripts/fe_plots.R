library(dplyr)
library(tibble)
library(glue)
library(ggplot2)

load(here::here("data/fit_ribbon.Rdata"))

age_sex_terms <- tibble(facet_label = "age_sex",
                        effect_label = c("intercept","ADULT.M", "SUBADULT", "YOUNG OF YEAR")
)

wx_terms <- tibble(facet_label = "weather",
                   effect_label = c("temp2","wind","precip","pressure")
)

time_terms <- tibble(facet_label = "time",
                     effect_label = c("day","day2","day3","sin1","cos1","sin2","cos2","sin3","cos3")
)

interact_terms <- tibble(facet_label = "interaction",
                         effect_label = c("ADULT.M:day", "ADULT.M:day2", "ADULT.M:day3",
                    "SUBADULT:day","SUBADULT:day2","SUBADULT:day3",
                    "YOUNG OF YEAR:day","YOUNG OF YEAR:day2","YOUNG OF YEAR:day3",
                    "sin1:day","sin1:day2","cos1:day","cos1:day2",
                    "sin2:day","sin2:day2","cos2:day","cos2:day2",
                    "sin3:day","sin3:day2","cos3:day","cos3:day2",
                    "temp2:wind")
)

effects_df <- bind_rows(age_sex_terms,wx_terms,time_terms,interact_terms)

# this needs some help to get the order of the effect_labels right and, also,
# reorder the facets; probably need to group_by and create an order col, then set
# the breaks and labels manually

fe <- fit_ribbon$fixed.effects %>%
  dplyr::select(effect, levels, estimate, std.err) %>%
  dplyr::filter(!is.na(std.err)) %>%
  dplyr::mutate(effect_label = glue::glue("{levels}{effect}")) %>%
  dplyr::mutate(effect_label = stringr::str_remove(effect_label, "age_sex"),
                effect_label = stringr::str_remove(effect_label, ",")) %>%
  dplyr::left_join(effects_df) %>%
  dplyr::mutate(effect_label = forcats::as_factor(effect_label) %>%
                  forcats::fct_reorder(c(age_sex_terms$effect_label,
                                                       wx_terms$effect_label,
                                                       time_terms$effect_label,
                                                       interact_terms$effect_label))) %>%
  dplyr::mutate(lower95 = estimate - std.err*1.96,
                upper95 = estimate + std.err*1.96)

ggplot(data = fe, aes(x = estimate, y = effect_label)) +
  geom_pointrange(aes(xmin = lower95, xmax = upper95)) +
  facet_grid(facet_label ~ ., scales = "free_y")
