r <- ribbon_newdata |> 
  group_by(age_sex,day,yday) |> 
  summarize(across(c(northing,temp2m,wind,pressure,precip), 
                   ~ mean(.x, na.rm = TRUE)))

ssrd_breaks <- ribbon_model_data$era5_ssrd_watts |> 
  quantile()

r <- r |> expand_grid(tibble(era5_ssrd_watts = ssrd_breaks,
       ssrd_quants = factor(names(ssrd_breaks),
                            levels = c("0%","25%","50%","75%","100%"))))

spenos <- unique(ribbon_model_data$speno) %>% 
  as.character()

r <- r %>% 
  mutate(speno = sample(spenos,1))

ribbon_ssrd_predict <- bind_cols(r,predict(m2_ssrd_ribbon,
                               newdata = r,
                               type = "response",
                               se.fit = TRUE,
                               exclude = "s(speno)")
)

plot_df <- ribbon_ssrd_predict %>%
  mutate(date = lubridate::as_date(yday, origin = "2015-01-01"),
         month = lubridate::month(date,label=TRUE),
         day = lubridate::day(date)) %>%
  filter(!month %in% c("Jul","Aug"))

p2 <- ggplot(plot_df, aes(day, ssrd_quants, fill = fit)) +
  geom_tile(color = "white", linewidth = 0) +
  scale_fill_viridis_c(
    name = "haul-out probability",
    aesthetics = "fill",
    limits = c(0, 1),
    breaks = c(0.25, 0.50, 0.75),
    guide = guide_colorbar(
      title.position = "bottom",
      barwidth = 15,
      barheight = 0.5,
      title.hjust = 0.5
    )
  )

p2 <- p2 + facet_grid(age_sex~month, scales = "free_x")
p2 <- p2 + scale_x_continuous(breaks = c(5,10,15,20,25)) +
  coord_cartesian(expand=FALSE)
p2 <- p2 + theme_minimal() +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(legend.position = "bottom") +
  theme(strip.background = element_rect(colour="white")) +
  theme(axis.ticks=element_blank()) +
  xlab("day of month") + ylab("ssrd quantiles")