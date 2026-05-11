df_daymet_full |>
  ggplot(aes(x = date, y = value, colour = measurement, group = location_id)) +
  geom_line(show.legend = F) +
  scale_colour_viridis_d() +
  facet_wrap(~measurement, scales = "free_y", ncol = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
