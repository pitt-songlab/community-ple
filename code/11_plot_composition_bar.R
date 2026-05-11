df |>
  group_by(location_id, observation_datetime, class) |>
  summarise(density = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(location_id, observation_datetime) |>
  mutate(rel_abund = density / sum(density)) |>
  ungroup() |>
  ggplot(aes(x = observation_datetime, y = rel_abund, fill = class)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_datetime(
    breaks = df |> pull(observation_datetime) |> unique() |> sort(),
    date_labels = "%b %d\n%Y"
  ) +
  labs(
    x        = "Time",
    y        = "Relative Abundance",
    fill     = "Class"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  facet_wrap(. ~ location_id, ncol = 1)
