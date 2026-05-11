library(vegan)

# ── Build site × class matrix ──────────────────────────────────────────────────

wide <- df |>
  group_by(location_id, observation_datetime, class) |>
  summarise(density = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(location_id, observation_datetime) |>
  mutate(rel_abund = density / sum(density)) |>
  ungroup() |>
  group_by(location_id, observation_datetime, class) |>
  summarise(density = sum(rel_abund, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = class, values_from = density, values_fill = 0)

comm_matrix <- wide |>
  select(-location_id, -observation_datetime) |>
  as.matrix()

# Drop any all-zero rows (samples where nothing was retained after class filter)
keep <- rowSums(comm_matrix) > 0
comm_matrix <- comm_matrix[keep, ]
wide <- wide[keep, ]

# ── Run NMDS ───────────────────────────────────────────────────────────────────

set.seed(42)
nmds <- metaMDS(comm_matrix, distance = "bray", k = 2, trymax = 100, trace = FALSE)
cat("Stress:", round(nmds$stress, 3), "\n") # want < 0.2

sample_scores <- scores(nmds, display = "sites")
species_scores <- scores(nmds, display = "species") |>
  as_tibble(rownames = "class")

# ── Plot ───────────────────────────────────────────────────────────────────────

sample_scores |>
  as_tibble() |>
  bind_cols(wide |> select(location_id, observation_datetime)) |>
  ggplot(aes(x = NMDS1, y = NMDS2)) +
  stat_ellipse(aes(group = location_id, colour = location_id),
    geom = "polygon",
    fill = NA,
    level = 0.75
  ) +
  geom_point(aes(colour = location_id), size = 3, alpha = 0.85) +
  ggrepel::geom_text_repel(
    data = species_scores,
    aes(x = NMDS1, y = NMDS2, label = class),
    colour = "black", size = 3.5, fontface = "italic",
    max.overlaps = Inf
  ) +
  scale_colour_viridis_d() +
  annotate("text",
    x = Inf, y = -Inf,
    label = paste0("Stress = ", round(nmds$stress, 3)),
    hjust = 1.1, vjust = -0.5, size = 3.5, colour = "grey40"
  ) +
  labs(x = "NMDS1", y = "NMDS2", colour = "Location") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

sample_scores |>
  as_tibble() |>
  bind_cols(wide |>
    select(location_id, observation_datetime) |>
    mutate(
      doy = lubridate::yday(observation_datetime),
      month = lubridate::month(observation_datetime)
    )) |>
  ggplot(aes(x = NMDS1, y = NMDS2)) +
  stat_ellipse(aes(group = month, colour = month),
    geom = "polygon",
    fill = NA,
    level = 0.75
  ) +
  geom_point(aes(colour = month), size = 3, alpha = 0.85) +
  ggrepel::geom_text_repel(
    data = species_scores,
    aes(x = NMDS1, y = NMDS2, label = class),
    colour = "black", size = 3.5, fontface = "italic",
    max.overlaps = Inf
  ) +
  scale_colour_viridis_c() +
  annotate("text",
    x = Inf, y = -Inf,
    label = paste0("Stress = ", round(nmds$stress, 3)),
    hjust = 1.1, vjust = -0.5, size = 3.5, colour = "grey40"
  ) +
  labs(x = "NMDS1", y = "NMDS2", colour = "Month") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))


# PERMANOVA with month as factor — tests if months differ in composition
adonis2(comm_matrix ~ location_id,
  data = wide |>
    select(location_id),
  permutations = 999,
  method = "bray"
)

adonis2(comm_matrix ~ factor(month),
  data = wide |>
    select(location_id, observation_datetime) |>
    mutate(
      doy = lubridate::yday(observation_datetime),
      month = lubridate::month(observation_datetime)
    ),
  permutations = 999,
  method = "bray"
)
