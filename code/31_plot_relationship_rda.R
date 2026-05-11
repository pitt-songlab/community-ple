library(vegan)

# ── Join env to community matrix ───────────────────────────────────────────────

# wide already has location_id + observation_datetime as keys
env_joined <- wide |>
  left_join(df_daymet, by = c("location_id", "observation_datetime")) |>
  select(
    dayl..s., prcp..mm.day., srad..W.m.2.,
    tmax..deg.c., tmin..deg.c., vp..Pa.
  ) |>
  rename(
    dayl = dayl..s., prcp = prcp..mm.day., srad = srad..W.m.2.,
    tmax = tmax..deg.c., tmin = tmin..deg.c., vp = vp..Pa.
  )

# Make sure rows match comm_matrix (apply same keep filter)
env_scaled <- env_joined |>
  slice(which(keep)) |>
  scale() |>
  as_tibble()

# ── Run RDA ────────────────────────────────────────────────────────────────────

rda_result <- rda(comm_matrix ~ ., data = env_scaled)
# rda_result <- rda(comm_matrix ~ factor(month), data = wide |>
#                     select(location_id, observation_datetime) |>
#                     mutate(
#                       doy = lubridate::yday(observation_datetime),
#                       month = lubridate::month(observation_datetime)
#                     ))
summary(rda_result)

# How much variance is explained?
RsquareAdj(rda_result)

# Test global model and each term
anova(rda_result, permutations = 999)
anova(rda_result, by = "term", permutations = 999)

# ── Extract scores for ggplot ──────────────────────────────────────────────────

site_sc <- scores(rda_result, display = "sites") |> as_tibble()
sp_sc <- scores(rda_result, display = "species") |> as_tibble(rownames = "class")
env_sc <- scores(rda_result, display = "bp") |> as_tibble(rownames = "variable")

# Scale arrows
scale_factor <- max(abs(site_sc)) / max(abs(env_sc[, -1]))
env_sc <- env_sc |> mutate(
  RDA1 = RDA1 * scale_factor,
  RDA2 = RDA2 * scale_factor
)

site_meta <- wide |>
  slice(which(keep)) |>
  select(location_id, observation_datetime) |>
  mutate(
    month = month(observation_datetime, label = TRUE),
    doy = yday(observation_datetime)
  )

# ── Plot ───────────────────────────────────────────────────────────────────────

var_exp <- summary(rda_result)$concont$importance[2, 1:2] * 100

ggplot(site_sc, aes(x = RDA1, y = RDA2)) +
  # Site points
  geom_point(aes(colour = site_meta$doy), size = 3, alpha = 0.85) +
  # Species scores
  ggrepel::geom_text_repel(
    data = sp_sc,
    aes(x = RDA1, y = RDA2, label = class),
    colour = "grey40", size = 3, fontface = "italic",
    max.overlaps = Inf
  ) +
  # Env arrows
  geom_segment(
    data = env_sc,
    aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
    arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
    colour = "firebrick", linewidth = 0.7
  ) +
  ggrepel::geom_text_repel(
    data = env_sc,
    aes(x = RDA1, y = RDA2, label = variable),
    colour = "firebrick", size = 3.5,
    max.overlaps = Inf
  ) +
  scale_colour_viridis_c() +
  labs(
    x      = paste0("RDA1 (", round(var_exp[1], 1), "%)"),
    y      = paste0("RDA2 (", round(var_exp[2], 1), "%)"),
    colour = "Day of year"
  ) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))
