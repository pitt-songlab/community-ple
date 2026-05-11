library(ggfortify)

# ── PCA on environmental variables ────────────────────────────────────────────

env_matrix <- df_daymet |>
  select(
    dayl..s., prcp..mm.day., srad..W.m.2.,
    tmax..deg.c., tmin..deg.c., vp..Pa.
  ) |>
  rename(
    dayl = dayl..s., prcp = prcp..mm.day., srad = srad..W.m.2.,
    tmax = tmax..deg.c., tmin = tmin..deg.c., vp = vp..Pa.
  )

pca <- prcomp(env_matrix, scale. = TRUE, center = TRUE)
summary(pca)


# ── Plot ───────────────────────────────────────────────────────────────────────

# Extract scores and loadings manually
pca_scores <- as_tibble(pca$x) |>
  bind_cols(df_daymet |> mutate(month = lubridate::month(observation_datetime)))

pca_loadings <- as_tibble(pca$rotation, rownames = "variable") |>
  mutate(
    PC1 = PC1 * max(abs(pca$x[, 1])), # scale arrows to match point spread
    PC2 = PC2 * max(abs(pca$x[, 2]))
  )

var_explained <- summary(pca)$importance[2, ] * 100

ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = month), size = 3, alpha = 0.85) +
  # Arrows
  geom_segment(
    data = pca_loadings,
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
    colour = "grey30", linewidth = 0.6
  ) +
  # Arrow labels
  ggrepel::geom_text_repel(
    data = pca_loadings,
    aes(x = PC1, y = PC2, label = variable),
    colour = "black", size = 3.5,
    max.overlaps = Inf
  ) +
  scale_colour_viridis_c() +
  labs(
    x      = paste0("PC1 (", round(var_explained[1], 1), "%)"),
    y      = paste0("PC2 (", round(var_explained[2], 1), "%)"),
    colour = "Month"
  ) +
  theme_classic(base_size = 13)
