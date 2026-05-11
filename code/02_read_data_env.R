library(daymetr)

df_meta <- neonDivData::data_algae |>
  filter(str_detect(location_id, "CRAM\\.AOS\\.riparian\\.point\\.(01|02|03|04|05)")) |>
  filter(variable_name == "cell density", sampleCondition == "Condition OK") |>
  distinct(location_id, observation_datetime, latitude, longitude)

# Pull Daymet for each row — download_daymet() fetches a full year at once
df_daymet_full <- df_meta |>
  mutate(
    year = year(observation_datetime)
  ) |>
  select(-observation_datetime) |>
  distinct() |>
  complete(location_id, year) |>
  group_by(location_id) |>
  fill(latitude, longitude, .direction = "downup") |> # fill NA lat/lon from other rows
  ungroup() |>
  pmap(function(observation_datetime, latitude, longitude, year, location_id) {
    download_daymet(
      site      = location_id,
      lat       = latitude,
      lon       = longitude,
      start     = year,
      end       = year,
      simplify  = TRUE,
      silent    = TRUE
    )
  }) |>
  list_rbind() |>
  rename(location_id = site) |>
  mutate(date = as.Date(paste(year, yday), format = "%Y %j")) |>
  select(-tile, -year, -yday)

# Pivot to wide so each variable is a column
df_daymet <- df_daymet_full |>
  right_join(df_meta |>
    mutate(date = lubridate::date(observation_datetime))) |>
  select(location_id, latitude, longitude, observation_datetime, measurement, value) |>
  pivot_wider(names_from = measurement, values_from = value)

df_daymet
# Key Daymet variables you'll get:
# tmax..deg.c.   tmin..deg.c.   prcp..mm.day.
# srad..W.m.2.   vp..Pa.        dayl..s.
# swe..kg.m.2.   (snow water equivalent)
