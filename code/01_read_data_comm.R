library(tidyverse)

# install.packages("neonDivData", repos = 'https://daijiang.r-universe.dev')
# install.packages("neonOS")

neonDivData::data_algae

f_taxa <- "data/taxa_lookup.csv"
if (!file.exists(f_taxa)) {
  taxa_lookup <- neonDivData::data_algae |>
    select(taxon_id, taxon_name) |>
    distinct() |>
    left_join(
      neonOS::getTaxonList("ALGAE") |> # pulls the full NEON taxonomy
        select(taxonID, class) |>
        rename(taxon_id = taxonID),
      by = "taxon_id"
    )

  write_csv(taxa_lookup, "data/taxa_lookup.csv")
} else {
  taxa_lookup <- read_csv("data/taxa_lookup.csv")
}

(
  df_phyto <- neonDivData::data_algae |>
    filter(str_detect(location_id, "CRAM\\.AOS\\.riparian\\.point\\.(01|02|03|05|05)")) |>
    filter(variable_name == "cell density", sampleCondition == "Condition OK") |>
    select(location_id, unique_sample_id, observation_datetime, taxon_id, taxon_name, value)
)

# ── Data prep ──────────────────────────────────────────────────────────────────

df <- df_phyto |>
  left_join(taxa_lookup) |>
  drop_na(class)
