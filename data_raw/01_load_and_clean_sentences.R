# ============================================================
# 01_load_and_clean_sentences.R
# Build cleaned sentences + metadata + references
# ============================================================

# Load paths + packages
source(here::here("data_raw", "paths_and_packages.R"))
p_load(
  here,
  tidyverse,
  data.table,
  arrow,
  stringr,
  glue,
  lubridate,
  see,
  scico
)

# ------------------------------------------------------------
# 1. Metadata
# ------------------------------------------------------------

metadata <- read_feather(file.path(ejhet_project, "metadata_maintext.feather"))

# ------------------------------------------------------------
# 2. Merge metadata + nb sentences
# ------------------------------------------------------------

# nb_sentences <- read_feather(file.path(
#   ejhet_project,
#   "n_sentences_per_id_after_cleaning.feather"
# ))

# metadata <- metadata %>%
#   filter(!is.na(year), !is.na(title)) %>%
#   left_join(nb_sentences, by = "id")

setDT(metadata)

# save cleaned metadata
write_rds(metadata, here("data_raw", "metadata_articles.rds"))

# ------------------------------------------------------------
# 3. Load SENTENCES dataset
# ------------------------------------------------------------
sentences_dataset <- open_dataset(
  file.path(future_project, "sentences_intertemporal_cluster.feather"),
  format = "feather"
)

sentences <- sentences_dataset %>%
  filter(between(year, 1900, 2009)) %>%
  select(
    id,
    sentence_id,
    cluster_id,
    HDBSCAN_cluster,
    window,
    backbone_community,
    sentence,
    similarity_rv,
    embedding,
    centroid
  ) %>%
  collect() %>%
  inner_join(metadata, by = "id") %>%
  mutate(
    # Cosine similarity of each sentence embedding to its cluster centroid
    similarity_centroid = purrr::map2_dbl(
      embedding,
      centroid,
      function(vec, ctr) {
        num <- sum(vec * ctr)
        denom <- sqrt(sum(vec * vec)) * sqrt(sum(ctr * ctr))
        if (is.na(denom) || denom == 0) {
          return(NA_real_)
        }
        num / denom
      }
    )
  ) %>%
  select(-centroid) %>% # centroid used for similarity only; drop to keep file size stable
  arrange(id, sentence_id)

setDT(sentences)

# Save cleaned sentences
write_rds(sentences, here("data_raw", "sentences_cleaned.rds"))

# Keep list of backbone communities
list_clusters <- sort(unique(sentences$backbone_community))
write_rds(list_clusters, here("data_raw", "backbone_community_list.rds"))

rm(metadata)
gc()

# ------------------------------------------------------------
# 5. References (Web of Science)
# ------------------------------------------------------------
sentences_art <- sentences[
  !is.na(id_wos_matched),
  .N,
  .(id, cluster_id, HDBSCAN_cluster, window, backbone_community, id_wos_matched)
]
setnames(sentences_art, "N", "nb_sentence")

# Load chosen references
refs <- open_dataset(
  here::here(wos_data_path, "all_ref.parquet"),
  format = "parquet"
) %>%
  filter(ID_Art %in% unique(sentences_art$id_wos_matched)) %>%
  distinct(ID_Art, ItemID_Ref, Annee, Nom, Revue_Abbrege) %>%
  collect()

write_rds(sentences_art, here("data_raw", "sentences_art.rds"))
write_rds(refs, here("data_raw", "references_wos.rds"))

rm(refs)
gc()

# ------------------------------------------------------------
# 6. Backbone network raw input
# ------------------------------------------------------------
backbone_network_raw <- read_rds(file.path(
  future_project,
  "backbone_network_of_sentences_clusters.rds"
))

write_rds(backbone_network_raw, here("data_raw", "backbone_network_raw.rds"))

# ------------------------------------------------------------
# 7. Save window levels
# ------------------------------------------------------------
window_levels <- sentences %>%
  pull(window) %>%
  unique() %>%
  sort()

write_rds(window_levels, here("data", "window_levels.rds"))
