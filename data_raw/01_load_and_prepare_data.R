# ============================================================
# 01_load_and_prepare_data.R
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
# 1b. Article centroids (external)
# ------------------------------------------------------------
article_centroids_raw <- read_feather(
  file.path(ejhet_project, "fulltexts_cosine_sim_with_rv.feather")
)

article_centroids <- article_centroids_raw %>%
  select(
    id,
    centroid = doc_average_embedding
  )

write_rds(article_centroids, here("data_raw", "article_centroids.rds"))
# remove raw to save memory
rm(article_centroids_raw)
gc()

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
# setDT(metadata)

metadata[,
  url := if_else(
    str_detect(id, "jstor"),
    id,
    str_c("https://www.doi.org/", doi)
  )
]

# save cleaned metadata
write_rds(metadata, here("data_raw", "metadata_articles.rds"))

# ------------------------------------------------------------
# 3. Load SENTENCES dataset
# ------------------------------------------------------------
sentences_dataset <- open_dataset(
  file.path(future_project, "sentences_intertemporal_cluster.feather"),
  format = "feather"
)

# collect minimal data, convert to data.table and join metadata (metadata is already a data.table)
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
    embedding
  ) %>%
  collect()

setDT(sentences, key = "sentence_id")
sentences <- merge(sentences, metadata, by = "id", all = FALSE)

# compute cluster centroids (one numeric vector per cluster)
centroids_dt <- sentences[,
  .(centroid = list(colMeans(do.call(rbind, embedding)))),
  by = cluster_id
]

# make a named list for fast lookup
cent_list <- setNames(
  centroids_dt$centroid,
  as.character(centroids_dt$cluster_id)
)

# cosine similarity between each sentence embedding and its cluster centroid
similarity_vec <- vapply(
  seq_len(nrow(sentences)),
  function(i) {
    vec <- sentences$embedding[[i]]
    ctr <- cent_list[[as.character(sentences$cluster_id[i])]]
    if (is.null(ctr) || any(is.na(vec)) || any(is.na(ctr))) {
      return(NA_real_)
    }
    num <- sum(vec * ctr)
    denom <- sqrt(sum(vec * vec)) * sqrt(sum(ctr * ctr))
    if (denom == 0) NA_real_ else num / denom
  },
  numeric(1)
)

sentences[, similarity_centroid := similarity_vec]

# keep ordering and return data.table
setorder(sentences, id, sentence_id)

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
  .(
    id,
    cluster_id,
    HDBSCAN_cluster,
    window,
    backbone_community,
    id_wos_matched,
    url
  )
]
setnames(sentences_art, "N", "nb_sentence")

# Load chosen references
refs <- open_dataset(
  here::here(wos_data_path, "all_ref.parquet"),
  format = "parquet"
) %>%
  filter(ID_Art %in% unique(sentences_art$id_wos_matched)) %>%
  distinct(ID_Art, ItemID_Ref, Annee, Nom, Revue_Abbrege) %>%
  collect() %>%
  rename(
    year = Annee,
    name = Nom,
    journal_abbrev = Revue_Abbrege
  )

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
