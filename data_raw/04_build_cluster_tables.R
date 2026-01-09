# ============================================================
# 04_build_cluster_tables.R
# Create tables used by Shiny when clicking a node
# ============================================================

source(here::here("data_raw", "paths_and_packages.R"))
p_load(tidyverse, data.table, arrow)

# ------------------------------------------------------------
# Load objects
# ------------------------------------------------------------
sentences <- read_rds(here("data_raw", "sentences_cleaned.rds"))
sentences_art <- read_rds(here("data_raw", "sentences_art.rds"))
refs <- read_rds(here("data_raw", "references_wos.rds"))
metadata <- read_rds(here("data_raw", "metadata_articles.rds"))

# backbone_network already contains:
# cluster_id, HDBSCAN_cluster, backbone_community,
# label_hdbscan_cluster, label_backbone_community,
# proportion_hdbscan_cluster
backbone_network <- read_rds(here("data", "backbone_network.rds"))

setDT(sentences)

# ============================================================
# 1) TABLE: SENTENCES PER CLUSTER (top sentences etc.)
# ============================================================

# Keep top sentences per cluster by both representative vector and centroid similarity
sentences[, rank_rv := frank(-similarity_rv, ties.method = "dense"), by = .(
  HDBSCAN_cluster, cluster_id, backbone_community, window
)]
sentences[, rank_centroid := frank(-similarity_centroid, ties.method = "dense"), by = .(
  HDBSCAN_cluster, cluster_id, backbone_community, window
)]

cluster_sentences <- sentences[
  rank_rv <= 25 | rank_centroid <= 25
][
  order(HDBSCAN_cluster, cluster_id, window, -similarity_rv)
]

# Add metadata
cluster_sentences <- cluster_sentences[,
  .(
    cluster_id,
    HDBSCAN_cluster,
    backbone_community,
    window,
    id,
    sentence_id,
    sentence,
    year,
    title,
    journal,
    authors,
    url,
    similarity_rv,
    similarity_centroid
  )
]

sentences[, c("rank_rv", "rank_centroid") := NULL]

# FLAG rationality sentences
cluster_sentences[,
  is_rational := str_detect(
    tolower(sentence),
    "rational|rationality"
  )
]

write_rds(
  cluster_sentences,
  here("data", "cluster_sentences.rds")
)

# ============================================================
# 2) TABLE: TOP ARTICLES PER CLUSTER
# ============================================================

# Count sentences per article × cluster
top_articles <- sentences[,
  .(
    n_sentences = .N,
    mean_similarity = mean(similarity_rv, na.rm = TRUE)
  ),
  by = .(
    id,
    title,
    journal,
    authors,
    year,
    HDBSCAN_cluster,
    cluster_id,
    backbone_community,
    window
  )
]

# Keep only top 20 per cluster
top_articles <- top_articles[
  order(backbone_community, HDBSCAN_cluster, cluster_id, window, -n_sentences)
][,
  head(.SD, 20),
  by = .(HDBSCAN_cluster, backbone_community, window)
]

write_rds(
  top_articles,
  here("data", "cluster_top_articles.rds")
)

# ============================================================
# 3) TABLE: MOST CITED REFERENCES
# ============================================================

# --- WITH stable ID ---
top_refs <- sentences_art %>%
  left_join(
    refs,
    by = c("id_wos_matched" = "ID_Art"),
    relationship = "many-to-many"
  ) %>%
  filter(ItemID_Ref != 0) %>%
  group_by(
    cluster_id,
    HDBSCAN_cluster,
    backbone_community,
    window,
    ItemID_Ref
  ) %>%
  reframe(
    absolute_cluster_cit = n(),
    Nom = first(Nom),
    Annee = first(Annee),
    Revue_Abbrege = first(Revue_Abbrege)
  ) %>%
  group_by(backbone_community, HDBSCAN_cluster, window) %>%
  slice_max(order_by = absolute_cluster_cit, n = 15) %>%
  ungroup()

# --- WITHOUT stable ID ---
top_refs_noid <- sentences_art %>%
  left_join(
    refs,
    by = c("id_wos_matched" = "ID_Art"),
    relationship = "many-to-many"
  ) %>%
  filter(ItemID_Ref == 0 & Nom != "" & Annee != 0) %>%
  group_by(
    backbone_community,
    HDBSCAN_cluster,
    cluster_id,
    window,
    Nom,
    Annee
  ) %>%
  reframe(
    absolute_cluster_cit = n(),
    Revue_Abbrege = first(Revue_Abbrege)
  ) %>%
  group_by(backbone_community, HDBSCAN_cluster, window) %>%
  slice_max(order_by = absolute_cluster_cit, n = 15) %>%
  ungroup()

# Merge
cluster_top_refs <- bind_rows(
  mutate(top_refs, has_id = TRUE),
  mutate(top_refs_noid, has_id = FALSE)
)

write_rds(
  cluster_top_refs,
  here("data", "cluster_top_references.rds")
)

# ============================================================
# 4) TABLE: RECURRING AUTHORS
# ============================================================

recurring_authors <- sentences[
  !is.na(authors),
  .(authors = unlist(strsplit(authors, ",\\s+"))),
  by = .(id, backbone_community, HDBSCAN_cluster, cluster_id, window)
][,
  authors := trimws(authors)
][
  authors != ""
][,
  .(
    nb_sentences = .N,
    nb_articles = uniqueN(id)
  ),
  by = .(backbone_community, HDBSCAN_cluster, cluster_id, window, authors)
][order(-nb_sentences)][
  nb_articles > 1
][,
  head(.SD, 10),
  by = .(backbone_community, HDBSCAN_cluster, cluster_id, window)
]

write_rds(
  recurring_authors,
  here("data", "cluster_author_stats.rds")
)
