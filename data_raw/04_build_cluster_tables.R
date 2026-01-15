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

# ============================================================
# 1) TABLE: SENTENCES PER CLUSTER (top sentences etc.)
# ============================================================

# Keep top sentences per cluster by both representative vector and centroid similarity
sentences[,
  rank_rv := frank(-similarity_rv, ties.method = "dense"),
  by = .(
    HDBSCAN_cluster,
    cluster_id,
    backbone_community,
    window
  )
]
sentences[,
  rank_centroid := frank(-similarity_centroid, ties.method = "dense"),
  by = .(
    HDBSCAN_cluster,
    cluster_id,
    backbone_community,
    window
  )
]

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

cluster_sentences[,
  title := paste0(
    "<a href='",
    url,
    "' target='_blank'>",
    title,
    "</a>"
  )
]

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
    window,
    url
  )
]

# Keep only top 20 per cluster
top_articles <- top_articles[
  order(backbone_community, HDBSCAN_cluster, cluster_id, window, -n_sentences)
][,
  head(.SD, 20),
  by = .(HDBSCAN_cluster, backbone_community, window)
]

top_articles[,
  title := paste0(
    "<a href='",
    url,
    "' target='_blank'>",
    title,
    "</a>"
  )
]

write_rds(
  top_articles,
  here("data", "cluster_top_articles.rds")
)

# ============================================================
# 3) TABLE: MOST CITED REFERENCES
# ============================================================

# References are part of clusters when their WOS ID matches an article
# in the clustered sentences dataset.

cluster_labels <- backbone_network %>%
  tidygraph::activate("nodes") %>%
  as_tibble() %>%
  select(cluster_id, label_hdbscan_cluster)

cluster_name_lookup <- sentences_art %>%
  distinct(id_wos_matched, cluster_id) %>%
  mutate(id_wos_matched = as.integer(id_wos_matched)) %>%
  left_join(cluster_labels, by = "cluster_id") %>%
  group_by(id_wos_matched) %>%
  summarise(
    ref_cluster_name = paste(
      sort(unique(label_hdbscan_cluster)),
      collapse = " | "
    ),
    .groups = "drop"
  )

# --- WITH stable ID ---
refs_with_id <- sentences_art %>%
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
    name = first(name),
    year = first(year),
    journal_abbrev = first(journal_abbrev)
  ) %>%
  mutate(ItemID_Ref = as.integer(ItemID_Ref)) %>%
  left_join(
    cluster_name_lookup,
    by = c("ItemID_Ref" = "id_wos_matched")
  ) %>%
  mutate(
    ref_in_cluster = !is.na(ref_cluster_name),
    ref_cluster_name = replace_na(ref_cluster_name, "")
  ) %>%
  ungroup()


# --- WITHOUT stable ID ---
refs_no_id <- sentences_art %>%
  left_join(
    refs,
    by = c("id_wos_matched" = "ID_Art"),
    relationship = "many-to-many"
  ) %>%
  filter(ItemID_Ref == 0 & name != "" & year != 0) %>%
  group_by(
    backbone_community,
    HDBSCAN_cluster,
    cluster_id,
    window,
    name,
    year
  ) %>%
  reframe(
    absolute_cluster_cit = n(),
    journal_abbrev = first(journal_abbrev)
  ) %>%
  mutate(
    ref_in_cluster = FALSE,
    ref_cluster_name = ""
  ) %>%
  ungroup()

# Merge
cluster_refs_all <- bind_rows(
  mutate(refs_with_id, has_id = TRUE),
  mutate(refs_no_id, has_id = FALSE)
)

# Top 20 references per cluster for UI
cluster_top_refs <- cluster_refs_all %>%
  group_by(backbone_community, HDBSCAN_cluster, window) %>%
  slice_max(order_by = absolute_cluster_cit, n = 20) %>%
  ungroup()

# All references that are themselves part of a cluster
cluster_refs_in_cluster <- cluster_refs_all %>%
  filter(ref_in_cluster)

write_rds(
  cluster_top_refs,
  here("data", "cluster_top_references.rds")
)

write_rds(
  cluster_refs_in_cluster,
  here("data", "cluster_refs_in_cluster.rds")
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
