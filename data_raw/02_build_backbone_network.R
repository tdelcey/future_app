# ============================================================
# 02_build_backbone_network.R
# Build enriched backbone network + layout + colors
# ============================================================

source(here::here("data_raw", "paths_and_packages.R"))
source(here::here("utils", "text_functions.R"))
p_load(tidyverse, data.table, arrow, ggraph, tidygraph, scico, tidyr)

# ------------------------------------------------------------
# Load data from script 01
# ------------------------------------------------------------
sentences <- read_rds(here("data_raw", "sentences_cleaned.rds"))
backbone_network_raw <- read_rds(here("data_raw", "backbone_network_raw.rds"))
window_levels <- read_rds(here("data", "window_levels.rds"))

setDT(sentences)

# ------------------------------------------------------------
# 1. Compute proportions per (HDBSCAN cluster × window)
# ------------------------------------------------------------
sentences[, size_window := .N, by = window]
sentences[,
  proportion_hdbscan_cluster := .N / size_window,
  by = .(HDBSCAN_cluster, window)
]

# ------------------------------------------------------------
# 2. COMPUTE TF-IDF LABELS FOR COMMUNITIES
# ------------------------------------------------------------

# TF-IDF (if you already computed them upstream, load them here instead)
# Otherwise, recompute — but as discussed, typically we load the saved ones.
# Here we recompute the minimal labels needed for the network plots:

tokens <- extract_ngrams(
  sentences[, .(
    sentence,
    window,
    HDBSCAN_cluster,
    cluster_id,
    backbone_community
  )],
  ngrams = 2L,
  grouping_cols = c(
    "HDBSCAN_cluster",
    "cluster_id",
    "backbone_community",
    "window"
  ),
  text_col = "sentence",
)

tokens_count_hdbscan_cluster <- compute_tf_idf(
  tokens,
  token_col = "token",
  document_col = c(
    "backbone_community",
    "cluster_id",
    "HDBSCAN_cluster",
    "window"
  )
)

# Labels per decade
labels_hdbscan_cluster <- tokens_count_hdbscan_cluster[order(-tf_idf)][
  corpus_tf > 20,
  head(.SD, 5),
  by = .(backbone_community, HDBSCAN_cluster, cluster_id, window)
]

labels_hdbscan_cluster <- labels_hdbscan_cluster[,
  .(label_hdbscan_cluster = paste0(token, collapse = ", ")),
  by = .(backbone_community, HDBSCAN_cluster, cluster_id, window)
]

labels_hdbscan_cluster[,
  label_hdbscan_cluster := paste0(
    backbone_community,
    "_",
    HDBSCAN_cluster,
    "_",
    window,
    ": ",
    label_hdbscan_cluster
  ) # we save the backbonne community number to see difference in top terms depending on hdbscan cluster
]

sentences <- merge(
  sentences,
  labels_hdbscan_cluster,
  all.x = TRUE,
  by = c("backbone_community", "HDBSCAN_cluster", "cluster_id", "window")
)

# we now compute meta-clusters labels (backbone communities)

tfidf_mean_community <- tokens_count_hdbscan_cluster[
  corpus_tf > 50,
  .(mean_tf_idf = mean(tf_idf)),
  by = .(token, backbone_community)
][order(-mean_tf_idf)]

# Top tf-idf terms
top_terms_cluster <- tfidf_mean_community[,
  head(.SD, 20),
  by = backbone_community
]

# Get top 4 tf-idf tokens per cluster
labels_backbone_community <- tfidf_mean_community[,
  head(.SD, 4),
  by = backbone_community
][, .(backbone_community, token)]

labels_backbone_community <- labels_backbone_community[,
  .(label_backbone_community = paste0(token, collapse = ", ")),
  by = backbone_community
]

labels_backbone_community[,
  label_backbone_community := paste0(
    str_extract(backbone_community, "\\d+$"),
    ": ",
    label_backbone_community
  )
]

sentences <- merge(
  sentences,
  labels_backbone_community,
  all.x = TRUE,
  by = "backbone_community"
)


# save a table that will be used in the app for the DT of cluster labels

top_terms_hdbscan_cluster <- tokens_count_hdbscan_cluster[
  order(-tf_idf)
][
  corpus_tf > 20
][,
  head(.SD, 15),
  by = .(backbone_community, HDBSCAN_cluster, window)
]


# rename HDBSCAN_cluster by cluster_id for consistency with other tables

tfidf_tables <- list(
  top_terms_hdbscan_cluster = top_terms_hdbscan_cluster,
  top_terms_cluster = top_terms_cluster,
  labels_hdbscan_cluster = labels_hdbscan_cluster,
  labels_backbone_community = labels_backbone_community
)

write_rds(
  tfidf_tables,
  here("data", "tfidf_tables.rds")
)


# ------------------------------------------------------------
# 3. Build the enriched backbone network
# ------------------------------------------------------------
backbone_network <- backbone_network_raw %>%
  activate("nodes") %>%
  left_join(
    sentences %>%
      distinct(
        cluster_id,
        HDBSCAN_cluster,
        backbone_community,
        label_hdbscan_cluster,
        label_backbone_community,
        proportion_hdbscan_cluster
      ),
    by = "cluster_id"
  )

# # ------------------------------------------------------------
# # 4. Edge-level community assignment
# # ------------------------------------------------------------
# backbone_network <- backbone_network %>%
#   tidygraph::activate("edges") %>%
#   dplyr::mutate(
#     comm_from = .N()[["backbone_community"]][from],
#     comm_to = .N()[["backbone_community"]][to],
#     same_comm = comm_from == comm_to,

#     # couleur de l'arête = couleur du node from si même communauté
#     edge_color = ifelse(
#       same_comm,
#       .N()[["fill_color"]][from],
#       "#DDDDDD" # gris inter-communautés
#     )
#   )
# ------------------------------------------------------------
# 5. Compute community ordering (comm_levels)
# ------------------------------------------------------------
# Compute backbone centroids by community

emb <- sentences[, .(embedding = embedding), by = backbone_community]
emb <- emb[,
  {
    m <- do.call(rbind, embedding)
    list(centroid = list(colMeans(m)))
  },
  by = backbone_community
]

# Build centroid matrix
centroid_mat <- do.call(rbind, emb$centroid)
mat_scaled <- scale(centroid_mat)

# Cosine similarity
row_norms <- sqrt(rowSums(mat_scaled^2))
row_norms[row_norms == 0] <- NA_real_
sim_mat <- (mat_scaled %*% t(mat_scaled)) / (row_norms %o% row_norms)

# Hierarchical clustering
d <- as.dist(1 - sim_mat)
hc <- hclust(d, method = "centroid")

comm_levels <- emb$backbone_community[hc$order]
write_rds(comm_levels, here("data_raw", "comm_levels.rds"))

# ------------------------------------------------------------
# 6. Color palette
# ------------------------------------------------------------
nb_clusters <- length(comm_levels)
palette_colors <- scico::scico(n = nb_clusters, palette = "roma")
cluster_colors <- palette_colors
names(cluster_colors) <- comm_levels

write_rds(cluster_colors, here("data", "cluster_colors.rds"))

# ------------------------------------------------------------
# 7. Compute layout (fa2)
# ------------------------------------------------------------

backbone_network <- backbone_network %>%
  activate("edges") %>%
  rename(weight = oldweight)


set.seed(89)
backbone_network <- vite::complete_forceatlas2(
  backbone_network,
  kgrav = 1,
  first.iter = 10000,
  overlap.method = "repel",
  overlap.iter = 2000
)

write_rds(backbone_network, here("data", "backbone_network.rds"))
