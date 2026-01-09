# ============================================================
# 03_build_temporal_layout.R
# Build temporal backbone layout (x = window, y = stacked community bands)
# ============================================================

source(here::here("data_raw", "paths_and_packages.R"))
p_load(tidyverse, data.table, tidygraph, ggraph)

# ------------------------------------------------------------
# Load objects from script 02
# ------------------------------------------------------------
backbone_network <- read_rds(here("data", "backbone_network.rds"))
window_levels <- read_rds(here("data", "window_levels.rds"))
comm_levels <- read_rds(here("data_raw", "comm_levels.rds"))

# ------------------------------------------------------------
# Extract nodes as table
# ------------------------------------------------------------
nodes_pos <- backbone_network %>%
  activate("nodes") %>%
  as_tibble() %>%
  mutate(
    x = as.integer(factor(window, levels = window_levels))
  ) %>%
  group_by(window) %>%
  arrange(desc(community), .by_group = TRUE) %>%
  mutate(
    n_in_window = n(),
    y_raw = ifelse(n_in_window == 1, 0, seq(-1, 1, length.out = n_in_window))
  ) %>%
  ungroup()

# ------------------------------------------------------------
# Compute vertical stacking per community band
# ------------------------------------------------------------
comm_band <- nodes_pos %>%
  mutate(community = factor(community, levels = comm_levels)) %>%
  group_by(community, x) %>%
  summarise(n_in_cell = n(), .groups = "drop") %>%
  group_by(community) %>%
  summarise(
    max_in_column = max(n_in_cell, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(community) %>%
  mutate(
    max_in_column = pmax(max_in_column, 1L),
    pad = 0.7,
    band = max_in_column + pad
  ) %>%
  mutate(
    baseline = cumsum(lag(band, default = 0)) + band / 2
  ) %>%
  select(community, band, baseline)

# ------------------------------------------------------------
# Assign y positions inside community bands
# ------------------------------------------------------------
nodes_pos <- nodes_pos %>%
  mutate(community = factor(community, levels = comm_levels)) %>%
  left_join(comm_band, by = "community") %>%
  group_by(community, x) %>%
  arrange(window, cluster_id, .by_group = TRUE) %>%
  mutate(
    n_in_cell = n(),
    offset = (row_number() - 0.5) * band / n_in_cell - band / 2,
    y = baseline + offset
  ) %>%
  ungroup() %>%
  select(cluster_id, x, y, community)

# ------------------------------------------------------------
# Merge positions back into graph
# ------------------------------------------------------------
temporal_backbone_network <- backbone_network %>%
  activate("nodes") %>%
  select(-x, -y) %>%
  left_join(nodes_pos, by = c("cluster_id", "community"))

write_rds(
  temporal_backbone_network,
  here("data", "temporal_backbone_network.rds")
)

# ------------------------------------------------------------
# Compute community label positions
# ------------------------------------------------------------
community_label_positions <- comm_band %>%
  left_join(
    backbone_network %>%
      activate(nodes) %>%
      as_tibble() %>%
      distinct(community, label_backbone_community),
    by = "community"
  ) %>%
  arrange(match(community, comm_levels)) %>%
  mutate(label = stringr::str_wrap(label_backbone_community, 40))

write_rds(
  community_label_positions,
  here("data", "community_label_positions.rds")
)
