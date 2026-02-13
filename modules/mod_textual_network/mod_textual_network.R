# modules/mod_textual_network.R

modules_textual_network_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(6, mod_network_view_ui(ns("net"))),
    column(6, mod_cluster_tables_ui(ns("tables")))
  )
}

modules_textual_network_server <- function(
  id,
  backbone_network,
  temporal_backbone_network,
  cluster_colors,
  community_label_positions,
  window_levels,
  tfidf_hdbscan,
  sentences_tbl,
  top_articles_by_count,
  top_articles_by_proximity,
  top_refs,
  top_refs_in_cluster
) {
  moduleServer(id, function(input, output, session) {
    selected_cluster <- mod_network_view_server(
      "net",
      backbone_network,
      temporal_backbone_network,
      cluster_colors,
      community_label_positions,
      window_levels
    )

    mod_cluster_tables_server(
      "tables",
      selected_cluster,
      tfidf_hdbscan,
      sentences_tbl,
      top_articles_by_count,
      top_articles_by_proximity,
      top_refs,
      top_refs_in_cluster
    )
  })
}
