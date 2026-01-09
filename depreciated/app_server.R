# R/app_server.R
app_server <- function(input, output, session) {
  # Sidebar controls (graph selector, sizes, label size)
  controls <- networkSidebarServer(
    id = "net_sidebar",
    graph_tbl = graphs
  )

  # Plot module (interactive graph)
  networkPlotServer(
    id = "net_plot",
    graph_tbl = graphs,
    node_id = "ID_Art",
    cluster_id = "value_col",
    node_tooltip = "nodes_tooltip",
    node_size = "node_size",
    cluster_tooltip = "Click on cluster to see more information",
    controls = controls
  )

  # ---- read ggiraph selection with the proper namespace
  sel_id <- shiny::NS("net_plot")("network_plot_selected")
  selected_cluster <- shiny::reactive(input[[sel_id]])

  # output$debug_sel <- shiny::renderPrint(selected_cluster())  # <- optional debug

  clusterServer(
    id = "cluster",
    # all_nodes_flat = all_nodes_flat,
    closest_sentences = closest_sentences,
    top_refs = top_refs,
    # refs_wo_id_joined = refs_wo_id_joined,
    # tf_idf_joined = tf_idf_joined,
    cluster_origins = cluster_origins,
    cluster_destinies = cluster_destinies,
    cluster_information = c(
      "Titre",
      "Annee_Bibliographique",
      "Nom",
      "node_size",
      "participation_coefficient",
      "z_within",
      "role",
      "sentence"
    ),
    selected_graph = controls$selected_graph,
    selected_cluster = selected_cluster
  )

  # # ---- Cluster share table in the sidebar
  # output$cluster_share <- DT::renderDT(
  #   {
  #     shiny::req(controls$selected_graph())
  #     vc <- rlang::sym("value_col")
  #
  #     tab <- all_nodes_flat %>%
  #       dplyr::filter(time_window == controls$selected_graph()) %>%
  #       dplyr::count(!!vc, name = "n")  %>%
  #       dplyr::mutate(
  #         prop = n / sum(n),
  #         pct = sprintf("%.1f%%", 100 * prop)
  #       ) |>
  #       dplyr::arrange(dplyr::desc(prop)) %>%
  #       dplyr::rename(Cluster = !!vc) %>%
  #       dplyr::select(Cluster, n, pct)
  #
  #     DT::datatable(
  #       tab,
  #       options = list(dom = 't', paging = FALSE),
  #       rownames = FALSE
  #     )
  #   },
  #   server = TRUE
  # )
}
