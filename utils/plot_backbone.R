plot_network_interactive_static <- function(graph, cluster_colors) {
  # 1. Appliquer la couleur aux NOEUDS à partir de cluster_colors
  graph <- graph %>%
    tidygraph::activate("nodes") %>%
    dplyr::mutate(
      fill_color = cluster_colors[as.character(backbone_community)],
      tooltip = paste0(
        "HDBSCAN_cluster: ",
        HDBSCAN_cluster,
        "\n",
        "Label: ",
        label_hdbscan_cluster,
        "\n",
        "Proportion: ",
        proportion_hdbscan_cluster
      )
    )

  # 2. Couleurs des ARÊTES = couleur du nœud "from"
  graph <- graph %>%
    tidygraph::activate("edges") %>%
    dplyr::mutate(
      comm_from = .N()[["backbone_community"]][from],
      comm_to = .N()[["backbone_community"]][to],
      same_comm = comm_from == comm_to,

      # couleur de l'arête = couleur du node from si même communauté
      edge_color = ifelse(
        same_comm,
        .N()[["fill_color"]][from],
        "#CCCCCC" # gris clair pour inter-communauté
      ),
      edge_width = ifelse(
        same_comm,
        weight, # keep original width for intra-community
        weight * 0.1 # shrink inter-community edges
      )
    )

  # 3. Dataframe pour les noeuds
  nodes_df <- graph %>%
    tidygraph::activate("nodes") %>%
    dplyr::as_tibble()

  # 4. Dataframe pour les labels statiques
  label_df <- nodes_df %>%
    dplyr::group_by(backbone_community) %>%
    dplyr::summarise(
      label_x = mean(x),
      label_y = mean(y),
      label = first(backbone_community),
      fill_color = first(fill_color),
      .groups = "drop"
    )

  # 5. Plot ggraph
  p <- ggraph::ggraph(graph, layout = "manual", x = x, y = y) +

    # --- ARÊTES ---
    # Couleur hex directe : ggplot ne remappe plus rien
    ggraph::geom_edge_link0(
      ggplot2::aes(
        color = I(edge_color),
        width = edge_width
      ),
      alpha = 0.5,
      show.legend = FALSE
    ) +

    # --- NOEUDS ---
    # On colore par fill_color (hex direct)
    ggiraph::geom_point_interactive(
      data = nodes_df,
      ggplot2::aes(
        x = x,
        y = y,
        fill = I(fill_color),
        data_id = cluster_id,
        tooltip = tooltip,
        size = proportion_hdbscan_cluster
      ),
      shape = 21,
      alpha = 0.9,
      show.legend = FALSE,
    ) +

    # --- LABELS ---
    ggrepel::geom_label_repel(
      data = label_df,
      ggplot2::aes(
        x = label_x,
        y = label_y,
        label = label,
        fill = I(fill_color)
      ),
      color = "black",
      size = 4,
      fontface = "bold",
      label.size = 0.3,
      label.r = unit(0.15, "lines"),
      alpha = 0.95,
      seed = 42
    ) +

    # --- SCALING ---
    scale_size_continuous(range = c(2, 15)) +
    scale_edge_width(range = c(0.3, 1.2)) +

    ggplot2::theme_void()

  # 6. Girafe interactive
  ggiraph::girafe(
    ggobj = p,
    width_svg = 16,
    height_svg = 10,
    options = list(
      ggiraph::opts_selection(type = "single"),
      ggiraph::opts_zoom(min = 1, max = 12),
      ggiraph::opts_toolbar(position = "topright")
    )
  )
}


plot_network_interactive_dynamic <- function(
  temporal_backbone_network,
  cluster_colors,
  community_label_positions,
  window_levels
) {
  # 1. Add fill_color + tooltip to nodes (same as static)
  graph <- temporal_backbone_network %>%
    tidygraph::activate("nodes") %>%
    mutate(
      fill_color = cluster_colors[as.character(backbone_community)],
      tooltip = paste0(
        "HDBSCAN_cluster: ",
        HDBSCAN_cluster,
        "\n",
        "Label: ",
        label_hdbscan_cluster,
        "\n",
        "Period: ",
        window,
        "\n",
        "Proportion: ",
        round(proportion_hdbscan_cluster * 100, 2),
        "%"
      )
    )

  # 2. Compute edge colors = color of node 'from' (same logic as static)
  graph <- graph %>%
    tidygraph::activate("edges") %>%
    mutate(
      comm_from = .N()[["backbone_community"]][from],
      comm_to = .N()[["backbone_community"]][to],
      same_comm = comm_from == comm_to,

      edge_color = ifelse(
        same_comm,
        .N()[["fill_color"]][from], # color of source node
        "#CCCCCC" # grey for inter-community edges
      ),

      edge_width = ifelse(
        same_comm,
        1.2, # width intra-community
        0.3 # thin inter-community
      )
    )

  nodes_df <- graph %>%
    tidygraph::activate("nodes") %>%
    as_tibble()

  # 3. Plot
  p <- ggraph(
    graph,
    layout = "manual",
    x = x,
    y = y
  ) +
    # --- EDGES ---
    geom_edge_link(
      aes(
        color = I(edge_color),
        width = edge_width
      ),
      alpha = 0.45,
      show.legend = FALSE
    ) +

    # --- NODES ---
    ggiraph::geom_point_interactive(
      data = nodes_df,
      aes(
        x = x,
        y = y,
        fill = I(fill_color),
        size = proportion_hdbscan_cluster,
        tooltip = tooltip,
        data_id = cluster_id
      ),
      shape = 21,
      alpha = 0.9,
      show.legend = FALSE
    ) +

    # --- SCALES ---
    scale_size_continuous(range = c(2, 15)) +
    scale_edge_width(range = c(0.3, 1.2)) +
    scale_x_continuous(
      breaks = seq_along(window_levels),
      labels = window_levels
    ) +

    scale_y_continuous(
      breaks = community_label_positions$baseline,
      labels = community_label_positions$label,
      expand = expansion(add = c(0.9, 0.9))
    ) +

    labs(x = NULL, y = NULL) +

    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 10, hjust = 1),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 9
      ),
      axis.ticks = element_line(color = "grey50"),
      axis.line = element_line(color = "grey50")
    )

  # 4. Return girafe widget
  ggiraph::girafe(
    ggobj = p,
    width_svg = 16,
    height_svg = 10,
    options = list(
      ggiraph::opts_selection(type = "single"),
      ggiraph::opts_zoom(min = 1, max = 12),
      ggiraph::opts_tooltip(use_fill = TRUE),
      ggiraph::opts_toolbar(position = "topright")
    )
  )
}
