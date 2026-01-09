networkPlotUI <- function(id) {
  ns <- shiny::NS(id)
  ggiraph::girafeOutput(ns("network_plot"), width = "100%", height = "600px")
}

networkPlotServer <- function(
    id,
    graph_tbl,
    node_id,
    cluster_id,
    node_tooltip,
    node_size,
    cluster_tooltip,
    controls
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_list_graph <- is.list(graph_tbl) &&
      all(purrr::map_lgl(graph_tbl, ~ inherits(.x, "tbl_graph")))

    active_graph <- shiny::reactive({
      if (is_list_graph) {
        shiny::req(controls$selected_graph())
        graph_tbl[[controls$selected_graph()]]
      } else {
        graph_tbl
      }
    })

    output$network_plot <- ggiraph::renderGirafe({
      g_tbl <- active_graph() %>% tidygraph::activate("nodes")

      # Node size
      if (
        is.null(node_size) || !(node_size %in% colnames(as.data.frame(g_tbl)))
      ) {
        g_tbl <- dplyr::mutate(g_tbl, size = 1)
      } else {
        g_tbl <- dplyr::mutate(g_tbl, size = !!rlang::sym(node_size))
      }

      # ---- Fix cluster_id coercion
      cluster_sym <- rlang::sym(cluster_id)
      g_tbl <- g_tbl  %>%
        dplyr::mutate(!!cluster_sym := as.character(!!cluster_sym))

      nodes_df <- as.data.frame(tidygraph::activate(g_tbl, "nodes"))

      required_cols <- c(cluster_id, node_id, "color", "size", "x", "y")
      shiny::validate(shiny::need(
        all(required_cols %in% colnames(nodes_df)),
        paste("Missing:", paste(setdiff(required_cols, colnames(nodes_df)), collapse = ", "))
      ))

      color_sym <- rlang::sym("color")
      id_sym <- rlang::sym(node_id)
      tooltip_sym <- if (!is.null(node_tooltip)) rlang::sym(node_tooltip) else id_sym

      # Label positions per cluster
      label_data <- nodes_df %>%
        dplyr::group_by(!!cluster_sym) %>%
        dplyr::summarise(
          label_x = mean(x, na.rm = TRUE),
          label_y = mean(y, na.rm = TRUE),
          color = dplyr::first(!!color_sym),
          cluster_label = dplyr::first(!!cluster_sym),
          .groups = "drop"
        ) %>%
        dplyr::mutate(label_tooltip = cluster_tooltip)

      node_aes <- list(
        x = quote(x),
        y = quote(y),
        fill = color_sym,
        size = quote(size),
        data_id = cluster_sym,
        tooltip = tooltip_sym
      )

      label_aes <- list(
        x = quote(label_x),
        y = quote(label_y),
        label = quote(cluster_label),
        data_id = quote(cluster_label),
        fill = quote(color),
        tooltip = quote(label_tooltip)
      )

      g <- ggraph::ggraph(g_tbl, layout = "manual", x = x, y = y) +
        ggiraph::geom_point_interactive(
          mapping = do.call(ggplot2::aes, node_aes),
          shape = 21,
          alpha = 0.8,
          show.legend = FALSE
        ) +
        ggiraph::geom_label_repel_interactive(
          data = label_data,
          mapping = do.call(ggplot2::aes, label_aes),
          alpha = 0.9,
          fontface = "bold",
          show.legend = FALSE,
          size = controls$label_size()
        ) +
        ggplot2::scale_size_continuous(
          range = c(controls$min_node_size(), controls$max_node_size())
        ) +
        ggplot2::scale_fill_identity() +
        ggplot2::theme_void()

      ggiraph::girafe(
        ggobj = g,
        width_svg = 10,
        height_svg = 6,
        options = list(
          ggiraph::opts_selection(type = "single"),
          ggiraph::opts_zoom(min = 1, max = 12),
          ggiraph::opts_toolbar(position = "topright")
        )
      )
    })
  })
}
