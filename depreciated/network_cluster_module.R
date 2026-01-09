clusterUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .section-card{
          background:#fff; border:1px solid #e9ecef; border-radius:12px;
          padding:12px 14px; margin-bottom:16px; box-shadow:0 1px 2px rgba(0,0,0,.03);
        }
        .section-card h4{ margin-top:0; margin-bottom:8px; }
        .muted{ color:#6c757d; margin-top:-2px; margin-bottom:10px; }
        .divider{ border-top:2px solid #e9ecef; margin:14px 0; }
        .big-divider{ border-top:3px dashed #d0d7de; margin:18px 0; }
        table.dataTable.compact thead th, table.dataTable.compact tbody td { padding: 6px 8px; }
        .dt-wrapper{ overflow-x:auto; }
      "))
    ),
    
    shiny::fluidRow(
      # --- Left column: Origins (top) + Sentences (bottom)
      shiny::column(
        width = 6,
        shiny::div(class = "section-card",
                   shiny::h4("Cluster origins (t−1 → t)"),
                   shiny::p(class = "muted", "Clusters in which the current documents were located in the previous time window"),
                   shiny::div(class = "dt-wrapper", DT::DTOutput(ns("cluster_origins_table")))
        ),
        shiny::div(class = "divider"),
        shiny::div(class = "section-card",
                   shiny::h4("Closest sentences"),
                   shiny::p(class = "muted", "Sentences most semantically similar to the selected cluster."),
                   shiny::div(class = "dt-wrapper", DT::DTOutput(ns("cluster_sentences")))
        )
      ),
      
      # --- Right column: Destinies (top) + References (bottom)
      shiny::column(
        width = 6,
        shiny::div(class = "section-card",
                   shiny::h4("Cluster destinies (t → t+1)"),
                   shiny::p(class = "muted", "Clusters into which the current documents move in the next time window"),
                   shiny::div(class = "dt-wrapper", DT::DTOutput(ns("cluster_destinies_table")))
        ),
        shiny::div(class = "divider"),
        shiny::div(class = "section-card",
                   shiny::h4("Top references"),
                   shiny::p(class = "muted", "Most frequently cited references within this cluster."),
                   shiny::div(class = "dt-wrapper", DT::DTOutput(ns("cluster_refs")))
        )
      )
    )
  )
}



clusterServer <- function(
    id,
    closest_sentences,
    top_refs,
    cluster_origins,
    cluster_destinies,
    cluster_information,
    selected_graph,   # reactive
    selected_cluster  # reactive
) {
  shiny::moduleServer(id, function(input, output, session) {
    
    coerce_value_col <- function(df) {
      df %>%
        dplyr::mutate(
          value_col = as.character(.data$value_col),
          time_window = substr(.data$time_window, 1, 4)
        )
    }
    
    # --- Helper : datatable vide (garde les colonnes visibles)
    empty_dt <- function(cols, msg = "Sélectionnez un nœud du réseau pour afficher les données.") {
      # Crée un data.frame vide avec les colonnes attendues
      df <- as.data.frame(
        stats::setNames(replicate(length(cols), character(0), simplify = FALSE), cols)
      )
      
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          dom = "t",  # seulement le tableau (pas de search bar, pas de pagination)
          language = list(
            emptyTable = msg,
            zeroRecords = msg
          ),
          autoWidth = TRUE,
          scrollX = TRUE,
          info = FALSE
        )
      )
    }
    
    # --- Helper : rend tab OU placeholder (structure des colonnes constante)
    render_or_placeholder <- function(tab, cols, page_len = 10, searchable = TRUE) {
      if (!is.null(tab) && nrow(tab) > 0) {
        # ajoute les colonnes manquantes comme NA
        missing_cols <- setdiff(cols, names(tab))
        if (length(missing_cols) > 0) {
          for (mc in missing_cols) {
            tab[[mc]] <- NA_character_
          }
        }
        tab <- tab %>% dplyr::select(dplyr::any_of(cols))
        
        DT::datatable(
          tab,
          class = "compact stripe hover",
          rownames = FALSE,
          escape = FALSE,
          options = list(
            pageLength = page_len,
            lengthChange = FALSE,
            searching = searchable,
            autoWidth = TRUE,
            scrollX = TRUE,
            info = FALSE,
            columnDefs = list(list(width = 'auto', targets = "_all"))
          )
        )
      } else {
        empty_dt(cols)
      }
    }
    
    # --- Sentences
    output$cluster_sentences <- DT::renderDT({
      if (is.null(selected_cluster()) || is.null(selected_graph())) {
        cols <- unique(c(cluster_information, "sentence", "similarity"))
        return(empty_dt(cols))
      }
      sc <- as.character(selected_cluster())
      tab <- closest_sentences %>%
        coerce_value_col() %>%
        dplyr::filter(.data$value_col == sc, .data$time_window == selected_graph()) %>%
        dplyr::select(dplyr::any_of(c(cluster_information, "sentence", "similarity")))
      render_or_placeholder(
        tab,
        cols = unique(c(cluster_information, "sentence", "similarity")),
        page_len = 5,
        searchable = TRUE
      )
    }, server = TRUE)
    
    # --- References
    output$cluster_refs <- DT::renderDT({
      if (is.null(selected_cluster()) || is.null(selected_graph())) {
        return(empty_dt(c("Nom", "Annee", "Revue_Abbrege", "nb_cit")))
      }
      sc <- as.character(selected_cluster())
      tab <- top_refs %>%
        coerce_value_col() %>%
        dplyr::filter(.data$value_col == sc, .data$time_window == selected_graph()) %>%
        dplyr::select(.data$Nom, .data$Annee, .data$Revue_Abbrege, .data$nb_cit)
      render_or_placeholder(tab, cols = c("Nom", "Annee", "Revue_Abbrege", "nb_cit"), page_len = 20)
    }, server = TRUE)
    
    # --- Origins
    output$cluster_origins_table <- DT::renderDT({
      if (is.null(selected_cluster()) || is.null(selected_graph())) {
        return(empty_dt(c("previous_cluster", "origin_percent")))
      }
      sc <- as.character(selected_cluster())
      tab <- cluster_origins %>%
        coerce_value_col() %>%
        dplyr::filter(.data$value_col == sc, .data$time_window == selected_graph()) %>%
        dplyr::select(.data$previous_cluster, .data$origin_percent) %>%
        dplyr::mutate(origin_percent = sprintf("%.1f%%", 100 * .data$origin_percent))
      render_or_placeholder(tab, cols = c("previous_cluster", "origin_percent"), page_len = 10, searchable = FALSE)
    }, server = TRUE)
    
    # --- Destinies
    output$cluster_destinies_table <- DT::renderDT({
      if (is.null(selected_cluster()) || is.null(selected_graph())) {
        return(empty_dt(c("forward_cluster", "destiny_percent")))
      }
      sc <- as.character(selected_cluster())
      tab <- cluster_destinies %>%
        coerce_value_col() %>%
        dplyr::filter(.data$value_col == sc, .data$time_window == selected_graph()) %>%
        dplyr::select(.data$forward_cluster, .data$destiny_percent) %>%
        dplyr::mutate(destiny_percent = sprintf("%.1f%%", 100 * .data$destiny_percent))
      render_or_placeholder(tab, cols = c("forward_cluster", "destiny_percent"), page_len = 10, searchable = FALSE)
    }, server = TRUE)
  })
}
