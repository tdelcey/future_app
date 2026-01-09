# modules/mod_cluster_tables.R

mod_cluster_tables_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "
      border:2px solid #D4D4D4;
      border-radius:10px;
      padding:20px;
      background-color:#FAFAFA;
      height:100%;
    ",

    div(
      style = "
        font-size:20px;
        font-weight:600;
        padding-left:12px;
        border-left:5px solid #3F51B5;
        margin-bottom:15px;
      ",
      "Cluster details"
    ),

    p("Click a node in the network to explore its content."),

    tabsetPanel(
      id = ns("tabs"),

      # TF-IDF -------------------------------------------------------------
      tabPanel(
        "TF-IDF terms",
        callout_box(
          "Interpretation",
          "TF-IDF highlights words that are specific to a group of data in comparison to the entire dataset. 
          In this context, it emphhasiezs terms specific to the sentences within the selected cluster in comparison to all sentences across clusters."
        ),
        DTOutput(ns("tfidf"))
      ),

      # Sentences ----------------------------------------------------------
      tabPanel(
        "Sentences",
        callout_box(
          "Interpretation",
          "Sentences assigned to this cluster, ranked by semantic proximity. Two metrics of similarity are provided: 
            (1) similarity to the cluster centroid gives the cluster sentences most representative of the cluster as a whole;
            (2) similarity to the representative vector gives cluster sentences most representative of future discussion at this period.
            You can also filter to only show sentences mentioning 'future' or 'futures' (since not all sentences in the cluster may explicitly mention these terms)."
        ),
        prettyRadioButtons(
          inputId = ns("sentence_order_metric"),
          label = "Rank sentences by",
          choices = c(
            "Representative vector" = "rv",
            "Cluster centroid" = "centroid"
          ),
          selected = "rv",
          inline = TRUE,
          status = "primary"
        ),
        prettySwitch(
          inputId = ns("rational_only"),
          label = "Only sentences mentioning 'future' or 'futures'",
          value = FALSE,
          status = "primary",
          inline = TRUE
        ),
        DTOutput(ns("sentences"))
      ),

      # Top articles -------------------------------------------------------
      tabPanel(
        "Top articles",
        callout_box(
          "Interpretation",
          "Articles with the most sentences in this cluster."
        ),
        DTOutput(ns("articles"))
      ),

      # References ---------------------------------------------------------
      tabPanel(
        "References",
        callout_box(
          "Interpretation",
          "Most cited references by articles which have sentences in this cluster. The user can filter to only show references with a known Web of Science ID."
        ),
        prettySwitch(
          inputId = ns("articles_only"),
          label = "Only references with a known Web of Science ID",
          value = FALSE,
          status = "primary",
          inline = TRUE
        ),
        DTOutput(ns("refs"))
      )

    )
  )
}


mod_cluster_tables_server <- function(
  id,
  selected_cluster,
  tfidf_hdbscan,
  sentences_tbl,
  top_articles,
  top_refs
) {
  moduleServer(id, function(input, output, session) {
    # TF-IDF ---------------------------------------------------------------
    output$tfidf <- DT::renderDT({
      cid <- selected_cluster()
      if (is.null(cid)) {
        return(NULL)
      }
      df <- tfidf_hdbscan %>%
        filter(cluster_id == cid) %>%
        arrange(desc(tf_idf)) %>%
        select(token, tf_idf) %>%
        rename(term = token, tf_idf_score = tf_idf)

      datatable(df, rownames = FALSE, options = list(dom = "tip"))
    })

    # Sentences ------------------------------------------------------------
    output$sentences <- DT::renderDT({
      cid <- selected_cluster()
      if (is.null(cid)) {
        return(NULL)
      }

      df <- sentences_tbl %>%
        filter(cluster_id == cid) %>%
        select(
          sentence,
          title,
          year,
          journal,
          authors,
          similarity_rv,
          similarity_centroid
        ) %>%
        rename(
          similarity_to_rv = similarity_rv,
          similarity_to_centroid = similarity_centroid
        )

      if (isTRUE(input$rational_only)) {
        df <- df %>%
          filter(stringr::str_detect(sentence, regex("future|futures", TRUE)))
      }

      order_col <- if (identical(input$sentence_order_metric, "centroid")) {
        "similarity_to_centroid"
      } else {
        "similarity_to_rv"
      }

      df <- df %>%
        arrange(dplyr::desc(.data[[order_col]]))

      datatable(
        df,
        escape = FALSE,
        rownames = FALSE,
        options = list(dom = "tip")
      )
    })

    # Articles -------------------------------------------------------------
    output$articles <- DT::renderDT({
      cid <- selected_cluster()
      if (is.null(cid)) {
        return(NULL)
      }
      df <- top_articles %>%
        filter(cluster_id == cid) %>%
        select(title, journal, authors, year, n_sentences, mean_similarity) %>%
        arrange(desc(n_sentences))

      datatable(
        df,
        escape = FALSE,
        rownames = FALSE,
        options = list(dom = "tip")
      )
    })

    # References -----------------------------------------------------------
    output$refs <- DT::renderDT({
      cid <- selected_cluster()
      if (is.null(cid)) {
        return(NULL)
      }
      df <- top_refs %>%
        filter(cluster_id == cid) %>%
        select(name, year, journal_abbrev, has_id, absolute_cluster_cit) %>%
        rename(
          title = name,
          year = year,
          journal = journal_abbrev,
          n_citations = absolute_cluster_cit
        ) %>%
        arrange(desc(n_citations))

      if (isTRUE(input$articles_only)) {
        df <- df %>%
          filter(has_id)
      }

      df <- df %>%
        select(-has_id)

      datatable(df, rownames = FALSE, options = list(dom = "tip"))
    })

  })
}
