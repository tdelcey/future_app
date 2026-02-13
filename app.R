library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(ggiraph)
library(ggraph)
library(here)
library(tidyverse)
library(tidygraph)
library(ggrepel)
library(DT)
library(bs4Dash)

# load any helper functions and modules
list_files_helpers <- list.files(
  "utils",
  pattern = "*.R",
  full.names = TRUE,
  recursive = TRUE
)

list_files_modules <- list.files(
  "modules",
  pattern = "*.R",
  full.names = TRUE,
  recursive = TRUE
)
list_files <- c(list_files_helpers, list_files_modules)
invisible(lapply(list_files, source))


#' load data
#' ⚠️ `create_update_data()` must have been run once before launching the app ⚠️
#' The script creates/updates all the data needed for the app using a path to the ejhet_project folder.

#' `rsconnect::writeManifest(appDir = ".", appFiles = NULL)` can be used to create a manifest file for deployment on cloud.

# general ui

ui <- fluidPage(
  # Nouveau titre général
  div(
    style = "text-align:center; margin-bottom:30px; margin-top:10px;",

    div(
      style = "font-size:32px; font-weight:700; margin-bottom:6px;",
      "Thinking Tomorrow"
    ),
  ),
  modules_textual_network_ui("textnet")
)

server <- function(input, output, session) {
  # réseau
  backbone_network <- load_data("backbone_network")
  cluster_colors <- load_data("cluster_colors")
  temporal_backbone_network <- load_data("temporal_backbone_network")
  community_label_positions <- load_data("community_label_positions")
  window_levels <- load_data("window_levels")

  # tables pour les DT
  tfidf_hdbscan <- load_data("tfidf_tables")$top_terms_hdbscan_cluster
  sentences_tbl <- load_data("cluster_sentences")
  top_articles_by_count <- load_data("cluster_top_articles_by_count")
  top_articles_by_proximity <- load_data("cluster_top_articles_by_proximity")
  top_refs <- load_data("cluster_top_references")
  top_refs_in_cluster <- load_data("cluster_refs_in_cluster")
  modules_textual_network_server(
    id = "textnet",
    backbone_network = backbone_network,
    temporal_backbone_network = temporal_backbone_network,
    cluster_colors = cluster_colors,
    community_label_positions = community_label_positions,
    window_levels = window_levels,
    tfidf_hdbscan = tfidf_hdbscan,
    sentences_tbl = sentences_tbl,
    top_articles_by_count = top_articles_by_count,
    top_articles_by_proximity = top_articles_by_proximity,
    top_refs = top_refs,
    top_refs_in_cluster = top_refs_in_cluster
  )
}

shiny::shinyApp(ui = ui, server = server)
