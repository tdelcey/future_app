# modules/mod_network_view.R
mod_network_view_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "
      border: 2px solid #D4D4D4;
      border-radius: 10px;
      padding: 20px;
      background-color: #FDFDFD;
      height: 100%;
    ",

    # ---- Title ----
    div(
      style = "
        font-size: 20px;
        font-weight: 600;
        padding: 4px 0 4px 12px;
        border-left: 5px solid #00897B;
        margin-bottom: 15px;
      ",
      "Network of semantic clusters"
    ),

    # ---- Explanations using callout boxes ----
    callout_box(
      title = "Interpretation",
      icon = "\U0001F4DA",
      text = "Each node is an HDBSCAN cluster; colors indicate groups of clusters.
              Users can switch between two visualization modes: static and temporal.
              Static mode positions clusters using a force-directed layout based on semantic similarity 
              between representative vectors. Static visualization focuses on cluster relationships.
              Temporal mode orders clusters chronologically along the x-axis based on the window in which they emerge. 
              Temporal visualization focuses on the evolution of clusters over time.",
      border = "#00796B",
      bg = "#E0F2F1"
    ),

    # ---- Switch between static and temporal ----
    radioButtons(
      inputId = ns("mode"),
      label = "Visualization mode",
      choices = c("Static", "Temporal"),
      selected = "Static",
      inline = TRUE
    ),

    br(),

    # ---- Plot ----
    div(
      style = "
        border:1px solid #D0D0D0;
        border-radius:8px;
        padding:12px;
        background:#FFF;
      ",
      shinycssloaders::withSpinner(
        girafeOutput(ns("plot"), height = '800px'),
        type = 4,
        color = '#607D8B'
      )
    )
  )
}


# ======================================================================
# SERVER
# ======================================================================

mod_network_view_server <- function(
  id,
  backbone_network,
  temporal_backbone_network,
  cluster_colors,
  community_label_positions,
  window_levels
) {
  moduleServer(id, function(input, output, session) {
    # ------------------------------------------------------------------
    # Render network plot
    # ------------------------------------------------------------------
    output$plot <- ggiraph::renderGirafe({
      if (input$mode == "Static") {
        plot_network_interactive_static(
          graph = backbone_network,
          cluster_colors = cluster_colors
        )
      } else {
        plot_network_interactive_dynamic(
          temporal_backbone_network = temporal_backbone_network,
          cluster_colors = cluster_colors,
          community_label_positions = community_label_positions,
          window_levels = window_levels
        )
      }
    })

    # ------------------------------------------------------------------
    # Return selected node cluster_id to parent module
    # ------------------------------------------------------------------
    reactive(input$plot_selected)
  })
}
