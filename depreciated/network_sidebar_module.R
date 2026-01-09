networkSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("graph_selector")),
    shiny::sliderInput(ns("min_node_size"), "Min Node Size:", 0.1, 10, 1, step = 0.1),
    shiny::sliderInput(ns("max_node_size"), "Max Node Size:", 0.1, 10, 5, step = 0.1),
    shiny::sliderInput(ns("label_size"), "Label size", 0.5, 5, 2.5, step = 0.1)
  )
}

networkSidebarServer <- function(id, graph_tbl) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_list_graph <- is.list(graph_tbl) &&
      all(purrr::map_lgl(graph_tbl, ~ inherits(.x, "tbl_graph")))

    output$graph_selector <- shiny::renderUI({
      if (is_list_graph) {
        shiny::selectInput(
          ns("selected_graph"),
          "Select Time Window (8 years):",
          choices = names(graph_tbl),
          selected = names(graph_tbl)[1]
        )
      }
    })

    list(
      selected_graph = shiny::reactive(input$selected_graph),
      min_node_size = shiny::reactive(input$min_node_size),
      max_node_size = shiny::reactive(input$max_node_size),
      label_size = shiny::reactive(input$label_size)
    )
  })
}
