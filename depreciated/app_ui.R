app_ui <- function(request) {
  shiny::fluidPage(
    shiny::navbarPage(
      "Rationality App",
      
      # --- Onglet 1 : bibliométrie
      shiny::tabPanel(
        "Bibliometric communities",
        networkSidebarUI("net_sidebar"),
        networkPlotUI("net_plot"),
        clusterUI("cluster")
      ),
      
      # --- Onglet 2 : clusters textuels
      shiny::tabPanel(
        "Textual clusters",
        shiny::div(
          style = "height: calc(100vh - 120px);",
          shiny::tags$iframe(
            src   = "textual_cluster.html",   
            style = "width:100%; height:100%; border:0;",
            loading = "lazy"
          )
        ),
        shiny::tags$p(
          shiny::tags$a(
            href   = "textual_cluster.html", 
            target = "_blank",
            "Open full screen"
          )
        )
      )
    )
  )
}