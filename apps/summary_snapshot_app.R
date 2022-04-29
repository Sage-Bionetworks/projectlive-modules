devtools::load_all()

ui <- function(req) {
  shiny::tagList(
    shiny::navbarPage(
      title = shiny::strong("projectLive"),
      shiny::tabPanel(
        "Snapshot",
        summary_snapshot_module_ui("summary_snapshot_module"),
        icon = shiny::icon("chart-area")
      ),
      collapsible = TRUE,	inverse = TRUE,
      windowTitle = "projectLive")
  )
}

server <- function(input, output, session) {

  summary_snapshot_module_server(
    id = "summary_snapshot_module",
    data = shiny::reactive(get_synthetic_data()),
    config = shiny::reactive(get_summary_snapshot_config())
  )

}

shiny::shinyApp(ui, server)




