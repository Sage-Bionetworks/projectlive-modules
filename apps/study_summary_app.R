devtools::load_all()

ui <- function(req) {
  shiny::tagList(
    shiny::navbarPage(
      title = shiny::strong("projectLive"),
      shiny::tabPanel(
        "study_summary",
        study_summary_module_ui("study_summary_module"),
        icon = shiny::icon("chart-area")
      ),
      collapsible = TRUE,	inverse = TRUE,
      windowTitle = "projectLive")
  )
}

server <- function(input, output, session) {

  study_summary_module_server(
    id = "study_summary_module",
    data = shiny::reactive(get_synthetic_data()),
    config = shiny::reactive(get_study_summary_config())
  )

}

shiny::shinyApp(ui, server)




