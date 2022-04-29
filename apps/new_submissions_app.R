devtools::load_all()

ui <- function(req) {
  shiny::tagList(
    shiny::navbarPage(
      title = shiny::strong("projectLive"),
      shiny::tabPanel(
        "new_submissions",
        new_submissions_module_ui("new_submissions"),
        icon = shiny::icon("chart-area")
      ),
      collapsible = TRUE,	inverse = TRUE,
      windowTitle = "projectLive")
  )
}

server <- function(input, output, session) {

  new_submissions_module_server(
    id = "new_submissions",
    data = shiny::reactive(get_synthetic_data()),
    config = shiny::reactive(get_new_submissions_config())
  )

}

shiny::shinyApp(ui, server)




