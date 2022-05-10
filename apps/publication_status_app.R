devtools::load_all()

ui <- function(req) {
  shiny::tagList(
    shiny::navbarPage(
      title = shiny::strong("projectLive"),
      shiny::tabPanel(
        "Pubs",
        publication_status_module_ui("publication_status_module"),
        icon = shiny::icon("chart-area")
      ),
      collapsible = TRUE,	inverse = TRUE,
      windowTitle = "projectLive")
  )
}

server <- function(input, output, session) {

  publication_status_module_server(
    id = "publication_status_module",
    data = shiny::reactive(get_synthetic_data()),
    config = shiny::reactive(get_publication_status_config())
  )

}

shiny::shinyApp(ui, server)




