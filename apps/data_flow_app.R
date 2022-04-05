devtools::load_all()

ui <- function(req) {
  shiny::tagList(
    shiny::navbarPage(
      title = shiny::strong("projectLive"), selected = "Data Flow",
      shiny::tabPanel(
        "Data Flow",
        data_flow_module_ui("data_flow_module"),
        icon = shiny::icon("chart-area")
      ),
      collapsible = TRUE,	inverse = TRUE,
      windowTitle = "projectLive")
  )
}

server <- function(input, output, session) {

  synapseclient <- reticulate::import("synapseclient", delay_load = TRUE)
  syn <- synapseclient$Synapse()
  syn$login()

  data <- shiny::reactive(
    list(
      "tables" = list(
        "files" = tibble::tibble(
          "name" = stringr::str_c("file_name_", 1:10),
          "month" = c(
            rep(lubridate::month(1, label = T), 3),
            rep(lubridate::month(2, label = T), 7)
          ),
          "status" = c(
            rep("released", 3),
            rep("quarantined", 3),
            rep("restricted", 4)
          ),
          "type" = c(
            "fastq",
            "fastq",
            "vcf",
            "fastq",
            "fastq",
            "image",
            "image",
            "image",
            "image",
            "image"
          )
        )
      )
    )
  )

  config <- shiny::reactive(
    jsonlite::read_json(get_json_path("mock_data_flow_module"))
  )

  data_flow_module_server(
    id = "data_flow_module",
    data = data,
    config = config
  )


}

shiny::shinyApp(ui, server)




