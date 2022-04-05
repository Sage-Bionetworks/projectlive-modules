#' Data Flow Module UI
#'
#' @param id shiny id
#' @export
data_flow_module_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(disable = T),
      shinydashboard::dashboardSidebar(disable = T),
      shinydashboard::dashboardBody(
        shiny::fluidPage(
          shinydashboard::box(
            DT::dataTableOutput(ns("table")),
            title = "Data Flow",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE
          ),
          plot_module_ui(
            ns("plot"),
            "Data Flow"
          )
        )
      )
    )
  )
}

#' Data Flow Module Server
#'
#' @param id shiny id
#' @param data A named list. It must contain a list named "tables".
#' @param config A named list.
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data

data_flow_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      table_data <- shiny::reactive({
        shiny::req(data(), config())

        config <- config()$data_table

        data <- data() %>%
          purrr::pluck("tables", config$table) %>%
          format_plot_data_with_config(config)
      })

      output$table <- DT::renderDataTable(
        base::as.data.frame(table_data()),
        server = TRUE,
        selection = 'single'
      )

      plot_module_server(
        id = "plot",
        data = data,
        config = shiny::reactive(purrr::pluck(config(), "plot")),
        plot_func = shiny::reactive("create_initiative_activity_plot")
      )
    }
  )
}


