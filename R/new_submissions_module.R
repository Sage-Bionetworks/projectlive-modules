#' New Submissions Module UI
#'
#' @param id shiny id
#' @export
new_submissions_module_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(disable = T),
      shinydashboard::dashboardSidebar(disable = T),
      shinydashboard::dashboardBody(
        shiny::fluidPage(
          shinydashboard::box(
            title = "Funding Partner",
            width = 12,
            solidHeader = T,
            status = "primary",
            shiny::textOutput(ns('header_text')),
          ),
          shinydashboard::box(
            title = "New Files",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            shiny::numericInput(
              ns("new_files_day_choice"),
              "Display files uploaded within the last N days:",
              60,
              min = 1,
              step = 1
            ),
            DT::dataTableOutput(ns('data_table'))
          ),
          plot_module_ui(ns("plot"), "New Submissions Plot"),
        )
      )
    )
  )
}

#' New Submissions Module Server
#'
#' @param id shiny id
#' @param data A named list. It must contain a list named "tables".
#' @param config A named list.
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
new_submissions_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$header_text <- shiny::renderText({
        shiny::req(config())
        glue::glue(config()$header_text)
      })

      minimum_date <- shiny::reactive({
        shiny::req(input$new_files_day_choice)
        lubridate::now() - lubridate::ddays(input$new_files_day_choice)
      })

      filtered_data <- shiny::reactive({

        shiny::req(data(), config(), minimum_date())

        config <- purrr::pluck(
          config(),
          "new_files_table"
        )

        filtered_data <- data() %>%
          purrr::pluck("tables", config$table) %>%
          dplyr::filter(!!rlang::sym(config$date_column) > minimum_date()) %>%
          dplyr::arrange(dplyr::desc(!!rlang::sym(config$date_column))) %>%
          list() %>%
          purrr::set_names(config$table) %>%
          list() %>%
          purrr::set_names("tables")

        filtered_data$minimum_date <- minimum_date()
        print(filtered_data)
        return(filtered_data)
      })

      data_table <- shiny::reactive({
        shiny::req(filtered_data(), config())

        config <- purrr::pluck(
          config(),
          "data_table"
        )

        config$count_column$count <- F

        filtered_data() %>%
          purrr::pluck("tables") %>%
          purrr::pluck(config$table) %>%
          format_plot_data_with_config(config)
      })

      output$data_table <- DT::renderDataTable(
        data_table(),
        server = TRUE,
        selection = 'single'
      )

      plot_module_server(
        id = "plot",
        data = filtered_data,
        config = shiny::reactive(
          purrr::pluck(config(), "plot")
        ),
        plot_func = shiny::reactive("create_new_submissions_plot")
      )

    }
  )
}
