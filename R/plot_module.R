# Plot Module UI
#' @title plot_module_server and
#' plot_module_server_ui
#' @rdname plot_module
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param title A string, the displayed title of the shinydashboard::box()
#' @param button_text button_text A string
#'
#' @export
plot_module_ui <- function(id, title, button_text = "Download plot table"){
  ns <- shiny::NS(id)
  shinydashboard::box(
    title = title,
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    collapsible = FALSE,
    shiny::downloadButton(ns("download_tbl"), button_text),
    plotly::plotlyOutput(ns("plot"))
  )
}

# Plot Module Server

#' @title plot_module_server and plot_module_server_ui
#' @param id shiny id
#' @param data A shiny::reactive() that returns a named list. The list must
#' contain a list named "tables".
#' @param config A shiny::reactive() that returns a named list.
#' @param plot_func A shiny::reactive() that returns a string that is the name of
#' a plotting function.
#' @param ... Arguments to plotly::ggplotly
#'
#' @rdname plot_module
#' @export
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
plot_module_server <- function(id, data, config, plot_func, ...){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      plot_data <- shiny::reactive({
        shiny::req(data(), config())

        data <- data() %>%
          purrr::pluck("tables", config()$table) %>%
          format_plot_data_with_config(config())

        shiny::validate(shiny::need(
          nrow(data) > 0,
          config()$empty_table_message
        ))

        count_column <- config()$count_column$name

        if(!is.null(count_column)){
          shiny::validate(shiny::need(
            sum(data[count_column]) > 0,
            config()$empty_table_message
          ))
        }

        return(data)
      })

      plot_obj <- shiny::reactive({
        shiny::req(config(), plot_data())
        create_plot_with_config(
          data = plot_data(),
          config = config(),
          plot_func = plot_func(),
          ...
        )
      })

      output$plot <- plotly::renderPlotly({
        shiny::req(plot_obj())
        plot_obj()
      })

      summarized_plot_data <- shiny::reactive({
        shiny::req(plot_data())
        if("Count" %in% colnames(plot_data())) data <- plot_data()
        else {
          data <- dplyr::count(
            plot_data(),
            dplyr::across(dplyr::everything()), name = "Count"
          )
        }
        return(data)
      })

      output$download_tbl <- shiny::downloadHandler(
        filename = function() stringr::str_c("data-", Sys.Date(), ".csv"),
        content = function(con) readr::write_csv(summarized_plot_data(), con)
      )
    }
  )
}
