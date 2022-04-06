# Study Summary Module UI

#' @title study_summary_module_ui and study_summary_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param button_text button_text A string
#'
#' @rdname study_summary_module
#' @export
study_summary_module_ui <- function(id, button_text = "Download plot table"){
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(disable = T),
      shinydashboard::dashboardSidebar(disable = T),
      shinydashboard::dashboardBody(
        shiny::fluidPage(
          shinydashboard::box(
            shiny::textOutput(ns('header_text')),
            title = "Funding Partner",
            width = 12,
            solidHeader = T,
            status = "primary"
          ),
          study_selection_module_ui(ns("study_summary")),
          shinydashboard::box(
            shiny::htmlOutput(ns('study_summary')),
            title = "Study Summary",
            status = "primary",
            solidHeader = T,
            width = 12,
            collapsible = FALSE
          ),
          plot_module_ui(ns("study_timeline_plot"), "Study Timeline"),
          shinydashboard::box(
            shiny::downloadButton(ns("download_tbl"), button_text),
            plotly::plotlyOutput(ns('data_focus_plot')),
            title = "Data Focus",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE
          ),
          plot_module_ui(ns("annotation_activity_plot"), "Annotation Activity"),
          plot_module_ui(ns("publication_status_plot"), "Publication Status"),
          milestone_reporting_module_ui(ns("milestone_reporting_plot"))
        )
      )
    )
  )
}

# Study Summary Module Server

#' @title study_summary_module_server and study_summary_module_server_ui
#' @param data A named list. The list must contain a list named "tables".
#' @param config A named list. The list must contain lists for each section:
#' - header_text
#' - study_table
#' - study_summary
#' - data_focus_plot
#' - annotation_activity_plot
#' - publication_status_plot
#' @rdname study_summary_module
#' @export
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
study_summary_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$header_text <- shiny::renderText({
        shiny::req(config())
        glue::glue(config()$header_text)
      })

      filtered_data <- study_selection_module_server(
        "study_summary", data, config
      )

      output$study_summary <- shiny::renderText({

        shiny::req(filtered_data(), config())

        config <- purrr::pluck(config(), "study_summary")
        config$count_column$count <- F

        data <- filtered_data() %>%
          purrr::pluck("tables", config$table) %>%
          format_plot_data_with_config(config) %>%
          dplyr::distinct() %>%
          dplyr::mutate("Unique Study ID" = stringr::str_c(
            '<a href=',
            '"https://www.synapse.org/#!Synapse:',
            .data$`Unique Study ID`,
            '" target="_blank">',
            .data$`Unique Study ID`,
            '</a>'
          )) %>%
          tidyr::pivot_longer(dplyr::everything()) %>%
          dplyr::mutate(
            "name" = stringr::str_to_title(.data$name),
            "name" = stringr::str_c("<b>", .data$name, "</b>")
          ) %>%
          knitr::kable(
            "html", escape = FALSE, col.names = NULL, align = c('r', 'l')
          ) %>%
          kableExtra::kable_styling("striped", full_width = T)
      })

      plot_module_server(
        id = "study_timeline_plot",
        data = filtered_data,
        config = shiny::reactive(purrr::pluck(config(), "study_timeline_plot")),
        plot_func = shiny::reactive("create_study_timeline_plot")
      )

      data_focus_plot_tbl <- shiny::reactive({
        shiny::req(filtered_data(), config())

        config <- purrr::pluck(config(), "data_focus_plot")
        config$count_column$count <- F

        filtered_data() %>%
          purrr::pluck("tables", config$table) %>%
          format_plot_data_with_config(config)

      })

      output$data_focus_plot <- plotly::renderPlotly({
        shiny::req(data_focus_plot_tbl(), config())

        config <- purrr::pluck(config(), "data_focus_plot")

        data_list <- data_focus_plot_tbl() %>%
          create_data_focus_tables(config$plot$x, config$plot$fill)

        shiny::validate(shiny::need(length(data_list) > 0 , config$empty_table_message))

        create_data_focus_plots(data_list, config)
      })

      data_focus_summary_tbl <- shiny::reactive({
        shiny::req(data_focus_plot_tbl(), config())

        config <- purrr::pluck(config(), "data_focus_plot")

        data_focus_plot_tbl() %>%
          tidyr::pivot_longer(-config$plot$x, names_to = "Name", values_to = "Value") %>%
          dplyr::count(dplyr::across(dplyr::everything()), name = "Count") %>%
          dplyr::arrange(!!rlang::sym(config$plot$x), .data$Name, .data$Value)
      })

      output$download_tbl <- shiny::downloadHandler(
        filename = function() stringr::str_c("data-", Sys.Date(), ".csv"),
        content = function(con) readr::write_csv(data_focus_summary_tbl(), con)
      )

      plot_module_server(
        id = "annotation_activity_plot",
        data = filtered_data,
        config = shiny::reactive(
          purrr::pluck(config(), "annotation_activity_plot")
        ),
        plot_func = shiny::reactive("create_annotation_activity_plot")
      )

      plot_module_server(
        id = "publication_status_plot",
        data = filtered_data,
        config = shiny::reactive(
          purrr::pluck(config(), "publication_status_plot")
        ),
        plot_func = shiny::reactive("create_publication_status_plot")
      )

      milestone_reporting_module_server(
       id =  "milestone_reporting_plot",
       data = filtered_data,
       config = shiny::reactive(
         purrr::pluck(config(), "milestone_reporting_plot")
       )
      )

    }
  )
}
