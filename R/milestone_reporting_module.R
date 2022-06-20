# Milestone Reporting Module UI

#' @title milestone_reporting_module_ui and milestone_reporting_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param button_text button_text A string
#'
#' @rdname milestone_reporting_module
#' @export
milestone_reporting_module_ui <- function(id, button_text = "Download plot table"){
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::box(
      title = "Milestone or Progress Report tracker",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,
      shiny::h4(stringr::str_c(
        "The following plots track the expected and actual data uploads to this",
        "study associated with each progress report or milestone report.",
        sep = " "
      )),
      shiny::p("Select the visualization type for the plots from the list below"),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::uiOutput(ns("join_column_choice_ui"))
        )
      ),
      # ----
      shinydashboard::box(
        title = "Researcher reported progress/milestone update",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        shiny::p(stringr::str_c(
          "Select a milestone or progress report number from the list below.",
          "The plots will show the expected data files for this milestone or progress report,",
          "and the uploaded data files that the researcher reported for this milestone or progress report.",
          sep = " "
        )),
        shiny::fluidRow(
          shiny::column(
            width = 2,
            shiny::uiOutput(ns("milestone_choice_ui"))
          )
        ),
        shiny::textOutput(ns("plot_title2")),
        shiny::downloadButton(ns("download_tbl2"), button_text),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            plotly::plotlyOutput(ns("plot2"))
          )
        ),
        shiny::uiOutput(ns("link_button2"))
      ),
      # ----
      shinydashboard::box(
        title = "Sage Internal milestone or progress report tracking",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::p(stringr::str_c(
              "Click on a row in the table below to select the milestone or progress report number of interest.",
              "Then use the slider on the right to determine a time window around the",
              "estimated date of upload to find all files uploaded during this window",
              sep = " "
            ))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            DT::dataTableOutput(ns("dt"))
          ),
          shiny::column(
            width = 6,
            shiny::sliderInput(
              inputId = ns("days_choice"),
              label = "Select Amount of Days",
              step = 1L,
              min = 30L,
              max = 365L,
              value = 60L
            )
          )
        ),
        shiny::textOutput(ns("plot_title1")),
        shiny::downloadButton(ns("download_tbl1"), button_text),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            plotly::plotlyOutput(ns("plot1"))
          )
        ),
        shiny::uiOutput(ns("link_button1"))
      )
    )
  )
}

# Milestone Reporting Module Server

#' @title milestone_reporting_module_server and milestone_reporting_module_server_ui
#' @param data A named list. The list must contain a list named "tables".
#' @param config A named list.
#' @rdname milestone_reporting_module
#' @export
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
milestone_reporting_module_server <- function(id, data, config, syn, study_id){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      join_column_choices <- shiny::reactive({
        shiny::req(config())
        choices <- unlist(config()$join_columns)
      })

      output$join_column_choice_ui <- shiny::renderUI({
        shiny::req(join_column_choices())
        shiny::selectInput(
          inputId = ns("join_column_choice"),
          label = "Choose paramater to visualize.",
          choices = join_column_choices()
        )
      })

      files_tbl <- shiny::reactive({
        shiny::req(data(), config())
        shiny::validate(shiny::need(
          !is.null(config()),
          "Not tracking milestones or progress reports"
        ))

        config <- purrr::pluck(config(), "files_table")
        config$count_column$count <- F

        tbl <- purrr::pluck(data(), "tables", config$name)
        format_plot_data_with_config(tbl, config)
      })

      id_tbl <- shiny::reactive({
        shiny::req(data(), config())
        shiny::validate(shiny::need(
          !is.null(config()),
          "Not tracking milestones or progress reports"
        ))

        config <- purrr::pluck(config(), "incoming_data_table")
        config$count_column$count <- F

        tbl <- purrr::pluck(data(), "tables", config$name)
        shiny::validate(shiny::need(
          nrow(tbl) > 0,
          "Study has no current milestones or progress report updates."
        ))
        format_plot_data_with_config(tbl, config)
      })

      fileview_id <- shiny::reactive({
        shiny::req(study_id(), syn)
        get_filview_id_from_study(study_id(), syn)
      })

      # plot1 ----

      dt_tbl <- shiny::reactive({
        shiny::req(id_tbl(), config())
        config <- config()
        create_internal_tracking_datatable(id_tbl(), config())
      })

      output$dt <- DT::renderDataTable(
        base::as.data.frame(dt_tbl()),
        server = TRUE,
        selection = list(mode = 'single', selected = 1),
        rownames = FALSE
      )

      dt_row <- shiny::reactive({
        shiny::req(dt_tbl(), input$dt_rows_selected)
        dplyr::slice(dt_tbl(), input$dt_rows_selected)
      })

      selected_milestone <- shiny::reactive({
        shiny::req(dt_row())
        milestone_column <- rlang::sym(config()$milestone_column)
        milestone <- dplyr::pull(dt_row(), !!milestone_column)
      })

      date_range_start <- shiny::reactive({
        shiny::req(input$days_choice, dt_row(), config())
        date_column <- rlang::sym(config()$date_estimate_column)
        date <- dplyr::pull(dt_row(), !!date_column)
        date - lubridate::duration(input$days_choice, 'days')
      })

      date_range_end <- shiny::reactive({
        shiny::req(input$days_choice, dt_row(), config())
        date_column <- rlang::sym(config()$date_estimate_column)
        date <- dplyr::pull(dt_row(), !!date_column)
        date + lubridate::duration(input$days_choice, 'days')
      })

      plot_title1 <- shiny::reactive({
        shiny::req(date_range_start(), date_range_end())
        stringr::str_c(
          "The plot below is showing all files expected or uploaded between the time window of ",
          as.character(date_range_start()),
          ", and ",
          as.character(date_range_end()),
          "."
        )
      })

      output$plot_title1 <- shiny::renderText({
        shiny::req(plot_title1())
        plot_title1()
      })

      filtered_files_tbl1 <- shiny::reactive({
        shiny::req(
          files_tbl(),
          config(),
          date_range_start(),
          date_range_end()
        )
        filter_files_tbl1(
          files_tbl(),
          config(),
          date_range_start(),
          date_range_end()
        )
      })

      grouped_files_tbl1 <- shiny::reactive({
        shiny::req(
          filtered_files_tbl1(),
          config(),
          input$join_column_choice
        )
        group_files_tbl(
          filtered_files_tbl1(),
          config(),
          input$join_column_choice
        )
      })

      filtered_id_tbl1 <- shiny::reactive({
        shiny::req(
          id_tbl(),
          config(),
          selected_milestone(),
          input$join_column_choice
        )
        filter_internal_data_tbl(
          id_tbl(),
          config(),
          selected_milestone(),
          input$join_column_choice
        )
      })

      merged_tbl1 <- shiny::reactive({
        shiny::req(
          filtered_id_tbl1(),
          grouped_files_tbl1(),
          config(),
          input$join_column_choice
        )
        merge_tbls(
          filtered_id_tbl1(),
          grouped_files_tbl1(),
          config(),
          input$join_column_choice
        )
      })

      plot_obj1 <- shiny::reactive({
        shiny::req(
          merged_tbl1(),
          input$join_column_choice
        )
        create_milestone_reporting_plot(
          merged_tbl1(),
          input$join_column_choice
        )
      })

      output$plot1 <- plotly::renderPlotly({
        shiny::req(plot_obj1())

        plot_obj1() %>%
          plotly::ggplotly(
            source = "milestone_plot1",
            tooltip = c("Number of Files")
          ) %>%
          plotly::layout(autosize = T)
      })

      event_data1 <- shiny::reactive({
        shiny::req(plot_obj1())
        eventdata <- plotly::event_data(event = "plotly_click", source = "milestone_plot1")
        if(is.null(eventdata) && !is.null(input$mock_event_data1)){
          eventdata <- input$mock_event_data1
        }
        return(eventdata)
      })

      link1 <- shiny::reactive({
        shiny::req(stringr::str_detect(fileview_id(), "^syn[0-9]+$"))
        shiny::validate(shiny::need(
          all(!is.null(event_data1()), event_data1() != ""),
          'Click on a bar above in the "ACTUAL" column to generate a link to the files in Synapse.'
        ))
        link <- create_fileview_link(fileview_id(), event_data1()$key[[1]])
      })

      output$link_button1 <- shiny::renderUI({
        shiny::actionButton(
          inputId = ns('synapse_link1'),
          label = "See selected files in Synapse.",
          icon = shiny::icon("map-marker-alt"),
          lib = "font-awesome",
          class = "btn btn-primary btn-lg btn-block",
          onclick = stringr::str_c(
            "window.open('", link1(),  "','_blank')"
          )
        )
      })


      output$download_tbl1 <- shiny::downloadHandler(
        filename = function() stringr::str_c("data-", Sys.Date(), ".csv"),
        content = function(con) readr::write_csv(merged_tbl1(), con)
      )

      # plot2 ----

      milestone_choices <- shiny::reactive({

        shiny::req(files_tbl(), id_tbl(), config())
        config <- config()
        milestone_column   <- rlang::sym(config$milestone_column)

        milestones1 <- id_tbl() %>%
          dplyr::pull(!!milestone_column) %>%
          unique() %>%
          sort()

        milestones2 <- files_tbl() %>%
          dplyr::pull(!!milestone_column) %>%
          unique() %>%
          sort()

        base::union(milestones1, milestones2)
      })

      output$milestone_choice_ui <- shiny::renderUI({
        shiny::req(milestone_choices())
        shiny::selectInput(
          inputId = ns("milestone_choice"),
          label = "Choose Milestone or Progress Report Number",
          choices = milestone_choices()
        )
      })

      plot_title2 <- shiny::reactive({
        shiny::req(input$milestone_choice)
        stringr::str_c(
          "The plot below is showing all files expected or annotated with milestone or progress report number ",
          input$milestone_choice,
          "."
        )
      })

      output$plot_title2 <- shiny::renderText({
        shiny::req(plot_title2())
        plot_title2()
      })

      filtered_files_tbl2 <- shiny::reactive({
        shiny::req(
          files_tbl(),
          config(),
          input$milestone_choice
        )
        filter_files_tbl2(
          files_tbl(),
          config(),
          input$milestone_choice
        )
      })

      grouped_files_tbl2 <- shiny::reactive({
        shiny::req(
          filtered_files_tbl2(),
          config(),
          input$join_column_choice
        )
        group_files_tbl(
          filtered_files_tbl2(),
          config(),
          input$join_column_choice
        )
      })

      filtered_id_tbl2 <- shiny::reactive({
        shiny::req(
          id_tbl(),
          config(),
          input$milestone_choice,
          input$join_column_choice
        )
        filter_internal_data_tbl(
          id_tbl(),
          config(),
          input$milestone_choice,
          input$join_column_choice
        )
      })

      merged_tbl2 <- shiny::reactive({

        shiny::req(
          filtered_id_tbl2(),
          grouped_files_tbl2(),
          config(),
          input$join_column_choice
        )

        merge_tbls(
          filtered_id_tbl2(),
          grouped_files_tbl2(),
          config(),
          input$join_column_choice
        )
      })

      plot_obj2 <- shiny::reactive({
        shiny::req(
          merged_tbl2(),
          input$join_column_choice
        )
        create_milestone_reporting_plot(
          merged_tbl2(),
          input$join_column_choice
        )
      })

      output$plot2 <- plotly::renderPlotly({
        shiny::req(plot_obj2())

        plot_obj2() %>%
          plotly::ggplotly(
            source = "milestone_plot2",
            tooltip = c("Number of Files")
          ) %>%
          plotly::layout(autosize = T)
      })

      event_data2 <- shiny::reactive({
        shiny::req(plot_obj2())
        eventdata <- plotly::event_data(
          event = "plotly_click",
          source = "milestone_plot2"
        )
        if(is.null(eventdata) && !is.null(input$mock_event_data2)){
          eventdata <- input$mock_event_data2
        }
        return(eventdata)
      })

      link2 <- shiny::reactive({
        shiny::req(stringr::str_detect(fileview_id(), "^syn[0-9]+$"))
        shiny::validate(shiny::need(
          all(!is.null(event_data2()), event_data2() != ""),
          'Click on a bar above in the "ACTUAL" column to generate a link to the files in Synapse.'
        ))
        link <- create_fileview_link(fileview_id(), event_data1()$key[[1]])
      })

      output$link_button2 <- shiny::renderUI({
        shiny::actionButton(
          inputId = ns('synapse_link2'),
          label = "See selected files in Synapse.",
          icon = shiny::icon("map-marker-alt"),
          lib = "font-awesome",
          class = "btn btn-primary btn-lg btn-block",
          onclick = stringr::str_c(
            "window.open('", link2(),  "','_blank')"
          )
        )
      })

      output$download_tbl2 <- shiny::downloadHandler(
        filename = function() stringr::str_c("data-", Sys.Date(), ".csv"),
        content = function(con) readr::write_csv(merged_tbl2(), con)
      )

    }
  )
}

