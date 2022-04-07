
test_that("milestone_reporting_module_ui", {
  expect_type(milestone_reporting_module_ui("id"), "list")
})

test_that("milestone_reporting_module_server", {
  shiny::testServer(
    milestone_reporting_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(
        get_nf_study_summary_config()$milestone_reporting_plot
      )
    ),
    {
      session$setInputs("join_column_choice" = "File Format")
      session$setInputs("days_choice" = 365)
      session$setInputs("dt_rows_selected" = 2)
      session$setInputs("milestone_choice" = 2)

      expect_type(join_column_choices(), "character")
      expect_equal(join_column_choices(), c("Data Type", "File Format"))
      expect_type(output$join_column_choice_ui, "list")

      expect_true(tibble::is_tibble(files_tbl()))
      expect_named(
        files_tbl(),
        c('File Format', 'Data Type', 'Date Created', 'Progress Report Number')
      )
      expect_true(nrow(files_tbl()) > 0)

      expect_true(tibble::is_tibble(id_tbl()))
      expect_named(
        id_tbl(),
        c(
          'File Format',
          'Data Type',
          'Designated Upload Date',
          'Progress Report Number',
          'Expected'
        )
      )
      expect_true(nrow(id_tbl()) > 0)

      expect_type(dt_tbl(), "list")
      expect_type(output$dt, "character")
      expect_type(dt_row(), "list")
      expect_type(selected_milestone(), "integer")
      expect_type(date_range_start(), "double")
      expect_type(date_range_end(), "double")
      expect_type(plot_title1(), "character")
      expect_type(filtered_id_tbl1(), "list")
      expect_type(filtered_files_tbl1(), "list")
      expect_type(merged_tbl1(), "list")
      expect_type(plot_obj1(), "list")

      expect_type(milestone_choices(), "integer")
      expect_type(output$milestone_choice_ui, "list")

      expect_type(plot_title2(), "character")
      expect_type(filtered_id_tbl2(), "list")
      expect_type(filtered_files_tbl2(), "list")
      expect_type(merged_tbl2(), "list")
      expect_type(plot_obj2(), "list")
    }
  )
})

files_tbl1 <- dplyr::tibble(
  "fileFormat" = "txt",
  "dataType" = "x",
  "date" = lubridate::today(),
  "progressReportNumber" = 1:10
)

id_tbl1 <- dplyr::tibble(
  "fileFormat" = "txt",
  "dataType" = "x",
  "date_uploadestimate" = lubridate::today(),
  "estimatedMinNumSamples" = c(2,2),
  "progressReportNumber" = 1:2
)

data1 <- shiny::reactive(
  list(
    "tables" = list(
      "files" = files_tbl1,
      "incoming_data" = id_tbl1
    )
  )
)


# ensure milestone choices come from both files and incoming data
test_that("milestone_reporting_module_server2", {
  shiny::testServer(
    milestone_reporting_module_server,
    args = list(
      "data" = data1,
      "config" = shiny::reactiveVal(
        get_nf_study_summary_config()$milestone_reporting_plot
      )
    ),
    {
      session$setInputs("join_column_choice" = "File Format")
      session$setInputs("days_choice" = 365)
      session$setInputs("dt_rows_selected" = 2)
      session$setInputs("milestone_choice" = 10)

      expect_type(join_column_choices(), "character")
      expect_equal(join_column_choices(), c("Data Type", "File Format"))
      expect_type(output$join_column_choice_ui, "list")

      expect_true(tibble::is_tibble(files_tbl()))
      expect_named(
        files_tbl(),
        c('File Format', 'Data Type', 'Date Created', 'Progress Report Number')
      )
      expect_true(nrow(files_tbl()) > 0)

      expect_true(tibble::is_tibble(id_tbl()))
      expect_named(
        id_tbl(),
        c(
          'File Format',
          'Data Type',
          'Designated Upload Date',
          'Progress Report Number',
          'Expected'
        )
      )
      expect_true(nrow(id_tbl()) > 0)

      expect_type(dt_tbl(), "list")
      expect_type(output$dt, "character")
      expect_type(dt_row(), "list")
      expect_type(selected_milestone(), "integer")
      expect_type(date_range_start(), "double")
      expect_type(date_range_end(), "double")
      expect_type(plot_title1(), "character")
      expect_type(filtered_id_tbl1(), "list")
      expect_type(filtered_files_tbl1(), "list")
      expect_type(merged_tbl1(), "list")
      expect_type(plot_obj1(), "list")

      expect_equal(milestone_choices(), 1:10)
      expect_type(output$milestone_choice_ui, "list")

      expect_type(plot_title2(), "character")
      expect_type(filtered_id_tbl2(), "list")
      expect_type(filtered_files_tbl2(), "list")
      expect_type(merged_tbl2(), "list")
      expect_type(plot_obj2(), "list")
    }
  )
})


