
test_that("milestone_reporting_module_ui", {
  expect_type(milestone_reporting_module_ui("id"), "list")
})

test_that("milestone_reporting_module_server", {
  shiny::testServer(
    milestone_reporting_module_server,
    args = list(
      "data" = shiny::reactive(get_synthetic_data()),
      "config" = shiny::reactive(
        get_study_summary_config()$milestone_reporting_plot
      )
    ),
    {
      session$setInputs("join_column_choice" = "File Format")
      session$setInputs("days_choice" = 365)
      session$setInputs("dt_rows_selected" = 2)
      session$setInputs("milestone_choice" = 2)

      expect_type(join_column_choices(), "character")
      expect_equal(join_column_choices(), c("File Format"))
      expect_type(output$join_column_choice_ui, "list")

      expect_true(tibble::is_tibble(files_tbl()))
      expect_named(
        files_tbl(),
        c('File Format', 'Date Created', 'Milestone Number', 'File ID')
      )
      expect_true(nrow(files_tbl()) > 0)

      expect_true(tibble::is_tibble(id_tbl()))
      expect_named(
        id_tbl(),
        c('File Format', 'Date Estimate', 'Milestone Number', 'Expected')
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
      expect_true(tibble::is_tibble(grouped_files_tbl1()))
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
  "file_format" = "txt",
  "date" = lubridate::today(),
  "milestone" = 1:10,
  "file_id" = stringr::str_c("F", 1:10)
)

id_tbl1 <- dplyr::tibble(
  "file_format" = "txt",
  "date_estimate" = lubridate::today(),
  "expected_files" = c(2,2),
  "milestone" = 1:2
)

data1 <- shiny::reactive(
  list(
    "tables" = list(
      "files" = files_tbl1,
      "milestones" = id_tbl1
    )
  )
)


# ensure milestone choices come from both files and incoming data
test_that("milestone_reporting_module_server2", {
  shiny::testServer(
    milestone_reporting_module_server,
    args = list(
      "data" = data1,
      "config" = shiny::reactive(
        get_study_summary_config()$milestone_reporting_plot
      )
    ),
    {
      session$setInputs("join_column_choice" = "File Format")
      session$setInputs("days_choice" = 365)
      session$setInputs("dt_rows_selected" = 2)
      session$setInputs("milestone_choice" = 10)

      expect_type(join_column_choices(), "character")
      expect_equal(join_column_choices(), c("File Format"))
      expect_type(output$join_column_choice_ui, "list")

      expect_true(tibble::is_tibble(files_tbl()))
      expect_named(
        files_tbl(),
        c('File Format', 'Date Created', 'Milestone Number', "File ID")
      )
      expect_true(nrow(files_tbl()) > 0)

      expect_true(tibble::is_tibble(id_tbl()))
      expect_named(
        id_tbl(),
        c('File Format', 'Date Estimate', 'Milestone Number', 'Expected')
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
      expect_true(tibble::is_tibble(grouped_files_tbl1()))
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


