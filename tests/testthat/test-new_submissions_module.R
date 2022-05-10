test_that("new_submissions_module_ui", {
  expect_type(new_submissions_module_ui("id"), "list")
})

test_that("new_submissions_module_server", {
  shiny::testServer(
    new_submissions_module_server,
    args = list(
      "data" = shiny::reactive(get_synthetic_data()),
      "config" = shiny::reactive(get_new_submissions_config())
    ),
    {
      session$setInputs(
        "new_files_day_choice" = 60
      )
      expect_type(output$header_text, "character")
      expect_type(minimum_date(), "double")
      expect_type(filtered_data(), "list")
      expect_named(filtered_data(), c("tables", "minimum_date"))
      expect_type(filtered_data()$tables, "list")
      expect_named(filtered_data()$tables, "files")
      expect_true(tibble::is_tibble(filtered_data()$tables$files))
      expect_type(filtered_data()$minimum_date, "double")
      expect_true(tibble::is_tibble(data_table()))
      expect_true(nrow(data_table()) > 0)
      expect_named(
        data_table(),
        c(
          'File Name',
          'Date',
          'Study ID',
          'Assay'
        )
      )
      expect_type(output$data_table, "character")

    }
  )
})
