
test_that("study_selection_module_ui", {
  expect_type(study_selection_module_ui("id"), "list")
})

test_that("study_selection_module_server", {
  shiny::testServer(
    study_selection_module_server,
    args = list(
      "data" = shiny::reactive(get_synthetic_data()),
      "config" = shiny::reactive(get_study_summary_config())
    ),
    {
      expect_type(filter_choices(), "character")
      expect_type(output$filter_ui, "list")
      session$setInputs("filter_value" = "All")
      expect_type(filtered_table(), "list")
      expect_type(study_table(), "list")
      expect_type(output$study_table, "character")
      session$setInputs("study_table_rows_selected" = 3)
      expect_true(tibble::is_tibble(selected_study_row()))
      expect_equal(nrow(selected_study_row()), 1)

      expect_type(selected_study_id(), "character")
      expect_equal(
        selected_study_id(),
        "S3"
      )
      expect_type(selected_study_name(), "character")
      expect_equal(
        selected_study_name(),
        "StudyC"
      )
      expect_type(output$study, "list")
      res <- session$getReturned()()
      expect_type(res$selected_study, "character")

    }
  )
})
