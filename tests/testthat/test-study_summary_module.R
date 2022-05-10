
test_that("study_summary_module_ui", {
  expect_type(study_summary_module_ui("id"), "list")
})

test_that("study_summary_module_server", {
  shiny::testServer(
    study_summary_module_server,
    args = list(
      "data" = shiny::reactive(get_synthetic_data()),
      "config" = shiny::reactive(get_study_summary_config())
    ),
    {
      session$setInputs("study_summary-filter_value" = "All")
      session$setInputs("study_summary-study_table_rows_selected" = 1)

      expect_type(output$header_text, "character")

      expect_type(filtered_data(), "list")
      expect_named(
        filtered_data(), c('tables', 'selected_study')
      )
      expect_type(filtered_data()$tables, "list")
      expect_named(
        filtered_data()$tables,
        c('files', 'publications', 'studies', 'milestones', 'tools')
      )
      expect_equal(
        filtered_data()$selected_study,
        "StudyA"
      )

      expect_type(output$study_summary, "character")

      expect_true(tibble::is_tibble(data_focus_plot_tbl()))
      expect_named(
        data_focus_plot_tbl(),
        c('Study ID', 'Assays Used', 'Species Used')
      )
      expect_true(nrow(data_focus_plot_tbl()) > 0)

      expect_type(output$data_focus_plot, "character")
    }
  )
})

