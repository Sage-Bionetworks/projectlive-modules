
test_that("study_summary_module_ui", {
  expect_type(study_summary_module_ui("id"), "list")
})

test_that("nf_study_summary_module_server", {
  shiny::testServer(
    study_summary_module_server,
    args = list(
      "data" = shiny::reactiveVal(nf_data),
      "config" = shiny::reactiveVal(get_nf_study_summary_config())
    ),
    {
      session$setInputs("study_summary-filter_value" = "All")
      session$setInputs("study_summary-study_table_rows_selected" = 9)

      expect_type(output$header_text, "character")

      expect_type(filtered_data(), "list")
      expect_named(filtered_data(), c('tables', 'selected_group', 'selected_study'))
      expect_type(filtered_data()$tables, "list")
      expect_named(filtered_data()$tables, c('files', 'publications', 'studies', 'tools', 'incoming_data'))
      expect_equal(filtered_data()$selected_group, "Example Group")
      expect_equal(filtered_data()$selected_study, "Cutaneous Neurofibroma - Models, Biology, and Translation")
      expect_true(nrow(filtered_data()$tables$incoming_data) > 0)


      expect_type(output$study_summary, "character")

      expect_true(tibble::is_tibble(data_focus_plot_tbl()))
      expect_named(
        data_focus_plot_tbl(),
        c(
          'Study Name',
          'Assays Used',
          'Resource Added',
          'Species Used',
          'Tumor Types Investigated'
        )
      )
      expect_true(nrow(data_focus_plot_tbl()) > 0)

      expect_type(output$data_focus_plot, "character")
    }
  )
})

test_that("csbc_study_summary_module_server", {
  shiny::testServer(
    study_summary_module_server,
    args = list(
      "data" = shiny::reactiveVal(csbc_data),
      "config" = shiny::reactiveVal(get_csbc_study_summary_config())
    ),
    {
      session$setInputs("study_summary-filter_value" = "All")
      session$setInputs("study_summary-study_table_rows_selected" = 34)

      expect_type(output$header_text, "character")

      expect_type(filtered_data(), "list")
      expect_named(filtered_data(), c('tables', 'selected_study'))
      expect_type(filtered_data()$tables, "list")
      expect_named(filtered_data()$tables, c('files', 'publications', 'studies', 'tools'))
      expect_null(filtered_data()$selected_group, "x")
      expect_equal(filtered_data()$selected_study, "H Lee Moffitt Cancer Center and Research Institute")

      expect_type(output$study_summary, "character")
      expect_type(output$data_focus_plot, "character")
    }
  )
})
