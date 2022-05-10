
test_that("file_upload_timeline_module_ui", {
  expect_type(file_upload_timeline_module_ui("id"), "list")
})

test_that("file_upload_timeline_module_server", {
  shiny::testServer(
    file_upload_timeline_module_server,
    args = list(
      "data" = shiny::reactive(get_synthetic_data()),
      "config" = shiny::reactive(get_summary_snapshot_config()$file_upload_timeline)
    ),
    {
      expect_type(file_upload_timeline_filter_choices(), "character")
      expect_type(output$file_upload_timeline_filter_ui, "list")
      session$setInputs("file_upload_timeline_filter_value" = "All")
      expect_type(file_upload_timeline_data(), "list")
      expect_type(output$file_upload_timeline, "character")
      expect_type(output$download_tbl, "character")
    }
  )
})
