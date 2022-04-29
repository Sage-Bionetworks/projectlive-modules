
test_that("publication_status_module_ui", {
  expect_type(summary_snapshot_module_ui("id"), "list")
})

test_that("publication_status_module_server", {
  shiny::testServer(
    summary_snapshot_module_server,
    args = list(
      "data" = shiny::reactive(get_synthetic_data()),
      "config" = shiny::reactive(get_publication_status_config())
    ),
    {
      expect_type(output$header_text, "character")
    }
  )
})
