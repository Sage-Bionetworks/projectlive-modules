
test_that("plot_module_ui", {
  expect_type(plot_module_ui("id", "Title"), "list")
})

test_that("plot_module_server", {
  shiny::testServer(
    plot_module_server,
    args = list(
      "data" = shiny::reactive(get_synthetic_data()),
      "config" = get_summary_snapshot_config() %>%
        purrr::pluck("initiative_activity") %>%
        shiny::reactive(),
      "plot_func" = shiny::reactiveVal("create_initiative_activity_plot")
    ),
    {
      expect_true(tibble::is_tibble(plot_data()))
      expect_named(plot_data(), c("Species", "Initiative", "Access Type", "Count"))
      expect_type(output$plot, "character")
      expect_type(output$download_tbl, "character")
    }
  )
})

test_that("plot_module_server2", {
  shiny::testServer(
    plot_module_server,
    args = list(
      "data" = shiny::reactive(get_synthetic_data()),
      "config" = get_publication_status_config() %>%
        purrr::pluck("publication_status") %>%
        shiny::reactive(),
      "plot_func" = shiny::reactiveVal("create_publication_status_plot")
    ),
    {
      expect_true(tibble::is_tibble(plot_data()))
      expect_named(plot_data(), c("Year", "Study ID", "Count"))
      expect_type(output$plot, "character")
      expect_type(output$download_tbl, "character")
    }
  )
})

test_that("plot_module_server3", {
  shiny::testServer(
    plot_module_server,
    args = list(
      "data" = shiny::reactive(get_synthetic_data()),
      "config" = get_publication_status_config() %>%
        purrr::pluck("publication_disease") %>%
        shiny::reactive(),
      "plot_func" = shiny::reactiveVal("create_publication_disease_plot")
    ),
    {
      expect_true(tibble::is_tibble(plot_data()))
      expect_named(plot_data(), c('Year', 'Assay', 'Count'))
      expect_type(output$plot, "character")
      expect_type(output$download_tbl, "character")
    }
  )
})

test_that("plot_module_server4", {
  shiny::testServer(
    plot_module_server,
    args = list(
      "data" = shiny::reactive(get_synthetic_data()),
      "config" = get_summary_snapshot_config() %>%
        purrr::pluck("resources_generated") %>%
        shiny::reactive(),
      "plot_func" = shiny::reactiveVal("create_resources_generated_plot")
    ),
    {
      expect_true(tibble::is_tibble(plot_data()))
      expect_named(plot_data(), c('Year', 'Study ID', 'Assay', 'Count'))
      expect_type(output$plot, "character")
      expect_type(output$download_tbl, "character")
    }
  )
})

