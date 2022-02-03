tbl1 <- tibble::tribble(
  ~col1, ~col2, ~Count,
  "a",   "z",   2,
  "b",   "y",   6,
  "c",   "x",   4
)

tbl2 <- tibble::tribble(
  ~col1, ~col2, ~Count,
  "a",   "x",   1,
  "a",   "y",   1,
  "b",   "x",   3,
  "b",   "y",   3,
  "c",   "x",   2,
  "c",   "y",   2
)

expected_tbl1 <- tibble::tribble(
  ~col1, ~col2, ~Count,
  "a",   "z",   2,
  "c",   "x",   4,
  "b",   "y",   6
) %>%
  dplyr::mutate(
    "col1" = forcats::as_factor(.data$col1),
    "col1" = forcats::fct_relevel(.data$col1, c("a", "c", "b"))
  )

expected_tbl2 <- tibble::tribble(
  ~col1, ~col2, ~Count,
  "b",   "y",   6,
  "c",   "x",   4,
  "a",   "z",   2
) %>%
  dplyr::mutate(
    "col1" = forcats::as_factor(.data$col1),
    "col1" = forcats::fct_relevel(.data$col1, c("b", "c", "a"))
  )

expected_tbl3 <- tibble::tribble(
  ~col1, ~col2, ~Count,
  "a",   "x",   1,
  "a",   "y",   1,
  "c",   "x",   2,
  "c",   "y",   2,
  "b",   "x",   3,
  "b",   "y",   3
) %>%
  dplyr::mutate(
    "col1" = forcats::as_factor(.data$col1),
    "col1" = forcats::fct_relevel(.data$col1, c("a", "c", "b"))
  )

expected_tbl4 <- tibble::tribble(
  ~col1, ~col2, ~Count,
  "b",   "x",   3,
  "b",   "y",   3,
  "c",   "x",   2,
  "c",   "y",   2,
  "a",   "x",   1,
  "a",   "y",   1
) %>%
  dplyr::mutate(
    "col1" = forcats::as_factor(.data$col1),
    "col1" = forcats::fct_relevel(.data$col1, c("b", "c", "a"))
  )

test_that("reorder_table", {
  expect_equal(
    reorder_table(
      tbl1, ascending = T, value_column = "Count", reorder_column = "col1"
    ),
    expected_tbl1
  )
  expect_equal(
    reorder_table(
      tbl1, ascending = F, value_column = "Count", reorder_column = "col1"
    ),
    expected_tbl2
  )

  expect_equal(
    reorder_table(
      tbl2, ascending = T, value_column = "Count", reorder_column = "col1"
    ),
    expected_tbl3
  )
  expect_equal(
    reorder_table(
      tbl2, ascending = F, value_column = "Count", reorder_column = "col1"
    ),
    expected_tbl4
  )
})

test_that("reorder_table_with_config", {
  config1 <- list("reorder_table" = list(
    "reorder_column" = "col1",
    "value_column" = "Count",
    "ascending" = T
  ))

  config2 <- list("reorder_table" = list(
    "reorder_column" = "col1",
    "value_column" = "Count",
    "ascending" = F
  ))

  expect_equal(
    reorder_table_with_config(tbl1, config1),
    expected_tbl1
  )
  expect_equal(
    reorder_table_with_config(tbl1, config2),
    expected_tbl2
  )
})
