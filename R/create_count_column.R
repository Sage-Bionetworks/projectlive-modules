#' Create Plot Count Dataframe with Config
#' This function is used to create a count column.
#'
#' @param tbl A tibble
#' @param config A named list. This list must have a named list named
#' count_column. This list must have names "count" which must be T or F, and
#' "complete_columns" which must be a list of column names.
create_count_column_with_config <- function(tbl, config){
  do_count <- any(
    is.null(config$count_column),
    is.null(config$count_column$count),
    config$count_column$count
  )

  if(!do_count) return(tbl)
  else {
    result <- create_count_column(
      tbl,
      config$count_column$complete_columns
    )
  }
  return(result)
}

#' Create Count Column
#' This function is used to create a count column.
#'
#' @param tbl A tibble.
#' @param complete_columns A list of strings that are columns in the data. This
#' should be the aesthetic that is intended to be present in the plot even if
#' it has zero counts such as the x-axis, or possibly a facet.
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
create_count_column <- function(tbl, complete_columns = NULL){
  tbl %>%
    dplyr::mutate(dplyr::across(unlist(complete_columns), forcats::as_factor)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by_all() %>%
    dplyr::tally(., name = "Count") %>%
    dplyr::ungroup() %>%
    tidyr::complete(
      !!!rlang::syms(unlist(complete_columns)),
      fill = list("Count" = 0L)
    )
}
