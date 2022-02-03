reorder_table_with_config <- function(tbl, config){
  do_reorder <- all(
    !is.null(config$reorder_table),
    !is.null(config$reorder_table$reorder_column),
    !is.null(config$reorder_table$value_column),
    !is.null(config$reorder_table$ascending)
  )

  if(!do_reorder) return(tbl)
  else {
    result <- reorder_table(
      tbl,
      config$reorder_table$value_column,
      config$reorder_table$reorder_column,
      config$reorder_table$ascending
    )
  }
  return(result)
}

reorder_table <- function(tbl, value_column, reorder_column, ascending = T){
  value_column <- rlang::sym(value_column)
  reorder_column <- rlang::sym(reorder_column)

  if(ascending) func <- identity
  else func <- dplyr::desc

  levels <- tbl %>%
    dplyr::select(value_column, reorder_column) %>%
    dplyr::group_by(!!reorder_column) %>%
    dplyr::summarise("Count" = sum(!!value_column)) %>%
    dplyr::arrange(func(.data$Count)) %>%
    dplyr::pull(!!reorder_column)

  tbl %>%
    dplyr::mutate(
      !!reorder_column := forcats::as_factor(!!reorder_column),
      !!reorder_column := forcats::fct_relevel(!!reorder_column, levels)
    ) %>%
    dplyr::arrange(!!reorder_column)
}
