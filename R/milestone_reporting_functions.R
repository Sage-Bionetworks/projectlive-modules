create_internal_tracking_datatable <- function(tbl, config){
  date_column        <- rlang::sym(config$date_estimate_column)
  milestone_column   <- rlang::sym(config$milestone_column)

  tbl %>%
    dplyr::filter(!is.na(!!date_column)) %>%
    dplyr::select(!!milestone_column, !!date_column) %>%
    dplyr::arrange(!!date_column) %>%
    dplyr::distinct()
}

filter_internal_data_tbl <- function(tbl, config, milestone, join_column_string){
  join_column      <- rlang::sym(join_column_string)
  milestone_column <- rlang::sym(config$milestone_column)
  expected_column  <- rlang::sym(config$expected_files_column)

  tbl %>%
    dplyr::filter(!!milestone_column == milestone) %>%
    dplyr::select(
      !!join_column,
      !!milestone_column,
      !!expected_column
    )
}

filter_files_tbl1 <- function(tbl, config, date_range_start, date_range_end){
  date_column   <- rlang::sym(config$date_created_column)

  tbl %>%
    dplyr::filter(
      !!date_column < date_range_end,
      !!date_column > date_range_start
    )
}

filter_files_tbl2 <- function(tbl, config, milestone){
  milestone_column <- rlang::sym(config$milestone_column)

  tbl %>%
    dplyr::filter(!!milestone_column == milestone)
}

group_files_tbl <- function(tbl, config, join_column_string){
  join_column    <- rlang::sym(join_column_string)
  actual_column  <- rlang::sym(config$actual_files_column)
  file_id_column <- rlang::sym(config$file_id_column)

  tbl %>%
    dplyr::group_by(!!join_column) %>%
    dplyr::summarise(
      !!actual_column := dplyr::n(),
      !!file_id_column := stringr::str_c(!!file_id_column, collapse = ",")
    )
}

merge_tbls <- function(id_tbl, files_tbl, config, join_column_string){
  join_column      <- rlang::sym(join_column_string)
  actual_column    <- rlang::sym(config$actual_files_column)
  expected_column  <- rlang::sym(config$expected_files_column)
  file_id_column   <- rlang::sym(config$file_id_column)

  id_tbl %>%
    dplyr::full_join(files_tbl, by = join_column_string) %>%
    dplyr::mutate(
      !!actual_column := dplyr::if_else(
        is.na(!!actual_column),
        0L,
        !!actual_column
      )
    ) %>%
    dplyr::select(!!join_column, !!expected_column, !!actual_column, !!file_id_column) %>%
    tidyr::pivot_longer(
      cols = -c(!!join_column, !!file_id_column),
      names_to = "Types of Files",
      values_to = "Number of Files"
    ) %>%
    dplyr::mutate(
      !!file_id_column := dplyr::if_else(
        .data$`Types of Files` == config$expected_files_column,
        "",
        !!file_id_column
      ),
      "Types of Files" = base::factor(
        .data$`Types of Files`,
        levels = c(
          config$expected_files_column, config$actual_files_column
        )
      )
    )
}

get_filview_id_from_study <- function(study_id, syn){

  if(!stringr::str_detect(study_id, "^syn[0-9]+$")){
    return(stringr::str_c(study_id, " is not a valid synapse id."))
  }

  children <- try(
    syn$getChildren(
      study_id,
      includeTypes = list("entityview"),
      sortBy = "NAME",
      sortDirection = "DESC"
    ) %>%
      reticulate::iterate()
  )

  if(class(children) == "try-error"){
    return(stringr::str_c(study_id, " is not a valid synapse id."))
  }

  if(length(children) == 0){
    return(stringr::str_c(study_id, " has no children of type EntityView."))
  }

  detect_fileview <- function(entity){
    return(entity$name %in% c("Files and Metadata", "Project Files and Metadata"))
  }

  children <- purrr::keep(children, detect_fileview)

  if(length(children) == 0){
    return(stringr::str_c(study_id, " has no children with correct name."))
  }

  if(length(children) > 1){
    return(stringr::str_c(study_id, " has multiple children with correct name."))
  }

  return(children[[1]]$id)
}

create_fileview_query <- function(fileview_id, file_id_string){
  "SELECT * FROM {fileview_id} WHERE id IN ({file_id_string})" %>%
    glue::glue() %>%
    as.character()
}

create_fileview_query_json <- function(query){
  json_list <- list(
    "sql" = query,
    "additionalFilters" = list(),
    "selectedFacets" = list(),
    "includeEntityEtag" = T,
    "offset" = 0,
    "limit" = 25
  )
}

create_fileview_link <- function(fileview_id, json_list){

  encoded_json <- json_list %>%
    jsonlite::toJSON(auto_unbox = T) %>%
    charToRaw() %>%
    base64enc::base64encode()

  link <-
    "https://www.synapse.org/#!Synapse:{fileview_id}/tables/query/{encoded_json}" %>%
    glue::glue() %>%
    as.character()

  return(link)
}



