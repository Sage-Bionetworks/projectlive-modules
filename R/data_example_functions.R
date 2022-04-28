get_synthetic_data <- function(){
  list(
    "tables" = list(
      "files" = "files" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "publications" = "publications" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "studies" = "studies" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "milestones" = "milestones" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble(),
      "tools" = "tools" %>%
        get_rds_path() %>%
        readRDS() %>%
        dplyr::as_tibble()
    )
  )
}

get_summary_snapshot_config <- function(){
  "summary_snapshot_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}

get_publication_status_config <- function(){
  "publication_status_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}

get_study_summary_config <- function(){
  "study_summary_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}

get_new_submissions_config <- function(){
  "new_submissions_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}

get_nf_synapse_config <- function(){
  "nf_dev_synapse_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}

get_csbc_synapse_config <- function(){
  "csbc_synapse_module" %>%
    get_json_path() %>%
    jsonlite::read_json()
}
