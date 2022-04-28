dates        <- seq(
  lubridate::ymd('2021-04-07'),
  lubridate::ymd('2022-03-22'),
  by = '1 month'
)

date_estimates <- seq(
  lubridate::ymd('2021-07-01'),
  lubridate::ymd('2022-05-01'),
  by = '5 month'
)

assays       <- c("Assay1", "Assay2", "Assay3", NA)
species      <- c("Species1", "Species2", NA)
initiatives  <- c("Initiative1", "Initiative2")
access_types <- c("Public", "Private", NA)
file_formats <- c("jpg", "csv", "tsv")
milestones   <- c(1L,2L,3L)

n_studies <- 10

studies <- dplyr::tibble(
  "study_name"  = stringr::str_c("Study", stringr::str_to_upper(letters[1:n_studies])),
  "study_id"    = stringr::str_c("S", as.character(1:n_studies)),
  "initiative"  = sample(initiatives, n_studies,  replace = T),
)

study_ids <- c(studies$study_id, "SX", NA)


n_files      <- 1000

files <- dplyr::tibble(
  "file_name"   = stringr::str_c("File", as.character(1:n_files)),
  "file_id"     = stringr::str_c("F", as.character(1:n_files)),
  "study_id"    = sample(study_ids, n_files,  replace = T),
  "species"     = sample(species, n_files,  replace = T),
  "assay"       = sample(assays, n_files,  replace = T),
  "access_type" = sample(access_types, n_files,  replace = T),
  "date"        = sample(dates, n_files,  replace = T),
  "file_format" = sample(file_formats, n_files,  replace = T),
  "milestone"   = sample(milestones, n_files,  replace = T),
) %>%
  dplyr::mutate("year" = lubridate::year(.data$date))


n_pubs      <- 1000

publications <- dplyr::tibble(
  "publication_name" = stringr::str_c("File", as.character(1:n_pubs)),
  "publication_id"   = stringr::str_c("F", as.character(1:n_pubs)),
  "study_id"         = sample(study_ids, n_pubs,  replace = T),
  "assay"            = sample(assays, n_pubs,  replace = T),
  "date"             = sample(dates, n_pubs,  replace = T)
) %>%
  dplyr::mutate("year" = lubridate::year(.data$date))


milestones <- dplyr::tibble(
  "study_id"      = study_ids[[1]],
  "milestone"     = milestones,
  "file_format"   = file_formats[[1]],
  "expcted_files" = c(10, 20, 30),
  "date_estimate" = date_estimates
)

n_tools <- 20

tools <- dplyr::tibble(
  "study_id"  = sample(study_ids, n_tools,  replace = T),
  "tool_name" = stringr::str_c("Tool", as.character(1:n_tools)),
  "tool_id"   = stringr::str_c("T", as.character(1:n_tools)),
)


saveRDS(studies,      "inst/RDS/studies.rds")
saveRDS(files,        "inst/RDS/files.rds")
saveRDS(publications, "inst/RDS/publications.rds")
saveRDS(milestones,   "inst/RDS/milestones.rds")
saveRDS(tools,        "inst/RDS/tools.rds")
