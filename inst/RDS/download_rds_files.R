syn <- create_synapse_login()

# nf ----

nf_files         <- read_rds_file_from_synapse("syn24474805", syn)
nf_incoming_data <- read_rds_file_from_synapse("syn25257326", syn)
nf_publications  <- read_rds_file_from_synapse("syn24474886", syn)
nf_studies       <- read_rds_file_from_synapse("syn24474946", syn)
nf_tools         <- read_rds_file_from_synapse("syn24475065", syn)

saveRDS(nf_files, "inst/RDS/nf_files.rds")
saveRDS(nf_incoming_data, "inst/RDS/nf_incoming_data.rds")
saveRDS(nf_studies, "inst/RDS/nf_studies.rds")
saveRDS(nf_publications, "inst/RDS/nf_publications.rds")
saveRDS(nf_tools, "inst/RDS/nf_tools.rds")
