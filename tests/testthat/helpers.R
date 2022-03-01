synapseclient <- reticulate::import("synapseclient", delay_load = TRUE)
syn <- synapseclient$Synapse()
invisible(syn$login())


nf_data <- get_nf_data()
nf_gff_data  <- get_nf_gff_data()
nf_ntap_data <- get_nf_ntap_data()
csbc_data <- get_csbc_data()


