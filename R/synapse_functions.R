library(magrittr)

create_synapse_login <- function(){
  if ("use_conda_env.R" %in% list.files("R")) source("R/use_conda_env.R")
  synapseclient <- reticulate::import("synapseclient")
  syn <- synapseclient$Synapse()
  syn$login()
  return(syn)
}

synapse_feather_id_to_tbl <- function(syn, id) {
  id %>%
    syn$get(.) %>%
    purrr::pluck("path") %>%
    arrow::read_feather(.) %>%
    dplyr::as_tibble()
}

synapse_csv_id_to_tbl <- function(syn, id) {
  id %>%
    syn$get(.) %>%
    purrr::pluck("path") %>%
    readr::read_csv()
}

synapse_tsv_id_to_tbl <- function(syn, id) {
  id %>%
    syn$get(.) %>%
    purrr::pluck("path") %>%
    readr::read_tsv()
}

synapse_rds_id_to_tbl <- function(syn, id) {
  id %>%
    syn$get(.) %>%
    purrr::pluck("path") %>%
    readRDS() %>%
    dplyr::as_tibble()
}

synapse_excel_id_to_tbl <- function(syn, id) {
  id %>%
    syn$get(.) %>%
    purrr::pluck("path") %>%
    readxl::read_excel(.) %>%
    dplyr::as_tibble()
}

synapse_store_file <- function(syn, file_path, parent_id, component_name) {
  synapseclient <- reticulate::import("synapseclient")
  file_entity <- synapseclient$File(file_path, parent = parent_id)
  file_entity$annotations$Component <- component_name
  syn$store(file_entity)
}

synapse_store_table_as_csv <- function(syn, table, parent_id, component_name) {
  readr::write_csv(table, "synapse_storage_manifest.csv", na = "")
  synapse_store_file(syn, "synapse_storage_manifest.csv", parent_id, component_name)
}
