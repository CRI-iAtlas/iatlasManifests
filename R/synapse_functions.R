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
  path <- id %>%
    syn$get(.) %>%
    purrr::pluck("path") %>%
    readRDS() %>%
    dplyr::as_tibble()
}
