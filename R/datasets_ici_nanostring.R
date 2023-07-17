build_dataset_nanostring_table <- function(){

  require(magrittr)
  require(rlang)

  datasets <-
    dplyr::tibble("name" = character(), "display" = character(), "dataset_type" = character()) %>%
    dplyr::add_row("name" = c( "Chen_CanDisc_2016", "Prat_CanRes_2017", "Melero_GBM_2019"),
                    "display" = c( "Chen 2016 - SKCM, Anti-CTLA4", "Prat 2017 - HNSC, LUAD, LUSC, SKCM, Anti-PD-1", "Melero 2019 - GBM, Anti-PD-1"),
                   "dataset_type" = "ici") %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets"
    )

  synapse_store_table_as_csv(
    syn,
    datasets,
    "syn52067722",
    "datasets"
  )

  #readr::write_csv(datasets, "synapse_storage_manifest.csv", na = "")

}

