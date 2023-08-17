dataset_ici_table <- function(){

  require(magrittr)
  require(rlang)

  ici_datasets <-
    synapse_feather_id_to_tbl(syn, "syn25981548") %>%
    dplyr::rename(
      "dataset_type" = "type"
    ) %>%
    dplyr::add_row("name" = c("PRINCE"), "display" = c("PRINCE"), "dataset_type" = "ici") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets"
    )

  # synapse_store_table_as_csv(
  #   syn,
  #   ici_datasets,
  #   "syn52060414",
  #   "datasets"
  # )

  #readr::write_csv(datasets, "synapse_storage_manifest.csv", na = "")

}

