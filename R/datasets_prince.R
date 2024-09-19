dataset_prince <- function(){

  require(magrittr)
  require(rlang)

  ici_datasets <-
    data.frame("name" = c("PRINCE"), "display" = c("PRINCE - PDAC, PD-1"), "dataset_type" = "ici") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    ici_datasets,
    "syn63327057",
    "datasets"
  )

  #readr::write_csv(datasets, "synapse_storage_manifest.csv", na = "")

}

