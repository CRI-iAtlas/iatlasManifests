datasets_to_samples_pcawg <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  samples <-
    synapse_csv_id_to_tbl(syn, "syn51088163") %>%
    dplyr::select(
      "sample_id" = "id"
    )

  dataset_id <-
    synapse_csv_id_to_tbl(syn, "syn51132398") %>%
    dplyr::filter(.data$name == "PCAWG") %>%
    dplyr::pull("id")


  datasets_to_samples <- samples %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets_to_tags"
    )

  synapse_store_table_as_csv(
    syn,
    datasets_to_samples,
    "syn51536953",
    "datasets_to_samples"
  )

}

