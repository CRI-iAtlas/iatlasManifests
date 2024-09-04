datasets_to_samples_krishna <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  dataset_id <- synapse_csv_id_to_tbl(syn, "syn59195251") %>%
    dplyr::pull(id)

  samples <- synapse_csv_id_to_tbl(syn, "syn59204288") %>%
    dplyr::select(
      "name",
      "sample_id" = "id"
    )

  datasets_to_samples <- samples %>%
    dplyr::select(-name) %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets_to_samples"
    )

  synapse_store_table_as_csv(
    syn,
    datasets_to_samples,
    "syn59204342",
    "datasets_to_samples"
  )
}
