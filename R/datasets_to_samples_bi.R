datasets_to_samples_bi <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  dataset_id <- synapse_csv_id_to_tbl(syn, "syn60531397") %>%
    dplyr::pull(id)

  samples <- synapse_csv_id_to_tbl(syn, "syn60531805") %>%
    dplyr::select(
      "name",
      "sample_id" = "id"
    )

  datasets_to_samples <- samples %>%
    dplyr::select(-name) %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    datasets_to_samples,
    "syn60530039",
    "datasets_to_samples"
  )
}
