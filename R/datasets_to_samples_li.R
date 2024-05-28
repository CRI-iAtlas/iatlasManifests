datasets_to_samples_li <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  dataset_id <- synapse_csv_id_to_tbl(syn, "syn60085138") %>%
    dplyr::pull(id)

  samples <- synapse_csv_id_to_tbl(syn, "syn60085711") %>%
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
    "syn60085712",
    "datasets_to_samples"
  )
}
