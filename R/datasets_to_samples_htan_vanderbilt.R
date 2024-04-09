datasets_to_samples_htan <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  dataset_id <- synapse_csv_id_to_tbl(syn, "syn53678206") %>%
    dplyr::pull(id)

  samples <- synapse_csv_id_to_tbl(syn, "syn53678312") %>%
    dplyr::select(
      "name",
      "sample_id" = "id"
    )

  datasets_to_samples <- samples %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets_to_samples"
    )

  synapse_store_table_as_csv(
    syn,
    datasets_to_samples,
    "syn53678429",
    "datasets_to_samples"
  )

}
