datasets_to_samples_prince <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  samples <-
    synapse_csv_id_to_tbl(syn, "syn63332022") %>%
    dplyr::select(
      "sample_id" = "id"
    )

  dataset_id <-
    synapse_csv_id_to_tbl(syn, "syn63329595") %>%
    dplyr::pull("id")


  datasets_to_samples <- samples %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
    )

  synapse_store_table_as_csv(
    syn,
    datasets_to_samples,
    "syn63327060",
    "datasets_to_samples"
  )

}

