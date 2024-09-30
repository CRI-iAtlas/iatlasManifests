datasets_to_samples_htan_ohsu <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  samples <- synapse_csv_id_to_tbl(syn, "syn63600384") %>%
    dplyr::select(
      "sample_id" = "id"
    )

  dataset_id <- synapse_csv_id_to_tbl(syn, "syn63600273") %>%
    dplyr::pull("id")


  datasets_to_samples <- samples %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    datasets_to_samples,
    "syn63600265",
    "datasets_to_samples"
  )

}

