datasets_to_samples_prince <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  samples <- samples_prince() %>%
    #synapse_csv_id_to_tbl(syn, "syn") %>%
    dplyr::select(
      "sample_id" = "id"
    )

  dataset_id <- dataset_ici_table() %>%
    #synapse_csv_id_to_tbl(syn, "syn) %>%
    dplyr::filter(.data$name == "PRINCE") %>%
    dplyr::pull("id")


  datasets_to_samples <- samples %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets_to_samples"
    )

  # synapse_store_table_as_csv(
  #   syn,
  #   datasets_to_samples,
  #   "syn51536950",
  #   "datasets_to_samples"
  # )

}

