datasets_to_samples_htan <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  samples <- samples_htan() %>% #update
    dplyr::select(
      "sample_id" = "id"
    )

  dataset_id <- dataset_htan() %>%  #update
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
  #   "", #update
  #   "datasets_to_samples"
  # )

}

