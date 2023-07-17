datasets_to_samples_ici_nanostring <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  original <-
    synapse_feather_id_to_tbl(syn, "syn27790733") %>% #replace
    dplyr::select(
      "name",
      "dataset"
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn51589463") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  dataset_id <-
    synapse_csv_id_to_tbl(syn, "syn52067760") %>%
    dplyr::select(
      "dataset_name" = "name",
      "dataset_id" = "id"
    )


  datasets_to_samples <- original %>%
    dplyr::inner_join(samples, by = dplyr::join_by(name == sample_name)) %>%
    dplyr::inner_join(dataset_id, by = dplyr::join_by(dataset == dataset_name)) %>%
    dplyr::select(
      "sample_id",
      "dataset_id"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets_to_samples"
    )

  synapse_store_table_as_csv(
    syn,
    datasets_to_samples,
    "syn52068900",
    "datasets_to_samples"
  )

}

