datasets_to_samples_ici <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  original <-
    synapse_tsv_id_to_tbl(syn, "syn52067877") %>%
    dplyr::select(
      "Run_ID",
      "Dataset"
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn51589463") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  dataset_id <-
    synapse_csv_id_to_tbl(syn, "syn52060439") %>%
    dplyr::select(
      "dataset_name" = "name",
      "dataset_id" = "id"
    )


  datasets_to_samples <- original %>%
    dplyr::inner_join(samples, by = dplyr::join_by(Run_ID == sample_name)) %>%
    dplyr::inner_join(dataset_id, by = dplyr::join_by(Dataset == dataset_name)) %>%
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
    "syn52067881",
    "datasets_to_samples"
  )

}

