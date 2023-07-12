datasets_to_samples_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  samples <-
    synapse_csv_id_to_tbl(syn, "syn50896891") %>%
    dplyr::select(
      "sample_id" = "id"
    )

  dataset_id <-
    synapse_csv_id_to_tbl(syn, "syn51132398") %>%
    dplyr::filter(.data$name == "TCGA") %>%
    dplyr::pull("id")


  datasets_to_samples <- samples %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets_to_samples"
    )

  synapse_store_table_as_csv(
    syn,
    datasets_to_samples,
    "syn51536950",
    "datasets_to_samples"
  )

}

