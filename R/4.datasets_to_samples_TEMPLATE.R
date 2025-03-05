datasets_to_samples_TEMPLATE <- function(){ #UPDATE function name

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #This table should be simple: we just want a table listing the association between dataset_id and sample_id,
  #so you just need to provide the synapse IDs for the dataset and samples files
  # columns in the datasets_to_samples table:
  # name = sample name
  # sample_id = id for the associated patient, generated in the patients table
  # dataset_id = id for the associated dataset, generated in the datasets_TEMPLATE
  # id = id created in this script for each relationship

  dataset_id <- synapse_csv_id_to_tbl(syn, "") %>% #update with synapse ID for the dataset file
    dplyr::pull(id)

  samples <- synapse_csv_id_to_tbl(syn, "") %>%
    dplyr::select(
      "name",
      "sample_id" = "id"
    )

  datasets_to_samples <- samples %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    datasets_to_samples,
    "", #UPDATE
    "datasets_to_samples"
  )
}
