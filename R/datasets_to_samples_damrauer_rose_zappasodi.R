datasets_to_samples_damrauer_rose_zappasodi <- function(){ #UPDATE function name

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

  dataset_id <- synapse_csv_id_to_tbl(syn, "syn66227471")%>%
    dplyr::select(
      "dataset_name" = "name",
      "dataset_id" = "id"
    )

  samples <- synapse_csv_id_to_tbl(syn, "syn66227595") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  samples_datasets <-
    synapse_csv_id_to_tbl(syn, "syn65986772") %>%
    dplyr::mutate(
      "dataset_name" = "Damrauer_NatComm_2022") %>%
    dplyr::add_row(dplyr::mutate(synapse_csv_id_to_tbl(syn, "syn65986580"),
                                 "dataset_name" = "Rose_BrJCancer_2021")) %>%
    dplyr::add_row(dplyr::mutate(synapse_csv_id_to_tbl(syn, "syn65941750"),
                                 "dataset_name" = "Zappasodi_Nature_2021")) %>%
    dplyr::select("dataset_name", "sample_name")

  datasets_to_samples <- samples_datasets %>%
    dplyr::inner_join(dataset_id, by = "dataset_name") %>%
    dplyr::inner_join(samples, by = "sample_name") %>%
    dplyr::select("sample_id", "dataset_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    datasets_to_samples,
    "syn66227462",
    "datasets_to_samples"
  )
}
