samples_porter <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients <-
    synapse_csv_id_to_tbl(syn, "") %>% #UPDATE
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  samples <-
    synapse_tsv_id_to_tbl(syn, "syn63607726") %>%
    dplyr::mutate(
      "name" = paste(Dataset, Patient_Name, Run_Name, sep= "_"),
    ) %>%
    dplyr::select(
      "name",
      "patient_name" = "Patient_Name"
    ) %>%
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select(-"patient_name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    samples,
    "", #UPDATE
    "samples"
  )

}



