samples_porter <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients <-
    synapse_csv_id_to_tbl(syn, "syn63623064") %>%
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )
  samples_features <- synapse_csv_id_to_tbl(syn, "syn63562155")
  samples <-
    synapse_tsv_id_to_tbl(syn, "syn63607726") %>% #manifest file, to get patient - sample relationship
    dplyr::mutate(
      "name" = paste(Dataset, Patient_Name, Run_Name, sep= "-"),
      "patient_name" = ifelse(grepl("^PICI0033-", Patient_Name),
                              Patient_Name,
                              paste0("PICI0033-", Patient_Name))
    ) %>%
    dplyr::select(
      "name",
      "patient_name"
    ) %>%
    dplyr::inner_join(samples_features, by = dplyr::join_by("name" == "Run_ID")) %>% #get the ids of the samples processed by LENS
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select("name", "patient_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    samples,
    "syn63623051",
    "samples"
  )

}



