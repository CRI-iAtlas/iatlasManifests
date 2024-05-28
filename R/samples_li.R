samples_li <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients <-
    synapse_csv_id_to_tbl(syn, "syn60085481") %>%
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  samples <-
    synapse_tsv_id_to_tbl(syn, "syn60085493") %>%
    dplyr::select(
      "patient",
      "summaryDescription"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "name" = paste("Li_ccRCC", patient, summaryDescription, sep = "_"),
      "patient_name" = paste0("Li_ccRCC_", patient)
    ) %>%
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select(
      "name",
      "patient_id"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "samples"
    )

  samples$name <- gsub(" ", "_", samples$name)
  samples$name <- gsub("-", "_", samples$name)

  synapse_store_table_as_csv(
    syn,
    samples,
    "syn60085132",
    "samples"
  )

}



