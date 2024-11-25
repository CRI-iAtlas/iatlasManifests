samples_shiao <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


  patients <- synapse_csv_id_to_tbl(syn, "syn58416567") %>%
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  samples <-  synapse_csv_id_to_tbl(syn, "syn55273095") %>%
    dplyr::select(cohort,
                  patient_treatment) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      name = paste0("Shiao_BRCA_", patient_treatment),
      patient_name = paste0("Shiao_BRCA_", gsub("T[[:digit:]]$", "", cohort))
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

  synapse_store_table_as_csv(
    syn,
    samples,
    "syn58417420",
    "samples"
  )

}
