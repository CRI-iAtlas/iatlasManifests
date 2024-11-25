samples_bi <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


  patients <- synapse_csv_id_to_tbl(syn, "syn60531770") %>%
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  # this dataset has only one sample per patient, so we will use the patient table again
  samples <-  patients %>%
    dplyr::mutate(
      name = patient_name
    ) %>%
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
    "syn60530061",
    "samples"
  )

}
