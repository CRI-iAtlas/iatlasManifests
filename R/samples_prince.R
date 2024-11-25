samples_prince <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients <-
    synapse_csv_id_to_tbl(syn, "syn63331646") %>%
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  samples <-
      synapse_csv_id_to_tbl(syn, "syn52349216") %>%
    dplyr::select(
      "name" = "Run_ID",
      "patient_name" = "subject.id"
    ) %>%
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select(-"patient_name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    samples,
    "syn63327059",
    "samples"
  )

}
