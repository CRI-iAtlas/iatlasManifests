samples_ici <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients <-
    synapse_csv_id_to_tbl(syn, "syn51589396") %>%
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  samples <-
    dplyr::bind_rows(
      synapse_feather_id_to_tbl(syn, "syn25981550"),
      synapse_feather_id_to_tbl(syn, "syn27790733")
    ) %>%
    dplyr::select(
      "name",
      "patient_name" = "patient_barcode"
    ) %>%
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select(-"patient_name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "samples"
    )

  synapse_store_table_as_csv(
    syn,
    samples,
    "syn51589455",
    "samples"
  )

}



