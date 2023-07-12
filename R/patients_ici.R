patients_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients <-
    dplyr::bind_rows(
      synapse_feather_id_to_tbl(syn, "syn25981549"),
      synapse_feather_id_to_tbl(syn, "syn27790727")
    ) %>%
    dplyr::rename("name" = "barcode") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "patients"
    )

  synapse_store_table_as_csv(
    syn,
    patients,
    "syn51589363",
    "patients"
  )

}



