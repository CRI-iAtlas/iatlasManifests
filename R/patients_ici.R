patients_ici <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #get age information for initial 15 ICI datasets
  ici_patients_age <-
    synapse_tsv_id_to_tbl(syn, "syn26560788") %>%
    dplyr::select(
      "name" = "Run_ID",
      "age_at_diagnosis" = "Age"
    ) %>%
    dplyr::bind_rows(
      synapse_tsv_id_to_tbl(syn, "syn46885846") %>%
        dplyr::select(
          "name" = "Run_ID",
          "age_at_diagnosis" = "Age"
        )
    ) %>%
    dplyr::inner_join(
      dplyr::bind_rows(
        synapse_feather_id_to_tbl(syn, "syn25981550"),
        synapse_feather_id_to_tbl(syn, "syn27790733")
        )
    ) %>%
    dplyr::select(
      "patient_barcode",
      "age_at_diagnosis",
    ) %>%
    dplyr::distinct()


  patients <-
    dplyr::bind_rows(
      synapse_feather_id_to_tbl(syn, "syn25981549"),
      synapse_feather_id_to_tbl(syn, "syn27790727")
    ) %>%
    dplyr::rename("name" = "barcode") %>%
    dplyr::inner_join(ici_patients_age, by = dplyr::join_by("name" == "patient_barcode")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "patients"
    )

  # synapse_store_table_as_csv(
  #   syn,
  #   patients,
  #   "syn51589363",
  #   "patients"
  # )

}



