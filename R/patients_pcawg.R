patients_pcawg <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patient_clinical_data <-
    synapse_excel_id_to_tbl(syn, "syn24827063" ) %>%
    dplyr::select(
      "name" = "icgc_donor_id",
      "gender" = "donor_sex",
      "age_at_diagnosis" = "donor_age_at_diagnosis"
    )

  patients <-
    synapse_tsv_id_to_tbl(syn, "syn21785582") %>%
    dplyr::select("name" = "icgc_donor_id") %>%
    dplyr::left_join(patient_clinical_data, by = "name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "patients"
    )


  readr::write_csv(patients, "synapse_storage_manifest.csv", na = "")

}
