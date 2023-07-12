patients_pcawg <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients <-
    synapse_csv_id_to_tbl(syn, "syn51088152") %>%
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  samples <-
    synapse_tsv_id_to_tbl(syn, "syn21785582") %>%
    dplyr::select("name" = "icgc_donor_id") %>%
    dplyr::mutate("patient_name" = .data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "samples"
    ) %>%
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select(-"patient_name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "samples"
    )

  readr::write_csv(samples, "synapse_storage_manifest.csv", na = "")

}
