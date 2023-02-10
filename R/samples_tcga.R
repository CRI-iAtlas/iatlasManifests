samples_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients <-
    synapse_csv_id_to_tbl(syn, "syn50932269") %>%
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  samples <-
    synapse_feather_id_to_tbl(syn, "syn22128019") %>%
    dplyr::select(
      "name" = "ParticipantBarcode",
      "patient_name" = "ParticipantBarcode"
    ) %>%
    dplyr::right_join(patients, by = "patient_name") %>%
    dplyr::select(-"patient_name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "samples"
    )

  readr::write_csv(samples, "synapse_storage_manifest.csv")
}



