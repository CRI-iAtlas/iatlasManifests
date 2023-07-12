slides_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients <-
    synapse_csv_id_to_tbl(syn, "syn50932269") %>%
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  slides <-
    synapse_feather_id_to_tbl(syn, "syn22128019") %>%
    dplyr::select(
      "name" = "Slide",
      "patient_name" = "ParticipantBarcode"
    ) %>%
    tidyr::drop_na() %>%
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select(-"patient_name") %>%
    dplyr::mutate(
      "display" = .data$name
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "slides"
    )

  readr::write_csv(slides, "synapse_storage_manifest.csv", na = "")
}



