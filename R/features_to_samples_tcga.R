samples_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  features <-
    synapse_csv_id_to_tbl(syn, "syn50944340") %>%
    dplyr::filter(.data$feature_class != "Clinical") %>%
    dplyr::select(
      "feature_name" =  "name",
      "feature_id" = "id"
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn50896891") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  features_to_samples <-
    synapse_feather_id_to_tbl(syn,  "syn22128019") %>%
    dplyr::rename_all(~stringr::str_replace_all(.x, "[\\.]", "_")) %>%
    dplyr::mutate("Tumor_fraction" = 1 - .data$Stromal_Fraction) %>%
    dplyr::rename("sample_name" = "ParticipantBarcode") %>%
    dplyr::select(dplyr::all_of(c("sample_name", features$feature_name))) %>%
    tidyr::pivot_longer(-"sample_name", names_to = "feature_name") %>%
    tidyr::drop_na() %>%
    dplyr::arrange(.data$feature_name, .data$sample_name) %>%
    dplyr::inner_join(features, by = "feature_name") %>%
    dplyr::inner_join(samples, by = "sample_name") %>%
    dplyr::select(
      "feature_id",
      "sample_id",
      "feature_to_sample_value" = "value"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "features_to_samples"
    )

  readr::write_csv(features_to_samples, "synapse_storage_manifest.csv", na = "")
}



