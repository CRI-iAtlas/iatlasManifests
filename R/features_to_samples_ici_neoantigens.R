features_to_samples_ici_neoantigens <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  features <-
    synapse_csv_id_to_tbl(syn, "syn51666407") %>%
    dplyr::select(
      "feature_name" =  "name",
      "feature_id" = "id"
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn51589463") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  features_to_samples <-
    synapse_feather_id_to_tbl(syn, "syn51658035") %>%
    dplyr::select(
      "feature_name",
      "feature_value",
      "sample_name"
    ) %>%
    dplyr::inner_join(features, by = "feature_name") %>%
    dplyr::inner_join(samples, by = "sample_name") %>%
    dplyr::select(
      "feature_id",
      "sample_id",
      "feature_to_sample_value" = "feature_value"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "features_to_samples"
    )

  synapse_store_table_as_csv(
    syn,
    features_to_samples,
    "syn51664494",
    "features_to_samples"
  )

}
