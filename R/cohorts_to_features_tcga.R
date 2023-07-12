cohorts_to_features_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cohorts_to_samples <-
    synapse_csv_id_to_tbl(syn, "syn51537400") %>%
    dplyr::select("cohort_id", "sample_id")

  features_to_samples <-
    synapse_csv_id_to_tbl(syn, "syn51440935") %>%
    dplyr::select("feature_id", "sample_id")

  cohorts_to_features <-
    dplyr::inner_join(
      cohorts_to_samples,
      features_to_samples,
      by = "sample_id"
    ) %>%
    dplyr::select("cohort_id", "feature_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cohorts_to_features"
    )

  synapse_store_table_as_csv(
    syn,
    cohorts_to_features,
    "syn51537456",
    "cohorts_to_features"
  )

}

