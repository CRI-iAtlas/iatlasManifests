samples_to_features_krishna <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  os_data <-
    syn$get("syn59202673") %>%
    purrr::pluck("path") %>%
    openxlsx::read.xlsx(., sheet = 1) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      "sample_name" = trimws(paste0("Krishna_ccRCC_", sample_name))
    ) %>%
    dplyr::select("sample_name", "OS", "OS_time") %>%
    tidyr::pivot_longer(-sample_name, names_to = "feature_name", values_to = "feature_to_sample_value")

  samples <-
    synapse_csv_id_to_tbl(syn, "syn59204288") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  features <- synapse_csv_id_to_tbl(syn, "syn50944340") %>%
    dplyr::select(
      "feature_name" = "name",
      "feature_id" = "id"
    )

  features_to_samples <-os_data %>%
    dplyr::inner_join(features, by = dplyr::join_by("feature_name")) %>%
    dplyr::inner_join(samples, by = dplyr::join_by("sample_name")) %>%
    dplyr::filter(!is.na(feature_to_sample_value)) %>%
    dplyr::select(
      "feature_id",
      "sample_id",
      "feature_to_sample_value"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "features_to_samples"
    )


  synapse_store_table_as_csv(
    syn,
    features_to_samples,
    "syn59473632",
    "features_to_samples"
  )
}



