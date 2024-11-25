samples_to_features_bi <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patient_info <-
    syn$get("syn60531525") %>%
    purrr::pluck("path") %>%
    openxlsx::read.xlsx(., sheet = 2) %>%
    dplyr::as_tibble()

  colnames(patient_info) <- patient_info[1,]
  patient_info <- patient_info[-1,]


  patients_age <- patient_info %>%
    dplyr::select(
      "Sample",
      "age_at_diagnosis" = "Age at Dx"
    ) %>%
    dplyr::mutate(
      "sample_name" = paste0("Bi_ccRCC_", Sample),
      "age_at_diagnosis" = trunc(as.numeric(age_at_diagnosis))
    ) %>%
    dplyr::select(sample_name, age_at_diagnosis) %>%
    tidyr::pivot_longer(age_at_diagnosis, names_to = "feature_name", values_to = "feature_to_sample_value")

  samples <-
    synapse_csv_id_to_tbl(syn, "syn60531805") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  features <- synapse_csv_id_to_tbl(syn, "syn50944340") %>%
    dplyr::select(
      "feature_name" = "name",
      "feature_id" = "id"
    )

  features_to_samples <-patients_age %>%
    dplyr::inner_join(features, by = dplyr::join_by("feature_name")) %>%
    dplyr::inner_join(samples, by = dplyr::join_by("sample_name")) %>%
    dplyr::filter(!is.na(feature_to_sample_value)) %>%
    dplyr::select(
      "feature_id",
      "sample_id",
      "feature_to_sample_value"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    features_to_samples,
    "syn62014113",
    "features_to_samples"
  )
}



