samples_to_features_shiao <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patient_info <- synapse_csv_id_to_tbl(syn, "syn58416567") %>%
    dplyr::select(
      "patient_name" = "name",
      "age_at_diagnosis"
    ) %>%
    tidyr::pivot_longer(age_at_diagnosis, names_to = "feature_name", values_to = "feature_to_sample_value")


  samples <-
    synapse_csv_id_to_tbl(syn, "syn58433237") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  features <- synapse_csv_id_to_tbl(syn, "syn50944340") %>%
    dplyr::select(
      "feature_name" = "name",
      "feature_id" = "id"
    )

  features_to_samples <- synapse_csv_id_to_tbl(syn, "syn55273095") %>%
    dplyr::select(cohort,
                  patient_treatment) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      sample_name = paste0("Shiao_BRCA_", patient_treatment),
      patient_name = paste0("Shiao_BRCA_", gsub("T[[:digit:]]$", "", cohort))
    ) %>%
    dplyr::inner_join(patient_info, by = "patient_name")  %>%
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
    "syn62014284",
    "features_to_samples"
  )
}



