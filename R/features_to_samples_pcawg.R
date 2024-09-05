features_to_samples_pcawg <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  features <-
    synapse_csv_id_to_tbl(syn, "syn51613666") %>%
    dplyr::select(
      "feature_name" =  "name",
      "feature_id" = "id"
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn50944340") %>%
        #dplyr::filter(.data$feature_class != "Clinical") %>%
        dplyr::select(
          "feature_name" =  "name",
          "feature_id" = "id"
        ) #add features in TCGA table
    )

  patient_clinical_data <-
    synapse_csv_id_to_tbl(syn, "syn51088152" ) %>%
    dplyr::select(
      "name",
      "age_at_diagnosis"
    ) %>%
    tidyr::pivot_longer(age_at_diagnosis, names_to = "feature_name", values_to = "feature_to_sample_value")

  samples_to_patients <-
    synapse_csv_id_to_tbl(syn, "syn51088163") %>%
    dplyr::select(
      "name",
      "sample_id" = "id"
    )

  features_to_samples <-
    samples_to_patients %>%
    dplyr::inner_join(patient_clinical_data, by = "name") %>%
    dplyr::inner_join(features, by = "feature_name") %>%
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
    "syn62013809",
    "features_to_samples"
  )
}



