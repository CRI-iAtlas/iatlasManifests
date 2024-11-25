samples_to_features_htan_msk <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  samples_to_patients <- read.csv("inst/syn39256250.csv") %>%
    dplyr::select(
      "sample_name" = "HTAN.Biospecimen.ID",
      "patient_name" = "HTAN.Parent.ID"
    )

  patients_age <- read.csv("inst/syn39254633.csv") %>%
    dplyr::select(
      "patient_name" = "HTAN.Participant.ID",
      "age_at_diagnosis" = "Age.at.Diagnosis"
    ) %>%
    dplyr::inner_join(samples_to_patients, by = "patient_name") %>%
    dplyr::mutate(
      "age_at_diagnosis" = floor(.data$age_at_diagnosis/365)
    ) %>%
    tidyr::pivot_longer(age_at_diagnosis, names_to = "feature_name", values_to = "feature_to_sample_value")

  samples <-
    synapse_csv_id_to_tbl(syn, "syn53678348") %>%
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
    "syn62014178",
    "features_to_samples"
  )
}



