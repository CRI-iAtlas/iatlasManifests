features_to_samples_prince <- function(){

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

  patient_age <- synapse_csv_id_to_tbl(syn, "syn63331646") %>%
    dplyr::rename("patient_id" = "id") %>%
    dplyr::select(-"name")

  samples <-
    synapse_csv_id_to_tbl(syn, "syn63332022") %>%
    dplyr::inner_join(patient_tags, by = "patient_id") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id",
      "age_at_diagnosis"
    )

  TIDE_df <-
    synapse_tsv_id_to_tbl(syn, "syn63296609") %>%
    dplyr::select(
      "sample_name" = "...1",
      "TIDE"
    )

  features_to_samples <-
    TIDE_df %>%
    dplyr::inner_join(synapse_tsv_id_to_tbl(syn, "syn63194040"), by = dplyr::join_by("sample_name" == "Run_ID")) %>%
    dplyr::select(
      "sample_name",
      "TIDE",
      "Module3_IFN_score" = "Module3_IFN_Score",
      "TGFB_score_21050467" = "TGFB_Score",
      "CHANG_CORE_SERUM_RESPONSE_UP" = "Chang_Serum_Response_Up",
      "CSF1_response" = "CSF1_Response",
      "LIexpression_score" = "LIexpression_Score",
      "Th1_cells" = "Bindea_Th1_Cells",
      "Th2_cells" = "Bindea_Th2_Cells",
      "Th17_cells" = "Bindea_Th17_Cells",
      "Vincent_IPRES_NonResponder",
      "Miracle",
      "Cytolytic_Score",
      "IMPRES" = "IMPRES_Score"
    ) %>%
    dplyr::inner_join(samples, by = "sample_name") %>%
    tidyr::pivot_longer(-c(sample_name, sample_id), names_to = "feature_name", values_to = "feature_to_sample_value") %>%
    dplyr::filter(!is.na(feature_to_sample_value)) %>%
    dplyr::inner_join(features, by = "feature_name") %>%
    dplyr::select(
      "feature_id",
      "sample_id",
      "feature_to_sample_value"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
    )


  synapse_store_table_as_csv(
    syn,
    features_to_samples,
    "syn63327061",
    "features_to_samples"
  )
}



