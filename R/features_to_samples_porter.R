samples_to_features_porter <- function(){

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

  patient_age <- synapse_csv_id_to_tbl(syn, "") %>% #UPDATE
    dplyr::rename("patient_id" = "id") %>%
    dplyr::select(-"name")

  samples <-
    synapse_csv_id_to_tbl(syn, "") %>% #UPDATE
    dplyr::inner_join(patient_tags, by = "patient_id") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id",
      "age_at_diagnosis"
    )

  TIDE_df <-
    synapse_tsv_id_to_tbl(syn, "syn63562158") %>%
    dplyr::select(
      "sample_name" = "...1",
      "TIDE"
    )

  features_iatlas <- synapse_csv_id_to_tbl(syn, "syn63562155") %>%
    dplyr::mutate(
      "sample_name" = substr(Run_ID, 26, 40)
    ) %>%
    dplyr::select(-"Run_ID")


  features_to_samples <-
    samples %>%
    dplyr::inner_join(features_iatlas, by = "sample_name") %>%
    dplyr::inner_join(timepoint_order_df, by = "sample_name") %>%
    dplyr::left_join(TIDE_df, by = "sample_name") %>%
    dplyr::select(
      "sample_id",
      "age_at_diagnosis",
      "Timepoint_Relative_Order",
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
    tidyr::pivot_longer(-(sample_id), names_to = "feature_name", values_to = "feature_to_sample_value") %>%
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
    "", #UPDATE
    "features_to_samples"
  )
}



