samples_to_features_anders <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  # columns in the features_to_samples table:
  # "feature_id" = id of the feature, from the features table,
  # "sample_id" = id of the sample, from the samples table,,
  # "feature_to_sample_value" = value of the sample for this feature

  features <- #there is no need to change this. Only add rows in case a new feature was included
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

  features_values <- synapse_csv_id_to_tbl(syn, "syn65887903")%>%
    dplyr::select(
      "sample_name",
      "age_at_diagnosis" = "patient_age_at_diagnosis",
      "PFI_time_1",
      "PFI_1",
      "OS_time",
      "OS"
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn65902354") %>%
    dplyr::inner_join(features_values, by = dplyr::join_by("name" == "sample_name")) %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id",
      "age_at_diagnosis",
      "PFI_time_1",
      "PFI_1",
      "OS_time",
      "OS"
    )

  TIDE_df <-
    synapse_tsv_id_to_tbl(syn, "syn65888206") %>% #update
    dplyr::select(
      "sample_name" = "...1",
      "TIDE"
    )

  features_iatlas <- synapse_csv_id_to_tbl(syn, "syn65888203") %>% #update
    dplyr::rename(
      "sample_name" = "Run_ID"
    )

  features_to_samples <-
    samples %>%
    dplyr::inner_join(features_iatlas, by = "sample_name") %>%
    dplyr::left_join(TIDE_df, by = "sample_name") %>%
    dplyr::select(
      "sample_id",
      "age_at_diagnosis",
      "PFI_time_1",
      "PFI_1",
      "OS_time",
      "OS",
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
    tidyr::pivot_longer(-(sample_id), names_to = "feature_name", values_to = "feature_to_sample_value") %>% #convert table to long format
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
    "syn65888283",
    "features_to_samples"
  )
}



