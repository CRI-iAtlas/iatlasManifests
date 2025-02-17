samples_to_features_amadeus <- function(){

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

  surv_data <- synapse_csv_id_to_tbl(syn, "syn54074560") %>%
    dplyr::mutate(
      "PFI_time_1" = `PFS (mo)` * 30,
      "OS_time" = `OS (mo)` * 30,
      "name" = paste0("AMADEUS_",
                      ifelse(`Subject ID` == "104-0013",
                      "104-0022",
                      `Subject ID`
                      ))
    ) %>%
    dplyr::select(
      "name",
      "PFI_time_1",
      "PFI_1" = "PFS Event Flag",
      "OS_time",
      "OS" = "OS Event Flag"
    )

  patient_age <- synapse_csv_id_to_tbl(syn, "syn64290704") %>%
    dplyr::rename("patient_id" = "id") %>%
    dplyr::inner_join(surv_data, by = "name") %>%
    dplyr::select(-"name")

  samples <-
    synapse_csv_id_to_tbl(syn, "syn64290688") %>%
    dplyr::inner_join(patient_age, by = "patient_id") %>%
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
    synapse_tsv_id_to_tbl(syn, "syn64154321") %>%
    dplyr::select(
      "sample_name" = "...1",
      "TIDE"
    )

  features_iatlas <- synapse_csv_id_to_tbl(syn, "syn64154238") %>%
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
    "syn64156732",
    "features_to_samples"
  )
}



