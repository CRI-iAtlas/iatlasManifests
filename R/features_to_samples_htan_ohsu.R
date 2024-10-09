features_to_samples_htan_ohsu <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #biospecimen file
  ohsu <- "syn39141309"

  patient_age <- synapse_csv_id_to_tbl(syn, "syn63600274") %>%
    dplyr::rename("patient_id" = "id") %>%
    dplyr::mutate(
      "OS" = 0,
      "OS_time" = 128.9
    ) %>%
    dplyr::select(-"name")

  samples <- synapse_csv_id_to_tbl(syn, "syn63600384") %>%
    dplyr::inner_join(patient_age, by = "patient_id") %>%
    dplyr::select("sample_name" = "name",
                  "sample_id" = "id",
                  "age_at_diagnosis",
                  "OS",
                  "OS_time"
    )

  features <-
    synapse_csv_id_to_tbl(syn, "syn51613666") %>%
    dplyr::select(
      "feature_name" =  "name",
      "feature_id" = "id"
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn63600423") %>%
        dplyr::select(
          "feature_name" =  "name",
          "feature_id" = "id"
        )
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn50944340") %>%
        dplyr::select(
          "feature_name" =  "name",
          "feature_id" = "id"
        ) #add features in TCGA table
    )

  TIDE_df <-
    synapse_tsv_id_to_tbl(syn, "syn63542972") %>%
    dplyr::mutate(
      "sample_name" = substr(`...1`, 26, 40)
    ) %>%
    dplyr::select(
      "sample_name",
      "TIDE"
    )

  #computing Timepoint_Relative_Order

  timepoint_order_df <-
    read.csv(paste("inst/",ohsu, ".csv", sep = "")) %>%
    dplyr::filter(HTAN.Biospecimen.ID %in% samples$sample_name) %>%
    dplyr::mutate(
      "Timepoint_Relative_Order" = dplyr::dense_rank(Collection.Days.from.Index)
    ) %>%
    dplyr::select(
      "sample_name" = "HTAN.Biospecimen.ID",
      "Timepoint_Relative_Order"
    )

  features_iatlas <- synapse_csv_id_to_tbl(syn, "syn63542967") %>%
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
      "OS",
      "OS_time",
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
    "syn63600266",
    "features_to_samples"
  )
}



