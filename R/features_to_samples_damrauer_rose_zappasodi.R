samples_to_features_damrauer_rose_zappasodi <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  # columns in the features_to_samples table:
  # "feature_id" = id of the feature, from the features table,
  # "sample_id" = id of the sample, from the samples table,,
  # "feature_to_sample_value" = value of the sample for this feature

  features <- #there is no need to change this_ Only add rows in case a new feature was included
    synapse_csv_id_to_tbl(syn, "syn51613666") %>%
    dplyr::select(
      "feature_name" =  "name",
      "feature_id" = "id"
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn50944340") %>%
        #dplyr::filter(_data$feature_class != "Clinical") %>%
        dplyr::select(
          "feature_name" =  "name",
          "feature_id" = "id"
        ) #add features in TCGA table
    )

  cibersort <- synapse_tsv_id_to_tbl(syn, "syn66227434") %>%
    dplyr::rename_with(~(gsub(" ", "_", .x, fixed = TRUE))) %>%
    dplyr::rename(
      `T_cells_regulatory_Tregs` = `T_cells_regulatory_(Tregs)`,
      "sample_name" = "Mixture") %>%
    dplyr::mutate( #compute the aggregates
        #Aggregate1
        Lymphocytes_Aggregate1 = B_cells_naive + B_cells_memory + T_cells_CD4_naive+ T_cells_CD4_memory_resting + T_cells_CD4_memory_activated +
          T_cells_follicular_helper + T_cells_regulatory_Tregs + T_cells_gamma_delta + T_cells_CD8 + NK_cells_resting + NK_cells_activated + Plasma_cells,
        Macrophage_Aggregate1 = Monocytes + Macrophages_M0 + Macrophages_M1 + Macrophages_M2,
        Dendritic_cells_Aggregate1 = Dendritic_cells_resting + Dendritic_cells_activated,
        Mast_cells_Aggregate1 = Mast_cells_resting + Mast_cells_activated,
        Neutrophils_Aggregate1 = Neutrophils,
        Eosinophils_Aggregate1 = Eosinophils,
        #Aggregate2
        T_cells_CD8_Aggregate2 = T_cells_CD8,
        T_cells_CD4_Aggregate2 = T_cells_CD4_naive+ T_cells_CD4_memory_resting + T_cells_CD4_memory_activated,
        B_cells_Aggregate2 = B_cells_naive + B_cells_memory,
        NK_cells_Aggregate2 = NK_cells_resting + NK_cells_activated,
        Macrophage_Aggregate2 = Macrophages_M0 + Macrophages_M1 + Macrophages_M2,
        Dendritic_cells_Aggregate2 = Dendritic_cells_resting + Dendritic_cells_activated,
        Mast_cells_Aggregate2 = Mast_cells_resting + Mast_cells_activated,
        Neutrophils_Aggregate2 = Neutrophils,
        Eosinophils_Aggregate2 = Eosinophils,
        #Aggregate3
        T_cells_CD8_Aggregate3 = T_cells_CD8,
        T_cells_CD4_Aggregate3 = T_cells_CD4_naive+ T_cells_CD4_memory_resting + T_cells_CD4_memory_activated + T_cells_follicular_helper + T_cells_regulatory_Tregs,
        T_cells_gamma_delta_Aggregate3 = T_cells_gamma_delta,
        B_cells_Aggregate3 = B_cells_naive + B_cells_memory,
        NK_cells_Aggregate3 = NK_cells_resting + NK_cells_activated,
        Plasma_cells_Aggregate3 = Plasma_cells,
        Macrophage_Aggregate3 = Monocytes + Macrophages_M0 + Macrophages_M1 + Macrophages_M2,
        Dendritic_cells_Aggregate3 = Dendritic_cells_resting + Dendritic_cells_activated,
        Mast_cells_Aggregate3 = Mast_cells_resting + Mast_cells_activated,
        Neutrophils_Aggregate3 = Neutrophils,
        Eosinophils_Aggregate3 = Eosinophils
      )

  clinical_df <- synapse_tsv_id_to_tbl(syn, "syn66227433") %>%
    dplyr::select(
      "sample_name" = "Run_ID",
      "OS" = "OS_e",
      "OS_time" = "OS_d",
      "PFI_1" = "PFS_e",
      "PFI_time_1" = "PFS_d",
      "age_at_diagnosis" = "Age"
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn66227595") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  TIDE_df <-
    synapse_tsv_id_to_tbl(syn, "syn67716488") %>%
    dplyr::select(
      "sample_name" = "...1",
      "TIDE"
    )

  features_iatlas <- synapse_tsv_id_to_tbl(syn, "syn66227435") %>%
    dplyr::select(
      "sample_name" = "Run_ID",
      "Module3_IFN_score" = "Module3_IFN_Score",
      "TGFB_score_21050467" = "TGFB_Score",
      "CHANG_CORE_SERUM_RESPONSE_UP" = "Chang_Serum_Response_Up",
      "CSF1_response" = "CSF1_Response",
      "LIexpression_score" = "LIexpression_Score",
      "Th1_cells" = "Bindea_Th1_Cells",
      "Th2_cells" = "Bindea_Th2_Cells",
      "Th17_cells" = "Bindea_Th17_Cells",
      "Vincent_IPRES_NonResponder"
    )

  features_to_samples <-
    samples %>%
    dplyr::inner_join(features_iatlas, by = "sample_name") %>%
    dplyr::left_join(TIDE_df, by = "sample_name") %>%
    dplyr::inner_join(clinical_df, by = "sample_name") %>%
    dplyr::inner_join(cibersort, by = "sample_name") %>%
    tidyr::pivot_longer(-c(sample_name, sample_id), names_to = "feature_name", values_to = "feature_to_sample_value") %>% #convert table to long format
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
    "syn66227463",
    "features_to_samples"
  )
}


