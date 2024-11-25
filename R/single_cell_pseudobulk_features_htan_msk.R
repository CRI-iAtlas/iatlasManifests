single_cell_pseudobulk_features_htan_msk <- function(){

  require(magrittr)
  require(rlang)

  syn <- create_synapse_login()

  pseudobulk_features <- synapse_tsv_id_to_tbl(syn, "syn53708308") %>%
    dplyr::mutate(
      "feature_name" = gsub("\\.", "_", .data$feature_name)
    )

  #getting ids
  tcga_features <- synapse_csv_id_to_tbl(syn, "syn50944340") %>%
    dplyr::filter(method_tag != "CIBERSORT") %>%
    dplyr::select(
      "feature_name" = "name",
      "feature_id" = "id"
    )

  features_to_add <- tcga_features[tcga_features$feature_name %in% pseudobulk_features$feature_name,]


  samples_ids <- synapse_csv_id_to_tbl(syn, "syn53678348")%>%
    dplyr::select(
      "sample_name"  = "name",
      "sample_id" = "id"
    )

  #single_cell_pseudobulk_features(id, sample_id, cell_type, feature_id, value)
  single_cell_pseudobulk_features <- pseudobulk_features %>%
    dplyr::inner_join(samples_ids, by = "sample_name") %>%
    dplyr::inner_join(features_to_add, by = "feature_name") %>%
    dplyr::select(
      "sample_id",
      "cell_type",
      "feature_id",
      "value" = feature_value
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "single_cell_pseudobulk_features"
    )

  synapse_store_table_as_csv(
    syn,
    single_cell_pseudobulk_features,
    "syn53708370",
    "single_cell_pseudobulk_features"
  )


}
