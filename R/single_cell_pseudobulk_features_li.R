single_cell_pseudobulk_features_li <- function(){

  require(magrittr)
  require(rlang)

  syn <- create_synapse_login()

  dataset_id <- synapse_csv_id_to_tbl(syn, "syn60085138") %>%
    dplyr::pull(id)

  samples_ids <- synapse_csv_id_to_tbl(syn, "syn60085711")%>%
    dplyr::select(
      "sample_name"  = "name",
      "sample_id" = "id"
    ) %>%
    dplyr::mutate(
      "sample_key" = gsub(".*_(.*)", "\\1", sample_name)
    )

  pseudobulk_features <- synapse_csv_id_to_tbl(syn, "syn59883016") %>%
    tidyr::separate_wider_regex("...1", c(sample_key = ".*", "_", cell_type = ".*")) %>%
    tidyr::pivot_longer(-c("sample_key", "cell_type"), names_to = "feature_name", values_to =  "value")

  #getting ids
  tcga_features <- synapse_csv_id_to_tbl(syn, "syn50944340") %>%
    dplyr::filter(method_tag != "CIBERSORT") %>%
    dplyr::select(
      "feature_name" = "name",
      "feature_id" = "id"
    )

  features_to_add <- tcga_features[tcga_features$feature_name %in% pseudobulk_features$feature_name,]



  #single_cell_pseudobulk_features(id, sample_id, cell_type, feature_id, value)
  single_cell_pseudobulk_features <- pseudobulk_features %>%
    dplyr::inner_join(samples_ids, by = "sample_key") %>%
    dplyr::inner_join(features_to_add, by = "feature_name") %>%
    dplyr::select(
      "sample_id",
      "cell_type",
      "feature_id",
      "value"
    ) %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    single_cell_pseudobulk_features,
    "syn60120859",
    "single_cell_pseudobulk_features"
  )


}
