single_cell_pseudobulk_features_krishna <- function(){

  require(magrittr)
  require(rlang)

  syn <- create_synapse_login()

  dataset_id <- synapse_csv_id_to_tbl(syn, "syn59195251") %>%
    dplyr::pull(id)

  pseudobulk_features <- synapse_csv_id_to_tbl(syn, "syn59473428") %>%
    tidyr::separate_wider_regex("...1", c(sample_name = ".*", "_", cell_type = ".*")) %>%
    dplyr::mutate(
      "sample_name" = paste0("Krishna_ccRCC_", sample_name)
    ) %>%
    tidyr::pivot_longer(-c("sample_name", "cell_type"), names_to = "feature_name", values_to =  "value") %>%
    dplyr::filter(!cell_type %in% c("Ambiguous", "Ambiguous/Dead", "TAM/TCR (Ambiguos)")) %>%
    dplyr::filter(value != "Inf")


  #getting ids
  tcga_features <- synapse_csv_id_to_tbl(syn, "syn50944340") %>%
    dplyr::filter(method_tag != "CIBERSORT") %>%
    dplyr::select(
      "feature_name" = "name",
      "feature_id" = "id"
    )

  features_to_add <- tcga_features[tcga_features$feature_name %in% pseudobulk_features$feature_name,]

  samples_ids <- synapse_csv_id_to_tbl(syn, "syn59204288")%>%
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
      "value"
    ) %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "single_cell_pseudobulk_features"
    )

  synapse_store_table_as_csv(
    syn,
    single_cell_pseudobulk_features,
    "syn58865002",
    "single_cell_pseudobulk_features"
  )


}
