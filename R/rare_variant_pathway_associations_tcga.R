rare_variant_pathway_associations_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  features <-
    synapse_csv_id_to_tbl(syn, "syn50944340") %>%
    dplyr::select(
      "feature" =  "name",
      "feature_id" = "id"
    )

  datasets <-
    synapse_csv_id_to_tbl(syn, "syn51132398") %>%
    dplyr::select(
      "dataset" = "name",
      "dataset_id" = "id"
    )

  rare_variant_pathway_associations <-

    synapse_feather_id_to_tbl(syn,  "syn24873873") %>% #REPLACE

    dplyr::inner_join(features, by = "feature") %>%
    dplyr::inner_join(datasets, by = "dataset") %>%
    dplyr::select(-c("feature", "dataset")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "rare_variant_association_pathways"
    )

  synapse_store_table_as_csv(
    syn,
    rare_variant_pathway_associations,
    "syn51440832",
    "rare_variant_pathway_associations"
  )

}

