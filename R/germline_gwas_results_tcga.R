germline_gwas_results_tcga <- function(){

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

  snps <-
    synapse_csv_id_to_tbl(syn, "syn51440489") %>%
    dplyr::select(
      "snp" = "name",
      "snp_id" = "id"
    )

  germline_gwas_results <-

    synapse_feather_id_to_tbl(syn,  "syn24202042") %>% #REPLACE

    dplyr::inner_join(features, by = "feature") %>%
    dplyr::inner_join(datasets, by = "dataset") %>%
    dplyr::inner_join(snps, by = "snp") %>%
    dplyr::select(-c("feature", "dataset", "snp")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "germline_gwas_results"
    )

  synapse_store_table_as_csv(
    syn,
    germline_gwas_results,
    "syn51440944",
    "germline_gwas_results"
  )

}

