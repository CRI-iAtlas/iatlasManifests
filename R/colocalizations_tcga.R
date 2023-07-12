colocalizations_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  features <-
    dplyr::bind_rows(
      synapse_csv_id_to_tbl(syn, "syn50944340"),
      synapse_csv_id_to_tbl(syn, "syn51588927")
    ) %>%
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

  coloc_datasets <- datasets %>%
    dplyr::rename(
      "coloc_dataset" = "dataset",
      "coloc_dataset_id" = "dataset_id"
    )

  snps <-
    synapse_csv_id_to_tbl(syn, "syn51440489") %>%
    dplyr::select(
      "snp" = "name",
      "snp_id" = "id"
    )

  genes <-
    synapse_csv_id_to_tbl(syn, "syn50896922") %>%
    dplyr::select(
      "entrez" = "entrez_id",
      "gene_id" = "id"
    )

  colocalizations <-

    synapse_feather_id_to_tbl(syn,  "syn24873864") %>% #REPLACE

    dplyr::rename("link" = "plot_link") %>%
    dplyr::inner_join(features, by = "feature") %>%
    dplyr::inner_join(datasets, by = "dataset") %>%
    dplyr::inner_join(coloc_datasets, by = "coloc_dataset") %>%
    dplyr::inner_join(snps, by = "snp") %>%
    dplyr::inner_join(genes, by = "entrez") %>%
    dplyr::select(-c("feature", "dataset", "coloc_dataset", "snp", "entrez")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "colocalizations"
    )

  synapse_store_table_as_csv(
    syn,
    colocalizations,
    "syn51440958",
    "colocalizations"
  )

}

