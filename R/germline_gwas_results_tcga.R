germline_gwas_results_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  features <-
    synapse_csv_id_to_tbl(syn, "syn50944340") %>%
    dplyr::select(
      "feature" =  "name",
      "feature_id" = "id"
    ) %>%
    dplyr::bind_rows(
      synapse_csv_id_to_tbl(syn, "syn51588927") %>%
        dplyr::select(
          "feature" =  "name",
          "feature_id" = "id"
        )
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

  original <- synapse_feather_id_to_tbl(syn,  "syn24202042") #REPLACE

  germline_gwas_results <-
    original %>%
    dplyr::inner_join(features, by = "feature") %>%
    dplyr::inner_join(datasets, by = "dataset") %>%
    dplyr::inner_join(snps, by = "snp")

  # A few SNPs were not included in this table, as they are with a mismatch in the alleles in the bp annotation

  missing_snps <- original[which(!original$snp %in% germline_gwas_results$snp),] %>%
    dplyr::mutate(
      snp = paste0(stringi::stri_sub(snp, 1, -4), stringi::stri_reverse(stringi::stri_sub(snp,-3,-1)))
    ) %>%
    dplyr::inner_join(features, by = "feature") %>%
    dplyr::inner_join(datasets, by = "dataset") %>%
    dplyr::inner_join(snps, by = "snp")


  germline_gwas_results <-
    germline_gwas_results %>%
      dplyr::bind_rows(missing_snps) %>%
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

