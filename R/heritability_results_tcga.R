heritability_results_tcga <- function(){

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

  heritability_results <-

    synapse_feather_id_to_tbl(syn,  "syn23651688") %>% #REPLACE

    dplyr::inner_join(features, by = "feature") %>%
    dplyr::inner_join(datasets, by = "dataset") %>%
    dplyr::select(-c("feature", "dataset")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "heritability_results"
    )

  readr::write_csv(
    heritability_results,
    "synapse_storage_manifest.csv",
    na = ""
  )

}

