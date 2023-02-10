datasets_to_tags_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51080176") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  datasets <-
    synapse_csv_id_to_tbl(syn, "syn51080455") %>%
    dplyr::select(
      "dataset_name" = "name",
      "dataset_id" = "id"
    )

  datasets_to_tags <-
    dplyr::tribble(
      ~tag_name,        ~dataset_name,
      "Immune_Subtype", "TCGA",
      "TCGA_Subtype",   "TCGA",
      "TCGA_Study",     "TCGA"
    ) %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::inner_join(datasets, by = "dataset_name") %>%
    dplyr::select(-c("tag_name", "dataset_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets_to_tags"
    )

  readr::write_csv(datasets_to_tags, "synapse_storage_manifest.csv")

}

