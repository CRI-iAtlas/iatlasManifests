publications_to_tags_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  publications <-
    synapse_csv_id_to_tbl(syn, "syn51080887") %>%
    dplyr::select(
      "publication_name" = "name",
      "publication_id" = "id"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51080176") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  publications_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn23545806") %>%
    dplyr::select(
      "publication_name" = "title",
      "tag_name"
    ) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::inner_join(publications, by = "publication_name") %>%
    dplyr::select(-c("tag_name", "publication_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "publications_to_tags"
    )

  readr::write_csv(publications_to_tags, "synapse_storage_manifest.csv")

}

