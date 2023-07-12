tags_to_publications_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  publications <-
    synapse_csv_id_to_tbl(syn, "syn51080887") %>%
    dplyr::select(
      "publication_title",
      "publication_id" = "id"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51080176") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  tags_to_publications <-
    synapse_feather_id_to_tbl(syn, "syn23545806") %>%
    dplyr::select(
      "publication_title" = "title",
      "tag_name"
    ) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::inner_join(publications, by = "publication_title") %>%
    dplyr::select(-c("tag_name", "publication_title")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "publications_to_tags"
    )

  synapse_store_table_as_csv(
    syn,
    tags_to_publications,
    "syn51080939",
    "tags_to_publications"
  )

}

