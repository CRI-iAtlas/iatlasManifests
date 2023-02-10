tags_to_tags_tcga <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51080176") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  tags_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn23545186") %>%
    dplyr::select("tag_name" = "tag", "parent_tag_name" = "related_tag") %>%
    dplyr::inner_join(tags, by = c("parent_tag_name" = "tag_name")) %>%
    dplyr::rename("parent_tag_id" = "tag_id") %>%
    dplyr::select(-"parent_tag_name") %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::select(-"tag_name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags_to_tags"
    )



  readr::write_csv(tags_to_tags, "synapse_storage_manifest.csv")

}
