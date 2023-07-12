tags_to_tags_pcawg <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  pcawg_tags <-
    synapse_csv_id_to_tbl(syn, "syn51088201") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  related_tag <- pcawg_tags %>%
    dplyr::filter(.data$tag_name == "PCAWG_Study") %>%
    dplyr::rename(
      "related_tag_name" = "tag_name",
      "related_tag_id" = "tag_id"
    )

  tags_to_tags <- pcawg_tags %>%
    dplyr::filter(.data$tag_name != "PCAWG_Study") %>%
    dplyr::mutate("related_tag_name" = "PCAWG_Study") %>%
    dplyr::inner_join(related_tag, by = "related_tag_name") %>%
    dplyr::select(-c("tag_name", "related_tag_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags_to_tags"
    )

  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "syn51093294",
    "tags_to_tags"
  )

}
