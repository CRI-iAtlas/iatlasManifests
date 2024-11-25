tags_to_tags_ici <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51613683") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  tags_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn25999170")%>% #replace
    dplyr::select("tag_name" = "tag", "related_tag_name" = "related_tag") %>%
    dplyr::inner_join(tags, by = c("related_tag_name" = "tag_name")) %>%
    dplyr::rename("related_tag_id" = "tag_id") %>%
    dplyr::select(-"related_tag_name") %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::select(-"tag_name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags_to_tags"
    )


  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "syn52069327",
    "tags_to_tags"
  )

}
