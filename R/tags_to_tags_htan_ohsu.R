tags_to_tags_htan_ohsu <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  htan_tags <- synapse_csv_id_to_tbl(syn, "syn52570142")

  #load the current ici tags
  db_tags <- iatlasGraphQLClient::query_tags_with_parent_tags()

  htan_categories <- htan_tags %>%
    tidyr::pivot_longer(-c("HTAN.Biospecimen.ID", "HTAN.Parent.ID"), names_to = "parent_group", values_to = "tag") %>%
    dplyr::select(parent_group, tag) %>%
    dplyr::distinct() %>%
    dplyr::filter(!tag %in% db_tags$tag_name)

  tags <-
    synapse_csv_id_to_tbl(syn, "") %>% #UPDATE WITH SYN ID OF TAGS HTAN OHSU
    dplyr::add_row(synapse_csv_id_to_tbl(syn, "syn51613683")) %>%  #ici specific tags
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  tags_to_tags <-
    htan_categories %>%
    dplyr::inner_join(tags, by = c("parent_group" = "tag_name")) %>%
    dplyr::rename("related_tag_id" = "tag_id") %>%
    dplyr::inner_join(tags, by = c("tag" = "tag_name")) %>%
    dplyr::select("related_tag_id", "tag_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags_to_tags"
    )


  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "", #UPDATE
    "tags_to_tags"
  )

}
