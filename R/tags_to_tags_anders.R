tags_to_tags_anders <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  # The tags_to_tags table has the following columns:
  # "related_tag_id"= id of the parent tag associated with a tag
  # "tag_id"= tag id for a group
  # "id" = id for the relationship


  #getting the new group tags
  tags <- #keep this code and add any new tags table that is necessary
    synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici specific tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176") #add tags from tcga
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn65913705") #add tags from Anders
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "related_tag_id" = "id"
    )

  # We will add the information on parent tags for the new groups
  new_tags <- synapse_csv_id_to_tbl(syn, "syn65887902")

  tags_to_tags <-
    new_tags %>%
    dplyr::inner_join(tags, by = dplyr::join_by("tag_name")) %>%
    dplyr::rename("tag_id" = "related_tag_id")  %>%
    dplyr::inner_join(tags, by = dplyr::join_by("parent_tag" == "tag_name")) %>%
    dplyr::select("related_tag_id" , "tag_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "syn65888287",
    "tags_to_tags"
  )

}
