tags_to_tags_damrauer_rose_zappasodi <- function() {

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
      synapse_csv_id_to_tbl(syn, "syn68143413") #add tags from datasets
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "related_tag_id" = "id"
    )

  # We have the parent_tags info from the original tables
  new_tags<- synapse_csv_id_to_tbl(syn, "syn65986771") %>%
    dplyr::bind_rows(synapse_csv_id_to_tbl(syn, "syn65986579")) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(tags, by = dplyr::join_by("tag_name")) %>%
    dplyr::select("tag_id" = "related_tag_id",
                  "parent_tag")

  tags_to_tags <-
    new_tags %>%
    dplyr::inner_join(tags, by = dplyr::join_by("parent_tag" == "tag_name")) %>%
    dplyr::select("related_tag_id" , "tag_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "syn66227467",
    "tags_to_tags"
  )

}
