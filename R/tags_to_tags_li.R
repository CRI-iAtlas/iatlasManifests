tags_to_tags_li <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  tags <-
    synapse_csv_id_to_tbl(syn, "syn53698018") %>% #msk tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn60157438") #li tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn58896103") #shiao tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn59210643") #krishna tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn53697423") #vanderbilt tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51613683") #ici tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176")  #add tags from TCGA
    ) %>%
    dplyr::select(
      "name",
      "related_tag_id" = "id"
    )


  #getting the new group tags from Li
  tags_li <-
    synapse_csv_id_to_tbl(syn, "syn60157438") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  # We will add manually the information on parent tags for the new groups
  tags_li$parent_tag <- c("Clinical_Stage", "Tumor_tissue_type", "Biopsy_Site")

  tags_to_tags <-
    tags_li %>%
    dplyr::inner_join(tags, by = dplyr::join_by("parent_tag" == "name")) %>%
    dplyr::select("related_tag_id", "tag_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "syn60157510",
    "tags_to_tags"
  )

}
