tags_to_tags_TEMPLATE <- function() { #UPDATE function name

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
      synapse_csv_id_to_tbl(syn, "syn64423867") #add tags from AMADEUS
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "related_tag_id" = "id"
    )

  # We will add the information on parent tags for the new groups
  new_tags <- synapse_csv_id_to_tbl(syn, "") #update with table of tags, from tags_TEMPLATE
  new_tags <- new_tags %>% #make changes as needed to add the group-parent_group relationship
    dplyr::mutate(
      "parent_tag" = dplyr::case_when(
        name == "NSCLC" ~ "TCGA_Study",
        stringr::str_ends(name, "cancer_tissue") ~ "Cancer_Tissue",
        stringr::str_ends(name, "_amadeus") ~ "AMADEUS_Study",
        stringr::str_ends(name, "_prior_ici_rx") ~ "Prior_ICI_Rx",
        stringr::str_ends(name, "_prior_rx") ~ "Prior_Rx"
      )
    )

  tags_to_tags <-
    new_tags %>%
    dplyr::inner_join(tags, by = dplyr::join_by("parent_tag" == "tag_name")) %>%
    dplyr::select("related_tag_id" , "tag_id" = "id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "", #update
    "tags_to_tags"
  )

}
