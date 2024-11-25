tags_to_tags_bi <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  tags <-
    synapse_csv_id_to_tbl(syn, "syn53698018") %>% #msk tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn60548945") #bi tags
    ) %>%
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
  tags_bi <-
    synapse_csv_id_to_tbl(syn, "syn60548945") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  # We will add manually the information on parent tags for the new groups
  tags_bi$parent_tag <- c("Non_ICI_Rx", "ICI_Rx")

  tags_to_tags <-
    tags_bi %>%
    dplyr::inner_join(tags, by = dplyr::join_by("parent_tag" == "name")) %>%
    dplyr::select("related_tag_id", "tag_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "syn60530048",
    "tags_to_tags"
  )

}
