tags_to_tags_prince <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  prince_tags <-
    synapse_csv_id_to_tbl(syn, "syn63389543") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  prince_labels <- synapse_csv_id_to_tbl(syn, "syn52366476") %>%
    dplyr::mutate(
      "name" = dplyr::if_else(
        short_display == "PDAC",
        "PDAC",
        name
      )
    )

  tags <- synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici specific tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176") #add tags from tcga
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn53698018") #msk tags
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
      synapse_csv_id_to_tbl(syn, "syn63389543") #PRINCE specific - REPLACE
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  tags_to_tags <-
    prince_tags %>%
    dplyr::inner_join(prince_labels, by = c("tag_name" = "name")) %>%
    dplyr::inner_join(tags, by = c("parent_tag" = "tag_name")) %>%
    dplyr::select("related_tag_id" = "tag_id.y",
                  "tag_id" = "tag_id.x") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "syn63327065",
    "tags_to_tags"
  )

}
