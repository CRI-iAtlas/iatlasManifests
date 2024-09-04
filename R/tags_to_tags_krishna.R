tags_to_tags_shiao <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  tags_from_tcga <- c("Immune_Subtype", "TCGA_Study", "TCGA_Subtype")

  parent_tags <-
    synapse_csv_id_to_tbl(syn, "syn53698018") %>% #msk tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn53697423") #vanderbilt tags
    )%>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn58896103") #shiao tags
    )%>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51613683") #ici tags
    ) %>%
    dplyr::filter(tag_type == "parent_group") %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176") %>% #add tags from TCGA
        dplyr::filter(name %in% tags_from_tcga)
    ) %>%
    dplyr::select(
      "parent_tag" = "name",
      "related_tag_id" = "id"
    )

  #getting the new group tags from Krishna
  tags <-
    synapse_csv_id_to_tbl(syn, "syn59210643") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )


  # We will add manually the information on parent tags for the new groups
  tags$parent_tag <- c("NeoICI_Rx", "Prior_ICI_Rx", "Prior_ICI_Rx", "TCGA_Study", "TCGA_Subtype")

  tags_to_tags <-
    tags %>%
    dplyr::inner_join(parent_tags, by = "parent_tag") %>%
    dplyr::select("related_tag_id", "tag_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags_to_tags"
    )


  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "syn59213644",
    "tags_to_tags"
  )
}
