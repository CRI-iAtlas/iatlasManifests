tags_to_tags_shiao <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #getting parent tags ids
  parent_tags_htan <-
    synapse_csv_id_to_tbl(syn, "syn53605383") %>% #table with consolidated annotation that was created with htan_create_wide_table_vanderbilt.R
    colnames()

  tags_from_tcga <- c("Immune_Subtype", "TCGA_Study", "TCGA_Subtype", "BRCA_subtypes")

  parent_tags <-
    synapse_csv_id_to_tbl(syn, "syn53698018") %>% #msk tags
    dplyr::filter(name %in% parent_tags_htan) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn53697423") %>% #vanderbilt tags
        dplyr::filter(name %in% parent_tags_htan)
    )%>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici tags
        dplyr::filter(name %in% parent_tags_htan)
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176") %>% #add tags from TCGA
        dplyr::filter(name %in% tags_from_tcga)
    ) %>%
    dplyr::select(
      "parent_tag" = "name",
      "related_tag_id" = "id"
    )

  #getting the new group tags from Shiao
  tags <-
    synapse_csv_id_to_tbl(syn, "syn58896103") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  #Tumor subtype can have two parent tags, so we will replicate this row
  tags <- rbind(tags, tags[rep(4, 1), ])

  # We will add manually the information on parent tags for the new groups
  tags$parent_tag <- c("Cancer_Tissue", "Biopsy_Site", "Clinical_Stage", "TCGA_Subtype", replicate(8, "Subsq_Rx"), "BRCA_subtypes")

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
    "syn58913350",
    "tags_to_tags"
  )

}
