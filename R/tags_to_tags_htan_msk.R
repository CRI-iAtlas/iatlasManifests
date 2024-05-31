tags_to_tags_htan_msk <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #getting parent tags ids
  parent_tags_htan <-
    synapse_csv_id_to_tbl(syn, "syn53605383") %>% #table with consolidated annotation that was created with htan_create_wide_table_vanderbilt.R
    colnames()

  tags_from_tcga <- c("Immune_Subtype", "TCGA_Study", "TCGA_Subtype")

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

  #getting the new group tags from HTAN MSK
  tags <-
    synapse_csv_id_to_tbl(syn, "syn53698018") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )


  tags_to_tags <- synapse_csv_id_to_tbl(syn, "syn53605384") %>%
    dplyr::add_row(
      parent_tag = "TCGA_Study",
      short_display = "SCLC",
      name = "SCLC"
    ) %>%
    dplyr::inner_join(tags, by = dplyr::join_by("name" == "tag_name")) %>%
    dplyr::inner_join(parent_tags, by = dplyr::join_by("parent_tag")) %>%
    dplyr::select("related_tag_id", "tag_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags_to_tags"
    )

  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "syn60229635",
    "tags_to_tags"
  )

}
