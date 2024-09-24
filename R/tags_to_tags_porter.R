tags_to_tags_porter <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  # tags_from_tcga <- c("Immune_Subtype", "TCGA_Study", "TCGA_Subtype")
  #
  # parent_tags <-
  #   synapse_csv_id_to_tbl(syn, "syn53698018") %>% #msk tags
  #   dplyr::filter(name %in% parent_tags_htan) %>%
  #   dplyr::add_row(
  #     synapse_csv_id_to_tbl(syn, "syn53697423") %>% #vanderbilt tags
  #       dplyr::filter(name %in% parent_tags_htan)
  #   )%>%
  #   dplyr::add_row(
  #     synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici tags
  #       dplyr::filter(name %in% parent_tags_htan)
  #   ) %>%
  #   dplyr::add_row(
  #     synapse_csv_id_to_tbl(syn, "syn51080176") %>% #add tags from TCGA
  #       dplyr::filter(name %in% tags_from_tcga)
  #   ) %>%
  #   dplyr::select(
  #     "parent_tag" = "name",
  #     "related_tag_id" = "id"
  #   )

  #getting the new group tags from Porter
  tags <-
    synapse_csv_id_to_tbl(syn, "") %>% #update with file id for Tags Porter
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  # We will add manually the information on parent tags for the new groups
  tags$parent_tag <- c("TCGA_Study", "TCGA_Subtype", replicate(3, "Non_ICI_Rx"), "Cancer_Tissue")

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
    "",
    "tags_to_tags"
  )

}
