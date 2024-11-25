tags_to_tags_porter <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


  #getting the new group tags from Porter
  tags <-
    synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici specific tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176") #add tags from tcga
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "related_tag_id" = "id"
    )

  # We will add manually the information on parent tags for the new groups
  tags_porter <- synapse_csv_id_to_tbl(syn, "syn63623105") # Tags Porter
  tags_porter$parent_tag <- c("TCGA_Study", "TCGA_Subtype", replicate(3, "Non_ICI_Rx"), "Cancer_Tissue")

  tags_to_tags <-
    tags_porter %>%
    dplyr::inner_join(tags, by = dplyr::join_by("parent_tag" == "tag_name")) %>%
    dplyr::select("related_tag_id" , "tag_id" = "id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "syn63623057",
    "tags_to_tags"
  )

}
