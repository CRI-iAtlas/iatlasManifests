tags_to_tags_amadeus <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


  #getting the new group tags from Porter
  tags <-
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
  tags_amadeus <- synapse_csv_id_to_tbl(syn, "syn64423867")
  tags_amadeus <- tags_amadeus %>%
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
    tags_amadeus %>%
    dplyr::inner_join(tags, by = dplyr::join_by("parent_tag" == "tag_name")) %>%
    dplyr::select("related_tag_id" , "tag_id" = "id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "syn64156736",
    "tags_to_tags"
  )

}
