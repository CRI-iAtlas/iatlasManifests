datasets_to_tags_htan <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  parent_tags <-
    synapse_csv_id_to_tbl(syn, "syn52570142") %>% #table with consolidated htan annotation that was created with prince_tags_function.R
    colnames()

  tags_from_tcga <- c("Immune_Subtype", "TCGA_Study")

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici tags
    dplyr::filter(name %in% parent_tags) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176") %>% #add tags from TCGA
        dplyr::filter(name %in% tags_from_tcga)
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  datasets <-
    dataset_htan() %>% #REPLACE
    dplyr::select(
      "dataset_name" = "name",
      "dataset_id" = "id"
    )

  datasets_to_tags <-
    tidyr::crossing(tags, datasets) %>%
    dplyr::select(-c("tag_name", "dataset_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets_to_tags"
    )

  # synapse_store_table_as_csv(
  #   syn,
  #   datasets_to_tags,
  #   "",
  #   "datasets_to_tags"
  # )

}

