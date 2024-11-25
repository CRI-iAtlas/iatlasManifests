datasets_to_tags_shiao <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #we are using the same tags used in HTAN MSK dataset, so we will use the same initial structure
  parent_tags <-
    synapse_csv_id_to_tbl(syn, "syn53605383") %>% #table with consolidated annotation that was created with htan_create_wide_table_vanderbilt.R
    colnames()

  tags_from_tcga <- c("Immune_Subtype", "TCGA_Study")

  tags <-
    synapse_csv_id_to_tbl(syn, "syn53698018") %>% #msk tags
    dplyr::filter(name %in% parent_tags) %>%
    dplyr::add_row(
    synapse_csv_id_to_tbl(syn, "syn53697423") %>% #vanderbilt tags
    dplyr::filter(name %in% parent_tags)
    )%>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici tags
        dplyr::filter(name %in% parent_tags)
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176") %>% #add tags from TCGA
        dplyr::filter(name %in% tags_from_tcga)
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  datasets <-
    synapse_csv_id_to_tbl(syn, "syn58399200") %>%
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

  synapse_store_table_as_csv(
    syn,
    datasets_to_tags,
    "syn58912907",
    "datasets_to_tags"
  )

}

