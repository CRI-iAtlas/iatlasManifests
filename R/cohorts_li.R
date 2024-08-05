cohorts_li <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  parent_tags_htan <-
    synapse_csv_id_to_tbl(syn, "syn53605383") %>% #table with consolidated annotation that was created with htan_create_wide_table_vanderbilt.R
    colnames()

  tags_from_tcga <- c("TCGA_Study", "TCGA_Subtype")

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
      "tag_name" = "name",
      "tag_id" = "id"
    )



  datasets <-
    synapse_csv_id_to_tbl(syn, "syn60085138") %>%
    dplyr::select(
      "dataset_name" = "name",
      "dataset_id" = "id"
    )

  cohorts <-
    tidyr::crossing(
      tag_name = parent_tags$tag_name,
      dataset_name = datasets$dataset_name) %>%
    dplyr::mutate(
      name = paste(dataset_name, tag_name, sep = "_")
    ) %>%
    dplyr::bind_rows(
      data.frame(
        name = datasets$dataset_name,
        dataset_name = datasets$dataset_name,
        tag_name = NA
      )
    ) %>%
    dplyr::left_join(parent_tags, by = "tag_name") %>%
    dplyr::inner_join(datasets, by = "dataset_name") %>%
    dplyr::select(-c("tag_name", "dataset_name"),
                  cohort_tag_id = tag_id) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cohorts"
    )

  synapse_store_table_as_csv(
    syn,
    cohorts,
    "syn60157637",
    "cohorts"
  )

}
