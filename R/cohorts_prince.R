cohorts_prince <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  parent_tags <-
    synapse_csv_id_to_tbl(syn, "syn52349216") %>% #table with consolidated prince annotation that was created with prince_tags_function.R
    dplyr::select(
      -c("Run_ID", "ph", "sample_key", "sample.id", "subject.id", "timepoint.relative.order")
    ) %>%
    colnames()

  tags_from_tcga <- c("Immune_Subtype", "TCGA_Study")

  tags_from_patients <- c("ethnicity", "gender", "race")

  parent_groups <- c(parent_tags, tags_from_tcga, tags_from_patients)

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici specific tags
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
      synapse_csv_id_to_tbl(syn, "syn63389543") #PRINCE specific
    ) %>%
    dplyr::filter(
      name %in% parent_groups
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "cohort_tag_id" = "id"
    )

  datasets <-
    synapse_csv_id_to_tbl(syn, "syn63329595") %>%
    dplyr::select(
      "dataset_name" = "name",
      "dataset_id" = "id"
    )

  cohorts <-
    tidyr::crossing(
      tag_name = tags$tag_name,
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
    dplyr::left_join(tags, by = "tag_name") %>%
    dplyr::inner_join(datasets, by = "dataset_name") %>%
    dplyr::select(-c("tag_name", "dataset_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    cohorts,
    "syn63327069",
    "cohorts"
  )

}

