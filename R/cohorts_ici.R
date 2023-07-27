cohorts_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  datasets <-
    synapse_csv_id_to_tbl(syn, "syn52060439") %>%
    dplyr::bind_rows(
      synapse_csv_id_to_tbl(syn, "syn52067760")
    ) %>%
    dplyr::select(
      "dataset_name" = "name",
      "dataset_id" = "id"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51613683") %>%
    dplyr::filter(tag_type == "parent_group") %>%
    dplyr::select(
      "tag_name" = "name",
      "cohort_tag_id" = "id"
    )

  cohorts <- tidyr::crossing(
    dataset_name = datasets$dataset_name,
    tag_name = tags$tag_name
  ) %>%
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
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cohorts"
    )

  synapse_store_table_as_csv(
    syn,
    cohorts,
    "syn52161605",
    "cohorts"
  )

}

