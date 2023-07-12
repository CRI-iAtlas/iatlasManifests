cohorts_to_samples_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cohorts <-
    synapse_csv_id_to_tbl(syn, "syn51088091") %>%
    dplyr::select(
      "cohort_id" = "id",
      "tag_id" = "cohort_tag_id",
      "dataset_id"
    )

  samples_to_tags <-
    synapse_csv_id_to_tbl(syn, "syn51080206") %>%
    dplyr::select(
      "tag_id",
      "sample_id"
    )

  datasets_to_samples <-
    synapse_csv_id_to_tbl(syn, "syn51537011") %>%
    dplyr::select(
      "dataset_id",
      "sample_id"
    )

  cohorts_to_samples1 <-
    dplyr::inner_join(
      cohorts,
      samples_to_tags,
      by = "tag_id"
    ) %>%
    dplyr::select("cohort_id", "sample_id")

  cohorts_to_samples2 <- cohorts %>%
    dplyr::filter(is.na(.data$tag_id)) %>%
    dplyr::inner_join(
      datasets_to_samples,
      by = "dataset_id"
    ) %>%
    dplyr::select("cohort_id", "sample_id")

  cohorts_to_samples <-
    dplyr::bind_rows(cohorts_to_samples1, cohorts_to_samples2) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cohorts_to_samples"
    )

  synapse_store_table_as_csv(
    syn,
    cohorts_to_samples,
    "syn51537397",
    "cohorts_to_samples"
  )

}

cohorts_to_samples_tcga2 <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cohorts <-
    synapse_csv_id_to_tbl(syn, "syn51088091") %>%
    dplyr::rename(
      "cohort_id" = "id",
      "cohort_name" = "name",
      "tag_id" = "cohort_tag_id"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51080176") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id",
      "tag_type"
    )

  tags_to_tags <-
    synapse_csv_id_to_tbl(syn, "syn51080196") %>%
    dplyr::select("tag_id", "related_tag_id")

  samples_to_datasets <-
    synapse_csv_id_to_tbl(syn, "syn51537011") %>%
    dplyr::select("sample_id", "dataset_id")


  group_tags <- tags %>%
    dplyr::left_join(tags_to_tags, by = c("tag_id")) %>%
    dplyr::left_join(tags, by = c("related_tag_id" = "tag_id")) %>%
    dplyr::filter(.data$tag_type.x == "group", .data$tag_type.y == "parent_group") %>%
    dplyr::select(
      "tag_id",
      "name" = "tag_name.x",
      "parent_tag_id" = "related_tag_id",
      "parent_name" = "tag_name.y"
    )

  samples_to_tags <-
    synapse_csv_id_to_tbl(syn, "syn51080206") %>%
    dplyr::select("sample_id", "tag_id") %>%
    dplyr::inner_join(
      group_tags,
      by = c("tag_id")
    )

  tag_cohorts_to_samples <- samples_to_tags %>%
    dplyr::inner_join(samples_to_datasets) %>%
    dplyr::inner_join(cohorts, by = c("parent_tag_id" = "tag_id", "dataset_id")) %>%
    dplyr::select("cohort_id", "sample_id", "tag_id")

  dataset_cohorts_to_samples <- cohorts %>%
    dplyr::filter(
      is.na(.data$tag_id)
    ) %>%
    dplyr::inner_join(samples_to_datasets, by = "dataset_id") %>%
    dplyr::select("cohort_id", "sample_id")

  cohorts_to_samples <- dplyr::bind_rows(
    tag_cohorts_to_samples,
    dataset_cohorts_to_samples
  )



}

