cohorts_to_tags_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cohorts_to_samples <-
    synapse_csv_id_to_tbl(syn, "syn51537400") %>%
    dplyr::select("cohort_id", "sample_id")

  tags_to_samples <-
    synapse_csv_id_to_tbl(syn, "syn51080206") %>%
    dplyr::select("tag_id", "sample_id")

  cohorts_to_tags <-
    dplyr::inner_join(
      cohorts_to_samples,
      tags_to_samples,
      by = "sample_id"
    ) %>%
    dplyr::select("cohort_id", "tag_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cohorts_to_tags"
    )

  synapse_store_table_as_csv(
    syn,
    cohorts_to_tags,
    "syn51538650",
    "cohorts_to_tags"
  )

}

