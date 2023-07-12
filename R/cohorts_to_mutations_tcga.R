cohorts_to_mutations_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cohorts_to_samples <-
    synapse_csv_id_to_tbl(syn, "syn51537400") %>%
    dplyr::select("cohort_id", "sample_id")

  mutations_to_samples <-
    synapse_csv_id_to_tbl(syn, "syn51440902") %>%
    dplyr::select("mutation_id", "sample_id")

  cohorts_to_mutations <-
    dplyr::inner_join(
      cohorts_to_samples,
      mutations_to_samples,
      by = "sample_id"
    ) %>%
    dplyr::select("cohort_id", "mutation_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cohorts_to_mutations"
    )

  synapse_store_table_as_csv(
    syn,
    cohorts_to_mutations,
    "syn51537498",
    "cohorts_to_mutations"
  )

}

