cohorts_to_genes_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cohorts_to_samples <-
    synapse_csv_id_to_tbl(syn, "syn51537400") %>%
    dplyr::select("cohort_id", "sample_id")

  genes_to_samples <-
    synapse_csv_id_to_tbl(syn, "syn51471734") %>%
    dplyr::select("gene_id", "sample_id")

  cohorts_to_genes <-
    dplyr::inner_join(
      cohorts_to_samples,
      genes_to_samples,
      by = "sample_id"
    ) %>%
    dplyr::select("cohort_id", "gene_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cohorts_to_genes"
    )

  synapse_store_table_as_csv(
    syn,
    cohorts_to_genes,
    "syn51537476",
    "cohorts_to_genes"
  )

}

