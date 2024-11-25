genes_to_samples_prince <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


    genes <-
      synapse_csv_id_to_tbl(syn, "syn50896922") %>%
      dplyr::select(
        "entrez" = "entrez_id",
        "gene_id" = "id"
      )

    samples <-
      synapse_csv_id_to_tbl(syn, "syn63332022") %>%
      dplyr::select(
        "sample" = "name",
        "sample_id" = "id"
      )

    rna_seq <-
      synapse_csv_id_to_tbl(syn, "syn63194036")


    genes_to_samples <-
      rna_seq %>%
      dplyr::inner_join(samples, by = dplyr::join_by("Run_ID" == "sample")) %>%
      dplyr::inner_join(genes, by = "entrez") %>%
      dplyr::select(
        "rna_seq_expression" = "rna_seq_expr",
        "gene_id",
        "sample_id"
      ) %>%
      dplyr::mutate(
        "id" = uuid::UUIDgenerate(n = dplyr::n()),
      )

  synapse_store_table_as_csv(
    syn,
    genes_to_samples,
    "syn63327062",
    "genes_to_samples"
  )

}
