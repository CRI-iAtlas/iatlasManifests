genes_to_samples_TEMPLATE <- function() {#UPDATE function name

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  # columns in the genes_to_samples table:
  # name = sample name
  # gene_id = id for a gene
  # sample_id = id for the associated sample
  # id = id created in this script for each relationship

  genes <-
    synapse_csv_id_to_tbl(syn, "syn50896922") %>% #no need to update this
    dplyr::select(
      "entrez" = "entrez_id",
      "gene_id" = "id"
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "") %>% #update with synapse id for the samples table
    dplyr::select(
      "sample" = "name",
      "sample_id" = "id"
    )

  rna_seq <-
    synapse_csv_id_to_tbl(syn, "") %>% #update with synapse id for the gene expression table
    dplyr::rename(
      "sample" = Run_ID
    )


  genes_to_samples <-
    rna_seq %>%
    dplyr::inner_join(samples, by = "sample") %>%
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
    "", #UPDATE
    "genes_to_samples"
  )

}
