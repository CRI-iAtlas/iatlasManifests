genes_to_samples_damrauer_rose_zappasodi <- function() {

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
      "entrez_id",
      "gene_id" = "id"
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn66227595") %>%
    dplyr::select(
      "sample" = "name",
      "sample_id" = "id"
    )

  hgnc_to_entrez_df <- synapse_csv_id_to_tbl(syn, "syn50896922")

  rna_seq <-
    synapse_tsv_id_to_tbl(syn, "syn66227436") %>%
    tidyr::pivot_longer(-"Gene", names_to = "sample", values_to = "rna_seq_expr") %>%
    dplyr::inner_join(hgnc_to_entrez_df, by = dplyr::join_by(Gene == hgnc_id), relationship = "many-to-many")



  genes_to_samples <-
    rna_seq %>%
    dplyr::inner_join(samples, by = "sample") %>%
    dplyr::inner_join(genes, by = "entrez_id") %>%
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
    "syn66227464",
    "genes_to_samples"
  )

}
