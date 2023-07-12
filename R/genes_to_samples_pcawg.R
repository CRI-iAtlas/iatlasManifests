genes_to_samples_pcawg <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  gene_sets <-
    synapse_csv_id_to_tbl(syn, "syn51034026") %>%
    dplyr::filter(.data$name %in% c("immunomodulator", "io_target")) %>%
    dplyr::pull("id")

  gene_sets_genes <-
    synapse_csv_id_to_tbl(syn, "syn51400859") %>%
    dplyr::filter(.data$gene_set_id %in% gene_sets) %>%
    dplyr::pull("gene_id") %>%
    unique()

  genes <-
    synapse_csv_id_to_tbl(syn, "syn50896922") %>%
    dplyr::filter(.data$id %in% gene_sets_genes) %>%
    dplyr::select(
      "hgnc" = "hgnc_id",
      "gene_id" = "id"
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn51088163") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  genes_to_samples <-
    synapse_tsv_id_to_tbl(syn,  "syn21785590") %>%
    dplyr::rename("hgnc" = "gene")  %>%
    dplyr::inner_join(genes, by = "hgnc") %>%
    dplyr::select(-"hgnc") %>%
    tidyr::pivot_longer(
      -"gene_id", names_to = "sample_name", values_to = "rna_seq_expression"
    ) %>%
    dplyr::inner_join(samples, by = "sample_name") %>%
    dplyr::select(-"sample_name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "genes_to_samples"
    )

  synapse_store_table_as_csv(
    syn,
    genes_to_samples,
    "syn51096172",
    "genes_to_samples"
  )

}
