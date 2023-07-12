genes_to_samples_tcga <- function() {

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
      "entrez" = "entrez_id",
      "gene_id" = "id"
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn50896891") %>%
    dplyr::select(
      "sample" = "name",
      "sample_id" = "id"
    )

  genes_to_samples <-
    synapse_feather_id_to_tbl(syn, "syn23560243") %>%
    dplyr::select("entrez", "sample", "rna_seq_expression" = "rna_seq_expr") %>%
    tidyr::drop_na() %>%
    dplyr::inner_join(genes, by = "entrez") %>%
    dplyr::inner_join(samples, by = "sample") %>%
    dplyr::select(-c("entrez", "sample")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "genes_to_samples"
    )

  genes_to_samples1 <- genes_to_samples %>%
    dplyr::slice(1:1000000)

  genes_to_samples2 <- genes_to_samples %>%
    dplyr::slice(1000001:2000000)

  genes_to_samples3 <- genes_to_samples %>%
    dplyr::slice(2000001:3000000)

  genes_to_samples4 <- genes_to_samples %>%
    dplyr::slice(3000001:4000000)

  genes_to_samples5 <- genes_to_samples %>%
    dplyr::slice(4000001:dplyr::n())

  synapse_store_table_as_csv(
    syn,
    genes_to_samples1,
    "syn51747473",
    "genes_to_samples"
  )

  synapse_store_table_as_csv(
    syn,
    genes_to_samples2,
    "syn51747474",
    "genes_to_samples"
  )

  synapse_store_table_as_csv(
    syn,
    genes_to_samples3,
    "syn51750773",
    "genes_to_samples"
  )

  synapse_store_table_as_csv(
    syn,
    genes_to_samples4,
    "syn51750774",
    "genes_to_samples"
  )
  synapse_store_table_as_csv(
    syn,
    genes_to_samples5,
    "syn51750775",
    "genes_to_samples"
  )

}
