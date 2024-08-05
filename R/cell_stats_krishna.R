cell_stats_krishna <- function(){
  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  summary_counts <- synapse_tsv_id_to_tbl(syn, "syn59473664")

  #getting ids
  tcga_genes <- synapse_csv_id_to_tbl(syn, "syn50896922")

  dataset_id <- synapse_csv_id_to_tbl(syn, "syn59195251") %>%
    dplyr::pull(id)

  cell_stats <- summary_counts %>%
    dplyr::inner_join(tcga_genes, by = dplyr::join_by(gene == hgnc_id)) %>%
    dplyr::select(
      "gene_id" = "id",
      "cell_type" = "cell",
      "cell_count" = "counts",
      "avg_expr" = "avg",
      "perc_expr"
    ) %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cell_stats"
    )

  synapse_store_table_as_csv(
    syn,
    cell_stats,
    "syn59473668",
    "cell_stats"
  )


}
