single_cell_pseudobulk_krishna <- function(){
  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  dataset_id <- synapse_csv_id_to_tbl(syn, "syn59195251") %>%
    dplyr::pull(id)

  pseudobulk <- synapse_csv_id_to_tbl(syn, "syn59473424") %>%
    tidyr::separate_wider_regex("...1", c(sample_name = ".*", "_", cell_type = ".*")) %>%
    dplyr::mutate(
      "sample_name" = paste0("Krishna_ccRCC_", sample_name)
    ) %>%
    tidyr::pivot_longer(-c("sample_name", "cell_type"), names_to = "feature_name", values_to =  "single_cell_seq_sum")%>%
    dplyr::filter(!cell_type %in% c("Ambiguous", "Ambiguous/Dead", "TAM/TCR (Ambiguos)"))

  #getting ids
  tcga_genes <- synapse_csv_id_to_tbl(syn, "syn50896922") %>%
    dplyr::select(
      "feature_name" = "hgnc_id",
      "gene_id" = "id"
    )

  samples_ids <- synapse_csv_id_to_tbl(syn, "syn59204288")%>%
    dplyr::select(
      "sample_name"  = "name",
      "sample_id" = "id"
    )

  single_cell_pseudobulk <- pseudobulk %>%
    dplyr::inner_join(samples_ids, by = "sample_name") %>%
    dplyr::inner_join(tcga_genes, by = "feature_name", relationship = "many-to-many") %>%
    dplyr::select(
      "sample_id",
      "cell_type",
      "gene_id",
      "single_cell_seq_sum"
    ) %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "single_cell_pseudobulk"
    )

  synapse_store_table_as_csv(
    syn,
    single_cell_pseudobulk[1:(nrow(single_cell_pseudobulk)/2),],
    "syn59473513",
    "single_cell_pseudobulk"
  )
  synapse_store_table_as_csv(
    syn,
    single_cell_pseudobulk[(1+nrow(single_cell_pseudobulk)/2):(nrow(single_cell_pseudobulk)),],
    "syn61483806",
    "single_cell_pseudobulk"
  )
}
