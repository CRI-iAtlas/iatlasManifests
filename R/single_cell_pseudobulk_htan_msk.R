single_cell_pseudobulk_htan_msk <- function(){
  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()
  pseudobulk <- synapse_tsv_id_to_tbl(syn, "syn53708239")

  #getting ids
  tcga_genes <- synapse_csv_id_to_tbl(syn, "syn50896922") %>%
    dplyr::select(
      "feature_name" = "hgnc_id",
      "gene_id" = "id"
    )

  samples_ids <- synapse_csv_id_to_tbl(syn, "syn53678348")%>%
    dplyr::select(
      "sample_name"  = "name",
      "sample_id" = "id"
    )

  single_cell_pseudobulk <- pseudobulk %>%
    dplyr::inner_join(samples_ids, by = "sample_name") %>%
    dplyr::inner_join(tcga_genes, by = "feature_name") %>%
    dplyr::select(
      "sample_id",
      "cell_type",
      "gene_id",
      "single_cell_seq_sum" = feature_value
    ) %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "single_cell_pseudobulk"
    )

  synapse_store_table_as_csv(
    syn,
    single_cell_pseudobulk,
    "syn53708288",
    "single_cell_pseudobulk"
  )


  }
